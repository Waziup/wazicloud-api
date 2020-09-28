{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module MQTT where

import           Prelude hiding (id)
import           Data.Aeson as JSON
import           Data.String.Conversions
import qualified Data.List as L
import           Data.Maybe
import           Data.Time
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit.Network (runTCPServer, runTCPClient, serverSettings, clientSettings, appSource, appSink, AppData)
import           Data.Conduit.Attoparsec (conduitParser)
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Lens
import           Control.Concurrent.Async (concurrently)
import           System.Log.Logger
import           Waziup.Types
import           Waziup.Utils
import           Waziup.Users hiding (info, warn, debug, err)
import           Waziup.Auth hiding (info, warn, debug, err)
import           Waziup.Devices hiding (info, warn, debug, err)
import           Orion as O hiding (info, warn, debug, err)
import           Network.MQTT.Client hiding (MQTTConfig)
import qualified Network.MQTT.Types as T
import           Conduit
import           Database.MongoDB as DB hiding (Username, Password, host)
import           Safe

-- | MQTT topics
dev_topic, sen_topic, act_topic, val_topic :: T.Text
dev_topic = "devices"
sen_topic = "sensors"
act_topic = "actuators"
val_topic = "value"

-- | Cache for the connection
data ConnCache = ConnCache {
  connUser :: User,
  connId   :: LB.ByteString}

-- | data structure to decode MQTT packets
data MQTTData = MQTTSen DeviceId SensorId SensorValue | 
                MQTTAct DeviceId ActuatorId JSON.Value |
                MQTTError String |
                MQTTOther

-- | the MQTT proxy
mqttProxy :: WaziupInfo -> IO ()
mqttProxy wi = do
  let port = _serverPortMQTT $ _serverConf $ _waziupConfig wi
  let settings = serverSettings port "*"
  -- Run a TCP Server to handle incoming requests
  runTCPServer settings (handleExternalStream wi)

-- | Handle external client stream.
handleExternalStream :: WaziupInfo -> AppData -> IO ()
handleExternalStream wi extClient = do
  let (MQTTConfig mosqUrl mosqPort) = _mqttConf $ _waziupConfig wi
  let settings = clientSettings mosqPort (convertString mosqUrl)
  -- connect to the internal MQTT server
  runTCPClient settings (handleStreams wi extClient)

-- connect both up and down streams together
handleStreams :: WaziupInfo -> AppData -> AppData -> IO ()
handleStreams wi extClient intServer = do
  cache <- atomically $ newTVar Nothing 
  debug $ "Starting streams"
  let downstream = appSource extClient .| conduitParser T.parsePacket .| filterMQTTin  cache extClient .| appSink' intServer
  let upstream   = appSource intServer .| conduitParser T.parsePacket .| filterMQTTout cache intServer .| appSink' extClient
  void $ concurrently
    (runWaziup (runConduit downstream) wi) --traffic downstream
    (runWaziup (runConduit upstream) wi)   --traffic upstream
  --write disconnect in DB
  debug $ "Ending streams"
  void $ liftIO $ runWaziup (putConnect cache False) wi

appSink' :: AppData -> Consumer T.MQTTPkt Waziup () 
appSink' app = awaitForever (yield . convertString . T.toByteString) .| appSink app

-- | traffic going downstream (from external client to internal MQTT server)
filterMQTTin :: TVar (Maybe ConnCache) -> AppData -> ConduitT (w, T.MQTTPkt) T.MQTTPkt Waziup ()
filterMQTTin cache extClient = awaitForever $ \(_,res) -> do 
  connCache <- liftIO $ atomically $ readTVar cache
  debug $ "Received downstream: " ++ (show res) ++ " ID= " ++ (show $ connId <$> connCache)
  -- putting the gateway connect
  lift $ putConnect cache True
  case res of
    -- Decode Connect request
    (T.ConnPkt p)    -> connectRequest p cache
    (T.PublishPkt p) -> publishRequest p cache extClient 
    _ -> yield res
    
connectRequest :: T.ConnectRequest -> TVar (Maybe ConnCache) -> ConduitT (w, T.MQTTPkt) T.MQTTPkt Waziup ()
connectRequest p@(T.ConnectRequest user pass _ _ _ cid) cache = do
  -- store user and connection ID
  tok <- if isJust user && isJust pass 
    then do
      t <- lift $ postAuth $ AuthBody (convertString $ fromJust user) (convertString $ fromJust pass)
      return $ Just t
    else return Nothing
  liftIO $ atomically $ writeTVar cache (Just $ ConnCache (getUserFromToken tok) cid) 
  lift $ putConnect cache True
  yield $ T.ConnPkt p

publishRequest :: T.PublishRequest -> TVar (Maybe ConnCache) -> AppData -> ConduitT (w, T.MQTTPkt) T.MQTTPkt Waziup ()
publishRequest p@(T.PublishRequest _ _ _ topic pid body) cache extClient = do
  -- Decode MQTT message
  case decodePub topic body of
    MQTTSen did sid senVal -> do
      -- check authorization
      isAuth <- lift $ isAuthDevice did cache DevicesDataCreate
      if isAuth
        then do
          -- Store the value
          lift $ postSensorValue did sid senVal
          -- pass value to MQTT server
          yield $ T.PublishPkt p
        else do
          -- return PUBACK directly to client (MQTT offers no way to inform the client about the failure)
          yield (T.PubACKPkt (T.PubACK pid)) .| appSink' extClient

    MQTTAct did aid actVal -> do 
      isAuth <- lift $ isAuthDevice did cache DevicesDataCreate 
      debug $ "Perm check: " ++ (show isAuth)
      if isAuth 
        then do
          lift $ putActuatorValue did aid actVal
          yield $ T.PublishPkt p
        else do
          yield (T.PubACKPkt (T.PubACK pid)) .| appSink' extClient
    MQTTError e -> err e
    MQTTOther -> yield (T.PublishPkt p)
      
-- | traffic going upstream (from internal MQTT server to external client)
filterMQTTout :: TVar (Maybe ConnCache) -> AppData -> ConduitT (w, T.MQTTPkt) T.MQTTPkt Waziup ()
filterMQTTout cache intServer = awaitForever $ \(_, res) -> do
  debug $ "Received upstream: " ++ (show res)
  case res of
    -- Decode Publish
    (T.PublishPkt (T.PublishRequest _ _ _ topic id body)) -> do
      -- Decode MQTT message
      case decodePub topic body of
        MQTTSen did _ _ -> do
          -- check authorization
          isAuth <- lift $ isAuthDevice did cache DevicesDataView 
          debug $ "Perm check upstream: " ++ (show isAuth)
          if isAuth 
            then yield res
            else yield (T.PubACKPkt (T.PubACK id)) .| appSink' intServer
        MQTTAct did _ _ -> do 
          isAuth <- lift $ isAuthDevice did cache DevicesDataView 
          debug $ "Perm check upstream: " ++ (show isAuth)
          if isAuth 
            then yield res
            else yield (T.PubACKPkt (T.PubACK id)) .| appSink' intServer
        MQTTError e -> do
          err e
        MQTTOther -> yield res
    _ -> yield res
  
  
decodePub :: LB.ByteString -> LB.ByteString -> MQTTData
decodePub topic body = do
  case T.split (== '/') (convertString topic) of
    [dt, d, st, s, vt] | dt == dev_topic && st == sen_topic && vt == val_topic -> do
      case eitherDecode body of
        Right senVal -> MQTTSen (DeviceId d) (SensorId s) senVal
        Left e -> MQTTError $ "Decode sensor value publish: " ++ (show e) 
    [dt, d, act, a, vt] | dt == dev_topic && act == act_topic && vt == val_topic -> do
      case eitherDecode body of
        Right actVal -> MQTTAct (DeviceId d) (ActuatorId a) actVal
        Left e -> MQTTError $ "Decode actuator value publish: " ++ (show e) 
    _ -> MQTTOther 

isAuthDevice :: DeviceId -> TVar (Maybe ConnCache) -> Scope -> Waziup Bool
isAuthDevice did cache scope = do
  c <- liftIO $ atomically $ readTVar cache
  let (ConnCache user _) = fromJustNote "No cache during Publish" c 
  dev <- getDeviceFromEntity <$> (liftOrion $ O.getEntity (EntityId $ unDeviceId did) devTyp)
  let isAuth = isPermitted' user (PermDevice dev) scope
  debug $ "Perm check: " ++ (show isAuth)
  return isAuth

-- Post sensor value to DBs
postSensorValue :: DeviceId -> SensorId -> SensorValue -> Waziup ()
postSensorValue did sid senVal@(SensorValue v ts dr) = do 
  info $ convertString $ "Post device " <> (unDeviceId did) <> ", sensor " <> (unSensorId sid) <> ", value: " <> (convertString $ show senVal)
  ent <- liftOrion $ O.getEntity (toEntityId did) devTyp
  let device = getDeviceFromEntity ent
  case L.find (\s -> (senId s) == sid) (maybeToList' $ devSensors device) of
      Just sensor -> do
        liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromSensor (sensor {senValue = Just senVal}))
        runMongo (postDatapoint $ Datapoint did sid v ts dr)
        return ()
      Nothing -> do 
        err "sensor not found"

putActuatorValue :: DeviceId -> ActuatorId -> JSON.Value -> Waziup ()
putActuatorValue did aid actVal = do
  info $ convertString $ "Post device " <> (unDeviceId did) <> ", actuator " <> (unActuatorId aid) <> ", value: " <> (convertString $ show actVal)
  ent <- liftOrion $ O.getEntity (toEntityId did) devTyp
  let device = getDeviceFromEntity ent
  case L.find (\a -> (actId a) == aid) (maybeToList' $ devActuators device) of
      Just act -> do
        liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromActuator (act {actValue = Just actVal}))
        return ()
      Nothing -> do 
        err "actuator not found"

putLastSeen :: GatewayId -> Waziup ()
putLastSeen (GatewayId gid) = do
  currentTime <- liftIO $ getCurrentTime
  runMongo $ modify (select ["_id" =: gid] "gateways") [ "$set" =: Doc ["last_seen" =: val currentTime]]
  return ()

putConnect :: TVar (Maybe ConnCache) -> Bool -> Waziup ()
putConnect connCache isConnected = do
  mCon <- liftIO $ atomically $ readTVar connCache
  case mCon of
    (Just (ConnCache _ cid)) -> do
      debug $ "Putting connected=" ++ (show isConnected) ++ " on gateway " ++ (show cid)
      void $ runMongo $ modify (select ["_id" =: (convertString cid :: T.Text)] "gateways") [ "$set" =: Doc ["connected" =: val isConnected]]
    Nothing -> return ()

publishSensorValue :: DeviceId -> SensorId -> SensorValue -> Waziup ()
publishSensorValue (DeviceId d) (SensorId s) v = do
  let topic = T.intercalate "/" [dev_topic, d, sen_topic, s, val_topic]
  (MQTTConfig host port) <- view $ waziupConfig.mqttConf
  info $ "Publish sensor value: " ++ (convertString $ encode v) ++ " to topic: " ++ (show topic)
  liftIO $ do
    mc <- runClient mqttConfig { _hostname = convertString host, _port = port}
    liftIO $ publish mc topic (convertString $ encode v) False

publishActuatorValue :: DeviceId -> ActuatorId -> JSON.Value -> Waziup ()
publishActuatorValue (DeviceId d) (ActuatorId a) v = do
  let topic = T.intercalate "/" [dev_topic, d, act_topic, a, val_topic]
  (MQTTConfig host port) <- view $ waziupConfig.mqttConf
  info $ "Publish actuator value: " ++ (convertString $ encode v) ++ " to topic: " ++ (show topic)
  liftIO $ do
    mc <- runClient mqttConfig { _hostname = convertString host, _port = port}
    liftIO $ publish mc topic (convertString $ encode v) False

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "MQTT" s
info  s = liftIO $ infoM    "MQTT" s
warn  s = liftIO $ warningM "MQTT" s
err   s = liftIO $ errorM   "MQTT" s

