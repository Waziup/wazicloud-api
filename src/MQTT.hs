{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MQTT where

import           Prelude hiding (id)
import           Data.Aeson as JSON
import           Data.String.Conversions
import qualified Data.List as L
import           Data.Maybe
import           Data.Either
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
import           Waziup.Auth hiding (info, warn, debug, err)
import           Waziup.Devices hiding (info, warn, debug, err)
import           Orion as O hiding (info, warn, debug, err)
import           Network.MQTT.Client hiding (MQTTConfig)
import qualified Network.MQTT.Types as T
import           Conduit
import           Keycloak as KC hiding (Scope) 
import           Database.MongoDB as DB hiding (Username, Password, host)
import           Data.Time

-- | Cache for the connection
data ConnCache = ConnCache {
  username :: Maybe Username,
  password :: Maybe Password,
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
  tvConnCache <- atomically $ newTVar Nothing 
  debug $ "Starting streams"
  void $ concurrently
    (runConduit $ appSource extClient .| conduitParser T.parsePacket .| filterMQTTin  wi tvConnCache extClient .| appSink intServer) --traffic downstream
    (runConduit $ appSource intServer .| conduitParser T.parsePacket .| filterMQTTout wi tvConnCache intServer .| appSink extClient) --traffic upstream
  --write disconnect in DB
  debug $ "Ending streams"
  void $ liftIO $ runWaziup (putConnect tvConnCache False) wi

-- | traffic going downstream (from external client to internal MQTT server)
filterMQTTin :: WaziupInfo -> TVar (Maybe ConnCache) -> AppData -> ConduitT (w, T.MQTTPkt) B.ByteString IO ()
filterMQTTin wi tvConnCache extClient = awaitForever $ \(_,res) -> do 
  connCache <- liftIO $ atomically $ readTVar tvConnCache
  debug $ "Received: " ++ (show res) ++ " ID= " ++ (show $ connId <$> connCache)
  -- putting the gateway connect
  void $ lift $ runWaziup (putConnect tvConnCache True) wi
  case res of
    -- Decode Connect request
    (T.ConnPkt (T.ConnectRequest user pass _ _ _ cid)) -> do
      -- store user and connection ID
      liftIO $ atomically $ writeTVar tvConnCache (Just $ ConnCache (convertString <$> user) (convertString <$> pass) cid) 
      void $ lift $ runWaziup (putConnect tvConnCache True) wi
      yield $ convertString $ T.toByteString res
    T.DisconnectPkt -> do
      yield $ convertString $ T.toByteString res
    -- Decode Publish
    (T.PublishPkt (T.PublishRequest _ _ _ topic pid body)) -> do
      -- Decode MQTT message
      case decodePub topic body of
        MQTTSen did sid senVal -> do
          -- check authorization
          perms <- lift $ runWaziup (getPerms' tvConnCache (getPermReq (Just $ PermDeviceId did) [DevicesDataCreate])) wi
          let isAuth = isPermittedResource DevicesDataCreate (PermDeviceId did) (fromRight [] perms)
          debug $ "Perm check: " ++ (show isAuth)
          if isAuth 
            then do
              -- Store the value
              void $ lift $ runWaziup (postSensorValue did sid senVal) wi
              -- pass value to MQTT server
              yield $ convertString $ T.toByteString res
            else do
              -- return PUBACK directly to client (MQTT offers no way to inform the client about the failure)
              yield (convertString $ T.toByteString $ T.PubACKPkt (T.PubACK pid)) .| appSink extClient

        MQTTAct did aid actVal -> do 
          perms <- lift $ runWaziup (getPerms' tvConnCache (getPermReq (Just $ PermDeviceId did) [DevicesDataCreate])) wi
          let isAuth = isPermittedResource DevicesDataCreate (PermDeviceId did) (fromRight [] perms)
          debug $ "Perm check: " ++ (show isAuth)
          if isAuth 
            then do
              void $ lift $ runWaziup (putActuatorValue did aid actVal) wi
              yield $ convertString $ T.toByteString res
            else do
              yield (convertString $ T.toByteString $ T.PubACKPkt (T.PubACK pid)) .| appSink extClient
        MQTTError e -> do
          err e
        MQTTOther -> yield $ convertString $ T.toByteString res
    _ -> yield $ convertString $ T.toByteString res
      
-- | traffic going upstream (from internal MQTT server to external client)
filterMQTTout :: WaziupInfo -> TVar (Maybe ConnCache) -> AppData -> ConduitT (w, T.MQTTPkt) B.ByteString IO ()
filterMQTTout wi tvConnCache intServer = awaitForever $ \(_, res) -> do
  debug $ "Received downstream: " ++ (show res)
  case res of
    -- Decode Publish
    (T.PublishPkt (T.PublishRequest _ _ _ topic id body)) -> do
      -- Decode MQTT message
      case decodePub topic body of
        MQTTSen did _ _ -> do
          -- check authorization
          perms <- lift $ runWaziup (getPerms' tvConnCache (getPermReq (Just $ PermDeviceId did) [DevicesDataView])) wi
          let isAuth = isPermittedResource DevicesDataView (PermDeviceId did) (fromRight [] perms)
          debug $ "Perm check downstream: " ++ (show isAuth)
          if isAuth 
            then yield $ convertString $ T.toByteString res
            else yield (convertString $ T.toByteString $ T.PubACKPkt (T.PubACK id)) .| appSink intServer
        MQTTAct did _ _ -> do 
          perms <- lift $ runWaziup (getPerms' tvConnCache (getPermReq (Just $ PermDeviceId did) [DevicesDataView])) wi
          let isAuth = isPermittedResource DevicesDataView (PermDeviceId did) (fromRight [] perms)
          debug $ "Perm check downstream: " ++ (show isAuth)
          if isAuth 
            then yield $ convertString $ T.toByteString res
            else yield (convertString $ T.toByteString $ T.PubACKPkt (T.PubACK id)) .| appSink intServer
        MQTTError e -> do
          err e
        MQTTOther -> yield $ convertString $ T.toByteString res
    T.DisconnectPkt -> do
      yield $ convertString $ T.toByteString res
    _ -> yield $ convertString $ T.toByteString res
  
  
getPerms' :: TVar (Maybe ConnCache) -> PermReq -> Waziup [Perm]
getPerms' tvConnCache permReq = do
  res <- liftIO $ atomically $ readTVar tvConnCache 
  case res of
    Just (ConnCache user pass _) -> do
      debug $ "Get perms with user: " ++ (show user)
      if isJust user && isJust pass
        then do
          tok <- liftKeycloak' $ getUserAuthToken (fromJust user) (fromJust pass)
          getPerms (Just tok) permReq 
        else do
          getPerms Nothing permReq
    Nothing -> return []

decodePub :: LB.ByteString -> LB.ByteString -> MQTTData
decodePub topic body = do
  case T.split (== '/') (convertString topic) of
    ["devices", d, "sensors", s, "value"] -> do
      case decode body of
        Just senVal -> MQTTSen (DeviceId d) (SensorId s) senVal
        Nothing -> MQTTError "Wrong body" 
    ["devices", d, "actuators", s, "value"] -> do
      case decode body of
        Just actVal -> MQTTAct (DeviceId d) (ActuatorId s) actVal
        Nothing -> MQTTError "Wrong body" 
    _ -> MQTTOther 

-- Post sensor value to DBs
-- TODO: access control
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
    (Just (ConnCache _ _ cid)) -> do
      debug $ "Putting connected=" ++ (show isConnected) ++ " on gateway " ++ (show cid)
      void $ runMongo $ modify (select ["_id" =: (convertString cid :: T.Text)] "gateways") [ "$set" =: Doc ["connected" =: val isConnected]]
    Nothing -> return ()

publishSensorValue :: DeviceId -> SensorId -> SensorValue -> Waziup ()
publishSensorValue (DeviceId d) (SensorId s) v = do
  let topic = "devices/" <> d <> "/sensors/" <> s <> "/value"
  (MQTTConfig host port) <- view $ waziupConfig.mqttConf
  info $ "Publish sensor value: " ++ (convertString $ encode v) ++ " to topic: " ++ (show topic)
  liftIO $ do
    mc <- runClient mqttConfig { _hostname = convertString host, _port = port}
    liftIO $ publish mc topic (convertString $ encode v) False

publishActuatorValue :: DeviceId -> ActuatorId -> JSON.Value -> Waziup ()
publishActuatorValue (DeviceId d) (ActuatorId a) v = do
  let topic = "devices/" <> d <> "/actuator/" <> a <> "/value"
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

