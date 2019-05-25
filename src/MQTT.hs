{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module MQTT where

import           Data.Aeson as JSON
import           Data.String.Conversions
import qualified Data.List as L
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Except (runExceptT)
import           Control.Lens
import           System.Log.Logger
import           Waziup.Types
import           Waziup.Utils
import           Waziup.Auth hiding (info, warn, debug, err)
import           Orion as O hiding (info, warn, debug, err)
import           Waziup.Devices hiding (info, warn, debug, err)
import           Network.MQTT.Client hiding (info, warn, debug, err, MQTTConfig)
import qualified Network.MQTT.Types as T
import           Conduit
import           Data.Conduit.Network
import           Control.Concurrent.Async (concurrently)
import           Data.Conduit.Attoparsec (conduitParser, sinkParser)
import           Data.Attoparsec.ByteString
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           Servant.Server.Internal.Handler

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
  permst <- atomically $ newTVar []
  void $ concurrently
    (runConduit $ appSource extClient .| filterMQTTin  wi permst extClient .| appSink intServer) --traffic downstream
    (runConduit $ appSource intServer .| filterMQTTout wi permst intServer .| appSink extClient) --traffic upstream

data MQTTData = MQTTSen DeviceId SensorId SensorValue | 
                MQTTAct DeviceId ActuatorId JSON.Value |
                MQTTError String |
                MQTTOther
-- | traffic going downstream (from external client to internal MQTT server)
filterMQTTin :: WaziupInfo -> TVar [Perm] -> AppData -> ConduitT B.ByteString B.ByteString IO ()
filterMQTTin wi tperms extClient = awaitForever $ \p -> do
  let res = parse T.parsePacket p
  debug $ "Received: " ++ (show res)
  case res of
    -- Decode Connect request
    Done _ (T.ConnPkt (T.ConnectRequest user pass _ _ _ _)) -> do
      -- get permissions for this user
      res <- lift $ runWaziup (getPerms' (convertString <$> user) (convertString <$> pass)) wi
      case res of
        Right perms -> do
          debug $ "Got perms:" ++ (show perms)
          -- store permissions
          liftIO $ atomically $ writeTVar tperms perms 
          yield p
        Left e -> do 
          err $ "Could not get Waziup permissions: " ++ (show e)
          return ()
    -- Decode Publish
    Done _ (T.PublishPkt (T.PublishRequest _ _ _ topic id body)) -> do
      -- Get the permissions
      perms <- liftIO $ atomically $ readTVar tperms
      -- Decode MQTT message
      case decodePub topic body of
        MQTTSen devId senId senVal -> do
          -- check authorization
          let auth = checkPermDevice DevicesDataCreate perms devId
          debug $ "Perm check: " ++ (show auth)
          if auth 
            then do
              -- Store the value
              lift $ runWaziup (postSensorValue devId senId senVal) wi
              -- pass value to MQTT server
              yield p
            else do
              -- return PUBACK directly to client (MQTT offers no way to inform the client about the failure)
              yield (convertString $ T.toByteString $ T.PubACKPkt (T.PubACK id)) .| appSink extClient

        MQTTAct devId actId actVal -> do 
          let auth = checkPermDevice DevicesDataCreate perms devId
          debug $ "Perm check: " ++ (show auth)
          if auth 
            then do
              lift $ runWaziup (putActuatorValue devId actId actVal) wi
              yield p
            else do
              yield (convertString $ T.toByteString $ T.PubACKPkt (T.PubACK id)) .| appSink extClient
        MQTTError e -> do
          err e
        MQTTOther -> yield p
    _ -> yield p
      
-- | traffic going upstream (from internal MQTT server to external client)
filterMQTTout :: WaziupInfo -> TVar [Perm] -> AppData -> ConduitT B.ByteString B.ByteString IO ()
filterMQTTout wi tperms intServer = awaitForever $ \p -> do
  let res = parse T.parsePacket p
  debug $ "Received downstream: " ++ (show res)
  case res of
    -- Decode Publish
    Done _ (T.PublishPkt (T.PublishRequest _ _ _ topic id body)) -> do
      -- Get the permissions
      perms <- liftIO $ atomically $ readTVar tperms
      -- Decode MQTT message
      case decodePub topic body of
        MQTTSen devId senId senVal -> do
          -- check authorization
          let auth = checkPermDevice DevicesDataView perms devId
          debug $ "Perm check downstream: " ++ (show auth)
          if auth 
            then yield p
            else yield (convertString $ T.toByteString $ T.PubACKPkt (T.PubACK id)) .| appSink intServer
        MQTTAct devId actId actVal -> do 
          let auth = checkPermDevice DevicesDataCreate perms devId
          debug $ "Perm check downstream: " ++ (show auth)
          if auth 
            then yield p
            else yield (convertString $ T.toByteString $ T.PubACKPkt (T.PubACK id)) .| appSink intServer
        MQTTError e -> do
          err e
        MQTTOther -> yield p
    _ -> yield p
  
  
getPerms' :: Maybe T.Text -> Maybe T.Text -> Waziup [Perm]
getPerms' user pass = do
  debug $ "Connect with user: " ++ (show user)
  if isJust user && isJust pass
    then do
      tok <- liftKeycloak' $ getUserAuthToken (fromJust user) (fromJust pass)
      getPermsDevices (Just tok)
    else do
      getPermsDevices Nothing 

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
  ent <- liftOrion $ O.getEntity (toEntityId did) (Just "Device")
  let mdevice = getDeviceFromEntity ent
  case L.find (\s -> (senId s) == sid) (maybeToList' $ devSensors $ fromJust mdevice) of
      Just sensor -> do
        liftOrion (O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senValue = Just senVal}))
        runMongo (postDatapoint $ Datapoint did sid v ts dr)
        return ()
      Nothing -> do 
        err "sensor not found"

putActuatorValue :: DeviceId -> ActuatorId -> JSON.Value -> Waziup ()
putActuatorValue did aid actVal = do
  info $ convertString $ "Post device " <> (unDeviceId did) <> ", actuator " <> (unActuatorId aid) <> ", value: " <> (convertString $ show actVal)
  ent <- liftOrion $ O.getEntity (toEntityId did) (Just "Device")
  let mdevice = getDeviceFromEntity ent
  case L.find (\a -> (actId a) == aid) (maybeToList' $ devActuators $ fromJust mdevice) of
      Just act -> do
        liftOrion (O.postAttribute (toEntityId did) $ getAttFromActuator (act {actValue = Just actVal}))
        return ()
      Nothing -> do 
        err "actuator not found"

publishSensorValue :: DeviceId -> SensorId -> SensorValue -> Waziup ()
publishSensorValue (DeviceId d) (SensorId s) v = do
  let topic = "devices/" <> d <> "/sensors/" <> s <> "/value"
  (MQTTConfig host port) <- view $ waziupConfig.mqttConf
  info $ "Publish sensor value: " ++ (convertString $ encode v) ++ " to topic: " ++ (show topic)
  liftIO $ do
    mc <- runClient mqttConfig { _connID = "pub", _hostname = convertString host, _port = port}
    liftIO $ publish mc topic (convertString $ encode v) False

publishActuatorValue :: DeviceId -> ActuatorId -> JSON.Value -> Waziup ()
publishActuatorValue (DeviceId d) (ActuatorId a) v = do
  let topic = "devices/" <> d <> "/actuator/" <> a <> "/value"
  (MQTTConfig host port) <- view $ waziupConfig.mqttConf
  info $ "Publish actuator value: " ++ (convertString $ encode v) ++ " to topic: " ++ (show topic)
  liftIO $ do
    mc <- runClient mqttConfig { _connID = "pub", _hostname = convertString host, _port = port}
    liftIO $ publish mc topic (convertString $ encode v) False

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "MQTT" s
info  s = liftIO $ infoM    "MQTT" s
warn  s = liftIO $ warningM "MQTT" s
err   s = liftIO $ errorM   "MQTT" s

