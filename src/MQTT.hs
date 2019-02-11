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
import           System.Log.Logger
import           Waziup.Types
import           Waziup.Utils
import           Orion as O hiding (info, warn, debug, err)
import           Waziup.Devices hiding (info, warn, debug, err)
import           Network.MQTT.Client hiding (info, warn, debug, err)
import qualified Network.MQTT.Types as T
import           Conduit
import           Data.Conduit.Network
import           Control.Concurrent.Async (concurrently)
import           Data.Conduit.Attoparsec (conduitParser, sinkParser)
import           Data.Attoparsec.ByteString
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           Servant.Server.Internal.Handler


mqttProxy :: WaziupInfo -> IO ()
mqttProxy wi = do
  runTCPServer (serverSettings 4002 "*") handleClient where
    handleClient :: AppData -> IO ()
    handleClient client = runTCPClient (clientSettings 1883 "localhost") (handleServer wi client) where

handleServer :: WaziupInfo -> AppData -> AppData -> IO ()
handleServer wi clientData serverData = do
  permst <- atomically $ newTVar []
  void $ concurrently
              (runConduit $ appSource serverData .| filterMQTTout wi permst .| appSink clientData)
              (runConduit $ appSource clientData .| filterMQTTin wi permst .| appSink serverData)
  debug "Connection finished"

data MQTTData = MQTTSen DeviceId SensorId SensorValue | 
                MQTTAct DeviceId ActuatorId JSON.Value |
                MQTTError String |
                MQTTOther

filterMQTTout :: WaziupInfo -> TVar [Perm] -> ConduitT B.ByteString B.ByteString IO ()
filterMQTTout wi tperms = awaitForever $ \p -> do
  let res = parse T.parsePacket p
  debug $ "Received downstream: " ++ (show res)
  case res of
    -- Decode Publish
    Done _ (T.PublishPkt (T.PublishRequest _ _ _ topic _ body)) -> do
      -- Get the permissions
      perms <- liftIO $ atomically $ readTVar tperms
      -- Decode MQTT message
      case decodePub topic body of
        MQTTSen devId senId senVal -> do
          -- check authorization
          let auth = checkPermDevice DevicesDataView perms devId
          debug $ "Perm check downstream: " ++ (show auth)
          when auth $ yield p
        MQTTAct devId actId actVal -> do 
          let auth = checkPermDevice DevicesDataCreate perms devId
          debug $ "Perm check downstream: " ++ (show auth)
          when auth $ yield p
        MQTTError e -> do
          err e
        MQTTOther -> yield p
    _ -> yield p
  

filterMQTTin :: WaziupInfo -> TVar [Perm] -> ConduitT B.ByteString B.ByteString IO ()
filterMQTTin wi tperms = awaitForever $ \p -> do
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
    Done _ (T.PublishPkt (T.PublishRequest _ _ _ topic _ body)) -> do
      -- Get the permissions
      perms <- liftIO $ atomically $ readTVar tperms
      -- Decode MQTT message
      case decodePub topic body of
        MQTTSen devId senId senVal -> do
          -- check authorization
          let auth = checkPermDevice DevicesDataCreate perms devId
          debug $ "Perm check: " ++ (show auth)
          when auth $ do
            -- Store the value
            lift $ runWaziup (postSensorValue devId senId senVal) wi
            -- pass value to MQTT server
            yield p
        MQTTAct devId actId actVal -> do 
          let auth = checkPermDevice DevicesDataCreate perms devId
          debug $ "Perm check: " ++ (show auth)
          when auth $ do
            lift $ runWaziup (putActuatorValue devId actId actVal) wi
            yield p
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
      getPerms (Just tok)
    else do
      getPerms Nothing 

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
  ent <- runOrion (O.getEntity $ toEntityId did)
  let mdevice = getDeviceFromEntity ent
  case L.find (\s -> (senId s) == sid) (devSensors $ fromJust mdevice) of
      Just sensor -> do
        runOrion (O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senValue = Just senVal}))
        runMongo (postDatapoint $ Datapoint did sid v ts dr)
        return ()
      Nothing -> do 
        err "sensor not found"

putActuatorValue :: DeviceId -> ActuatorId -> JSON.Value -> Waziup ()
putActuatorValue did aid actVal = do
  info $ convertString $ "Post device " <> (unDeviceId did) <> ", actuator " <> (unActuatorId aid) <> ", value: " <> (convertString $ show actVal)
  ent <- runOrion (O.getEntity $ toEntityId did)
  let mdevice = getDeviceFromEntity ent
  case L.find (\a -> (actId a) == aid) (devActuators $ fromJust mdevice) of
      Just act -> do
        runOrion (O.postAttribute (toEntityId did) $ getAttFromActuator (act {actValue = Just actVal}))
        return ()
      Nothing -> do 
        err "actuator not found"

publishSensorValue :: DeviceId -> SensorId -> SensorValue -> IO ()
publishSensorValue (DeviceId d) (SensorId s) v = do
  let topic = "devices/" <> d <> "/sensors/" <> s <> "/value"
  mc <- runClient mqttConfig { _connID = "pub"}
  info $ "Publish sensor value: " ++ (convertString $ encode v) ++ " to topic: " ++ (show topic)
  publish mc topic (convertString $ encode v) False

publishActuatorValue :: DeviceId -> ActuatorId -> JSON.Value -> IO ()
publishActuatorValue (DeviceId d) (ActuatorId a) v = do
  let topic = "devices/" <> d <> "/actuator/" <> a <> "/value"
  mc <- runClient mqttConfig { _connID = "pub"}
  info $ "Publish actuator value: " ++ (convertString $ encode v) ++ " to topic: " ++ (show topic)
  publish mc topic (convertString $ encode v) False

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "MQTT" s
info  s = liftIO $ infoM    "MQTT" s
warn  s = liftIO $ warningM "MQTT" s
err   s = liftIO $ errorM   "MQTT" s

