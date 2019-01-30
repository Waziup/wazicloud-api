{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module MQTT where

import           Data.Aeson
import           Data.String.Conversions
import qualified Data.List as L
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad (unless, forever)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Except (throwError, runExceptT)
import           System.IO (hPutStrLn, stderr)
import           System.Log.Logger
import           Waziup.Types
import           Orion as O hiding (info, warn, debug, err)
import           Waziup.Devices hiding (info, warn, debug, err)
import           Network.MQTT.Client
import           Database.MongoDB as DB

sensorsTopic :: Topic
sensorsTopic = "devices/+/sensors/+/value"


mqttClient :: WaziupInfo -> IO ()
mqttClient wi = do
  pubChan <- newTChanIO
  mc <- runClient mqttConfig {_msgCB  = Just (handleMsg pubChan),
                              _connID = "sub"}
  res <- subscribe mc [(sensorsTopic, QoS0)]
  case res of
    [Just QoS0] -> forkIO $ forever $ readMsg pubChan wi
    _ -> error "Subscribe failed"
  res <- waitForClient mc   -- wait for the the client to disconnect
  err $ "MQTT client terminated:" ++ (show res)

handleMsg :: TChan (Topic, BL.ByteString) -> MQTTClient -> Topic -> BL.ByteString -> IO ()
handleMsg tc mq topic payload = do
  atomically $ writeTChan tc (topic, payload)


readMsg :: TChan (Topic, BL.ByteString) -> WaziupInfo -> IO ()
readMsg tc wi = do
  (topic, payload) <- liftIO $ atomically $ readTChan tc
  case T.split (== '/') topic of
    ["devices", d, "sensors", s, "value"] -> do
       case (decode $ convertString payload) of
         Just val -> runReaderT (postSensorValue (DeviceId d) (SensorId s) val) wi
         Nothing -> err "not a sensor value"
  
postSensorValue :: DeviceId -> SensorId -> SensorValue -> ReaderT WaziupInfo IO ()
postSensorValue did sid senVal@(SensorValue v ts dr) = do 
  info $ convertString $ "Post device " <> (unDeviceId did) <> ", sensor " <> (unSensorId sid) <> ", value: " <> (convertString $ show senVal)
  (WaziupInfo pipe (WaziupConfig _ _ _ conf) _) <- ask
  eent <- liftIO $ runExceptT $ runReaderT (O.getEntity $ toEntityId did) conf
  case eent of 
    Right ent -> do
      let mdevice = getDeviceFromEntity ent
      case L.find (\s -> (senId s) == sid) (devSensors $ fromJust mdevice) of
          Just sensor -> do
            liftIO $ runExceptT $ runReaderT (O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senValue = Just senVal})) conf
            liftIO $ access pipe DB.master "waziup" (postDatapoint $ Datapoint did sid v ts dr)
            return ()
          Nothing -> do 
            err "sensor not found"
    Left e -> err "Orion error"

publishSensorValue :: DeviceId -> SensorId -> SensorValue -> IO ()
publishSensorValue (DeviceId d) (SensorId s) v = do
  let topic = "devices/" <> d <> "/sensors/" <> s <> "/value"
  mc <- runClient mqttConfig { _connID = "pub"}
  info $ "Publish sensor value: " ++ (convertString $ encode v) ++ " to topic: " ++ (show topic)
  publish mc topic (convertString $ encode v) False

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "MQTT" s
info  s = liftIO $ infoM    "MQTT" s
warn  s = liftIO $ warningM "MQTT" s
err   s = liftIO $ errorM   "MQTT" s

