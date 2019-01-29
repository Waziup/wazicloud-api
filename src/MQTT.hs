{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module MQTT where

import qualified Network.MQTT as MQTT
import           Data.Aeson
import           Data.String.Conversions
import qualified Data.List as L
import           Data.Maybe
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
import Network.MQTT.Client
import           Database.MongoDB as DB

sensorsTopic :: MQTT.Topic
sensorsTopic = "devices/+/sensors/+/value"


mqttClient :: ReaderT WaziupInfo IO ()
mqttClient = do
  wi <- ask
  liftIO $ do
    cmds <- MQTT.mkCommands
    pubChan <- newTChanIO
    let conf = (MQTT.defaultConfig cmds pubChan)
                { MQTT.cUsername = Nothing
                , MQTT.cPassword = Nothing
                , MQTT.cLogDebug = debug}
    forkIO $ do
      qosGranted <- MQTT.subscribe conf [(sensorsTopic, MQTT.Handshake)]
      case qosGranted of
        [MQTT.Handshake] -> forever $ do
                              msg <- atomically (readTChan pubChan)
                              runReaderT (handleMsg msg) wi
        _                -> err $ "Wanted QoS Handshake, got " ++ show qosGranted
    info "Subscribe to MQTT server"
    terminated <- MQTT.run conf
    print terminated

handleMsg :: MQTT.Message MQTT.PUBLISH -> ReaderT WaziupInfo IO ()
handleMsg msg =
  -- sometimes it's useful to ignore retained messages
  unless (MQTT.retain $ MQTT.header msg) $ do
    let t = MQTT.topic $ MQTT.body msg
        p = MQTT.payload $ MQTT.body msg
    case MQTT.getLevels t of
      ["devices", d, "sensors", s, "value"] -> do
         case (decode $ convertString p) of
           Just val -> postSensorValue (DeviceId d) (SensorId s) val
           Nothing -> err "not a sensor value"


postSensorValue :: DeviceId -> SensorId -> SensorValue -> ReaderT WaziupInfo IO ()
postSensorValue did sid senVal@(SensorValue v ts dr) = do 
  info $ "Put sensor value: " ++ (show senVal)
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
  mc <- runClient mqttConfig
  info $ "Publish sensor value: " ++ (convertString $ encode v) ++ " to topic: " ++ (show topic)
  publish mc topic (convertString $ encode v) True

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "MQTT" s
info  s = liftIO $ infoM    "MQTT" s
warn  s = liftIO $ warningM "MQTT" s
err   s = liftIO $ errorM   "MQTT" s

