{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Sensors where

import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Lens hiding ((.=))
import qualified Data.List as L
import           Data.Time
import           Keycloak as KC hiding (Scope) 
import           MQTT hiding (info, warn, debug, err) 
import           Orion as O hiding (info, warn, debug, err)
import           Servant
import           System.Log.Logger
import           Waziup.Types
import           Waziup.Utils
import           Waziup.SensorData hiding (info, warn, debug, err)
import           Waziup.Auth hiding (info, warn, debug, err)
import           Waziup.Devices hiding (info, warn, debug, err)

getSensors :: AuthUser -> DeviceId -> Waziup [Sensor]
getSensors tok did = do
  info "Get sensors"
  d <- getDevice tok did
  debug "Check permissions"
  checkPermResource tok DevicesView (PermDevice d)
  debug "Permission granted, returning sensors"
  device <- getDeviceFromEntity <$> (liftOrion $ O.getEntity (EntityId $ unDeviceId did) devTyp)
  return $ maybeToList' $ devSensors device

postSensor :: AuthUser -> DeviceId -> Sensor -> Waziup NoContent
postSensor tok did sensor = do
  info $ "Post sensor: " ++ (show sensor)
  d <- getDevice tok did
  debug "Check permissions"
  checkPermResource tok DevicesUpdate (PermDevice d)
  debug "Permission granted, creating sensor"
  liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromSensor sensor)
  return NoContent
 
getSensor :: AuthUser -> DeviceId -> SensorId -> Waziup Sensor
getSensor tok did sid = do
  info "Get sensor"
  d <- getDevice tok did
  debug "Check permissions"
  checkPermResource tok DevicesView (PermDevice d)
  debug "Permission granted, returning sensor"
  device <- getDeviceOrion did
  case L.find (\s -> senId s == sid) (maybeToList' $ devSensors device) of
    Just sensor -> return sensor
    Nothing -> do 
      warn "Sensor not found"
      throwError err404 {errBody = "Sensor not found"}

deleteSensor :: AuthUser -> DeviceId -> SensorId -> Waziup NoContent
deleteSensor tok did sid = do
  info "Delete sensor"
  d <- getDevice tok did
  debug "Check permissions"
  checkPermResource tok DevicesUpdate (PermDevice d)
  debug "Permission granted, deleting sensor"
  liftOrion $ O.deleteAttribute (toEntityId did) devTyp (toAttributeId sid)
  debug "Deleting Mongo datapoints"
  runMongo $ deleteSensorDatapoints did sid
  return NoContent

putSensorName :: AuthUser -> DeviceId -> SensorId -> SensorName -> Waziup NoContent
putSensorName mtok did sid name = do
  info $ "Put sensor name: " ++ (show name)
  updateSensorField mtok did sid $ \sensor -> do 
    liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromSensor (sensor {senName = Just name}))

putSensorMetaField :: AuthUser -> DeviceId -> SensorId -> MetadataValue -> Waziup NoContent
putSensorMetaField mtok did sid meta = do
  info $ "Put sensor metafield: " ++ (show meta)
  updateSensorField mtok did sid $ \sensor -> do 
    liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromSensor (sensor {senMeta = Just meta}))


putSensorSensorKind :: AuthUser -> DeviceId -> SensorId -> SensorKindId -> Waziup NoContent
putSensorSensorKind mtok did sid sk = do
  info $ "Put sensor sensor kind: " ++ (show sk)
  updateSensorField mtok did sid $ \sensor -> do 
    liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromSensor (sensor {senSensorKind = Just sk}))

putSensorQuantityKind :: AuthUser -> DeviceId -> SensorId -> QuantityKindId -> Waziup NoContent
putSensorQuantityKind mtok did sid qk = do
  info $ "Put sensor quantity kind: " ++ (show qk)
  updateSensorField mtok did sid $ \sensor -> do 
    liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromSensor (sensor {senQuantityKind = Just qk}))

putSensorUnit :: AuthUser -> DeviceId -> SensorId -> UnitId -> Waziup NoContent
putSensorUnit mtok did sid u = do
  info $ "Put sensor unit: " ++ (show u)
  updateSensorField mtok did sid $ \sensor -> do 
    liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromSensor (sensor {senUnit = Just u}))

putSensorCalib :: AuthUser -> DeviceId -> SensorId -> Calib -> Waziup NoContent
putSensorCalib mtok did sid cal = do
  info $ "Put sensor cal: " ++ (show cal)
  updateSensorField mtok did sid $ \sensor -> do 
    liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromSensor (sensor {senCalib = Just cal}))

putSensorValue :: AuthUser -> DeviceId -> SensorId -> SensorValue -> Waziup NoContent
putSensorValue mtok did sid sv = do
  info $ "Put sensor value: " ++ (show sv)
  d <- getDevice mtok did
  debug "Check permissions"
  checkPermResource mtok DevicesDataCreate (PermDevice d)
  debug "Permission granted, updating sensor"
  device <- getDeviceOrion did
  case L.find (\s -> (senId s) == sid) (maybeToList' $ devSensors device) of
    Just sensor -> putSensorValue' did sensor sv
    Nothing -> do 
      warn "sensor not found"
      throwError err404 {errBody = "Sensor not found"}

putSensorValue' :: DeviceId -> Sensor -> SensorValue -> Waziup NoContent
putSensorValue' did sensor val@(SensorValue v ts dr) = do
  -- Update data on ORion
  liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromSensor (sensor {senValue = Just val}))
  -- Push in DB
  runMongo $ postDatapoint $ Datapoint did (senId sensor) v ts dr
  -- publish on MQTT
  mqttAct <- view $ waziupConfig.serverConf.mqttActivated
  when mqttAct $ publishSensorValue did (senId sensor) val
  return NoContent

putSensorValues :: AuthUser -> DeviceId -> SensorId -> [SensorValue] -> Waziup NoContent
putSensorValues mtok did sid svs = do
  info $ "Put sensor values"
  d <- getDevice mtok did
  debug "Check permissions"
  checkPermResource mtok DevicesDataCreate (PermDevice d)
  debug "Permission granted, updating sensor"
  device <- getDeviceOrion did
  case L.find (\s -> (senId s) == sid) (maybeToList' $ devSensors device) of
    Just sensor -> do
      mapM_ (putSensorValue' did sensor) svs
      return NoContent
    Nothing -> do 
      warn "sensor not found"
      throwError err404 {errBody = "Sensor not found"}

getSensorValues :: AuthUser
              -> DeviceId
              -> SensorId
              -> Maybe Int
              -> Maybe Int
              -> Maybe Sort
              -> Maybe UTCTime
              -> Maybe UTCTime
              -> Maybe Bool
              -> Waziup [Datapoint]
getSensorValues tok mdids msids lim offset srt dateFrom dateTo calibEn = getDatapoints tok (Just [mdids]) (Just [msids]) lim offset srt dateFrom dateTo calibEn
  
updateSensorField :: AuthUser -> DeviceId -> SensorId -> (Sensor -> Waziup ()) -> Waziup NoContent
updateSensorField mtok did sid w = do
  debug "Check permissions"
  d <- getDevice mtok did
  checkPermResource mtok DevicesUpdate (PermDevice d)
  debug "Permission granted, updating sensor"
  device <- getDeviceOrion did
  case L.find (\s -> (senId s) == sid) (maybeToList' $ devSensors device) of
    Just sensor -> do
      w sensor
      return NoContent
    Nothing -> do 
      warn "sensor not found"
      throwError err404 {errBody = "Sensor not found"}

toAttributeId :: SensorId -> AttributeId
toAttributeId (SensorId sid) = AttributeId sid

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Sensors" s
info  s = liftIO $ infoM    "Sensors" s
warn  s = liftIO $ warningM "Sensors" s
err   s = liftIO $ errorM   "Sensors" s

