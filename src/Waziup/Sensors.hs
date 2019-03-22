{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Sensors where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Devices hiding (info, warn, debug, err)
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Data.Text hiding (map, filter, foldl, any)
import qualified Data.List as L
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           Orion as O hiding (info, warn, debug, err)
import           System.Log.Logger
import           MQTT hiding (info, warn, debug, err) 

getSensors :: Maybe Token -> DeviceId -> Waziup [Sensor]
getSensors tok did = do
  info "Get sensors"
  withKCId did $ \(keyId, device) -> do
    debug "Check permissions"
    liftKeycloak tok $ checkPermission keyId (pack $ show DevicesView)
    debug "Permission granted, returning sensors"
    return $ devSensors device

postSensor :: Maybe Token -> DeviceId -> Sensor -> Waziup NoContent
postSensor tok did sensor = do
  info $ "Post sensor: " ++ (show sensor)
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    liftKeycloak tok $ checkPermission keyId (pack $ show DevicesUpdate)
    debug "Permission granted, creating sensor"
    let att = getAttFromSensor sensor
    liftOrion $ O.postAttribute (toEntityId did) att 
    return NoContent
 
getSensor :: Maybe Token -> DeviceId -> SensorId -> Waziup Sensor
getSensor tok did sid = do
  info "Get sensor"
  withKCId did $ \(keyId, device) -> do
     debug "Check permissions"
     liftKeycloak tok $ checkPermission keyId (pack $ show DevicesView)
     debug "Permission granted, returning sensor"
     case L.find (\s -> senId s == sid) (devSensors device) of
       Just sensor -> return sensor
       Nothing -> do 
         warn "Sensor not found"
         throwError err404 {errBody = "Sensor not found"}

deleteSensor :: Maybe Token -> DeviceId -> SensorId -> Waziup NoContent
deleteSensor tok did sid = do
  info "Delete sensor"
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    liftKeycloak tok $ checkPermission keyId (pack $ show DevicesUpdate)
    debug "Permission granted, deleting sensor"
    liftOrion $ O.deleteAttribute (toEntityId did) (toAttributeId sid)
    debug "Deleting Mongo datapoints"
    runMongo $ deleteSensorDatapoints did sid
    return NoContent

putSensorName :: Maybe Token -> DeviceId -> SensorId -> SensorName -> Waziup NoContent
putSensorName mtok did sid name = do
  info $ "Put sensor name: " ++ (show name)
  updateSensorField mtok did sid $ \sensor -> do 
    liftOrion $ O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senName = Just name})

putSensorSensorKind :: Maybe Token -> DeviceId -> SensorId -> SensorKindId -> Waziup NoContent
putSensorSensorKind mtok did sid sk = do
  info $ "Put sensor sensor kind: " ++ (show sk)
  updateSensorField mtok did sid $ \sensor -> do 
    liftOrion $ O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senSensorKind = Just sk})

putSensorQuantityKind :: Maybe Token -> DeviceId -> SensorId -> QuantityKindId -> Waziup NoContent
putSensorQuantityKind mtok did sid qk = do
  info $ "Put sensor quantity kind: " ++ (show qk)
  updateSensorField mtok did sid $ \sensor -> do 
    liftOrion $ O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senQuantityKind = Just qk})

putSensorUnit :: Maybe Token -> DeviceId -> SensorId -> UnitId -> Waziup NoContent
putSensorUnit mtok did sid u = do
  info $ "Put sensor unit: " ++ (show u)
  updateSensorField mtok did sid $ \sensor -> do 
    liftOrion $ O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senUnit = Just u})

putSensorCalib :: Maybe Token -> DeviceId -> SensorId -> Calib -> Waziup NoContent
putSensorCalib mtok did sid cal = do
  info $ "Put sensor cal: " ++ (show cal)
  updateSensorField mtok did sid $ \sensor -> do 
    liftOrion $ O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senCalib = Just cal})

putSensorValue :: Maybe Token -> DeviceId -> SensorId -> SensorValue -> Waziup NoContent
putSensorValue mtok did sid senVal@(SensorValue v ts dr) = do
  info $ "Put sensor value: " ++ (show senVal)
  withKCId did $ \(keyId, device) -> do
    debug "Check permissions"
    liftKeycloak mtok $ checkPermission keyId (pack $ show DevicesDataCreate)
    debug "Permission granted, updating sensor"
    case L.find (\s -> (senId s) == sid) (devSensors device) of
      Just sensor -> do
        liftOrion $ O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senValue = Just senVal})
        runMongo $ postDatapoint $ Datapoint did sid v ts dr
        publishSensorValue did sid senVal
        return NoContent
      Nothing -> do 
        warn "sensor not found"
        throwError err404 {errBody = "Sensor not found"}
  
updateSensorField :: Maybe Token -> DeviceId -> SensorId -> (Sensor -> Waziup ()) -> Waziup NoContent
updateSensorField mtok did sid w = do
  withKCId did $ \(keyId, device) -> do
    debug "Check permissions"
    liftKeycloak mtok $ checkPermission keyId (pack $ show DevicesUpdate)
    debug "Permission granted, updating sensor"
    case L.find (\s -> (senId s) == sid) (devSensors device) of
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

