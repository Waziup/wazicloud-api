{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Sensors where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Devices hiding (info, warn, debug, err)
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Data.Maybe
import           Data.Text hiding (map, filter, foldl, any)
import           Data.String.Conversions
import qualified Data.List as L
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           Orion as O hiding (info, warn, debug, err)
import           Mongo as M hiding (info, warn, debug, err)
import           System.Log.Logger
import           Paths_Waziup_Servant


getSensors :: Maybe Token -> DeviceId -> Waziup [Sensor]
getSensors tok did = do
  info "Get measurements"
  withKCId did $ \(keyId, device) -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show DevicesView) tok
    debug "Permission granted, returning sensors"
    return $ devSensors device

postSensor :: Maybe Token -> DeviceId -> Sensor -> Waziup NoContent
postSensor tok did sensor = do
  info $ "Post sensor: " ++ (show sensor)
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show DevicesUpdate) tok
    debug "Permission granted, creating sensor"
    let att = getAttFromSensor sensor
    runOrion $ O.postAttribute (toEntityId did) att 
    return NoContent
 
getSensor :: Maybe Token -> DeviceId -> SensorId -> Waziup Sensor
getSensor tok did sid = do
  info "Get sensor"
  withKCId did $ \(keyId, device) -> do
     debug "Check permissions"
     runKeycloak $ checkPermission keyId (pack $ show DevicesView) tok
     debug "Permission granted, returning measurement"
     case L.find (\s -> senId s == sid) (devSensors device) of
       Just sensor -> return sensor
       Nothing -> do 
         warn "Sensor not found"
         throwError err404 {errBody = "Sensor not found"}

deleteSensor :: Maybe Token -> DeviceId -> SensorId -> Waziup NoContent
deleteSensor tok did sid = do
  info "Delete measurement"
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show DevicesUpdate) tok
    debug "Permission granted, deleting measurement"
    runOrion $ O.deleteAttribute (toEntityId did) (toAttributeId sid)
    return NoContent

putSensorName :: Maybe Token -> DeviceId -> SensorId -> SensorName -> Waziup NoContent
putSensorName mtok did sid name = do
  info $ "Put meas name: " ++ (show name)
  updateSensorField mtok did sid $ \sensor -> do 
    runOrion $ O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senName = Just name})

putSensorSensorKind :: Maybe Token -> DeviceId -> SensorId -> SensorKindId -> Waziup NoContent
putSensorSensorKind mtok did sid sk = do
  info $ "Put meas sensor kind: " ++ (show sk)
  updateSensorField mtok did sid $ \sensor -> do 
    runOrion $ O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senSensorKind = Just sk})

putSensorQuantityKind :: Maybe Token -> DeviceId -> SensorId -> QuantityKindId -> Waziup NoContent
putSensorQuantityKind mtok did sid qk = do
  info $ "Put meas quantity kind: " ++ (show qk)
  updateSensorField mtok did sid $ \sensor -> do 
    runOrion $ O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senQuantityKind = Just qk})

putSensorUnit :: Maybe Token -> DeviceId -> SensorId -> UnitId -> Waziup NoContent
putSensorUnit mtok did sid u = do
  info $ "Put meas unit: " ++ (show u)
  updateSensorField mtok did sid $ \sensor -> do 
    runOrion $ O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senUnit = Just u})

putSensorValue :: Maybe Token -> DeviceId -> SensorId -> SensorValue -> Waziup NoContent
putSensorValue mtok did sid senVal = do
  info $ "Put meas value: " ++ (show senVal)
  updateSensorField mtok did sid $ \sensor -> do 
    runOrion $ O.postAttribute (toEntityId did) $ getAttFromSensor (sensor {senLastValue = Just senVal})
    runMongo $ M.postDatapoint $ Datapoint did sid senVal
  
updateSensorField :: Maybe Token -> DeviceId -> SensorId -> (Sensor -> Waziup ()) -> Waziup NoContent
updateSensorField mtok did sid w = do
  withKCId did $ \(keyId, device) -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show DevicesUpdate) mtok
    debug "Permission granted, updating measurement"
    case L.find (\s -> (senId s) == sid) (devSensors device) of
      Just sensor -> do
        w sensor
        return NoContent
      Nothing -> do 
        warn "Measurement not found"
        throwError err404 {errBody = "Measurement not found"}

toAttributeId :: SensorId -> AttributeId
toAttributeId (SensorId sid) = AttributeId sid


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Sensors" s
info  s = liftIO $ infoM    "Sensors" s
warn  s = liftIO $ warningM "Sensors" s
err   s = liftIO $ errorM   "Sensors" s

