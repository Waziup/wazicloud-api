{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Measurements where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Sensors hiding (info, warn, debug, err)
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


getMeasurements :: Maybe Token -> SensorId -> Waziup [Measurement]
getMeasurements tok (SensorId sid) = do
  info "Get measurements"
  sensor <- getSensorFromEntity <$> runOrion (O.getEntity $ EntityId sid)
  case (senKeycloakId sensor) of
    Just keyId -> do
      debug "Check permissions"
      runKeycloak $ checkPermission keyId (pack $ show SensorsView) tok
      debug "Permission granted, returning measurements"
      return $ senMeasurements sensor
    Nothing -> do
      err "Error, sensor does not have a Keycloak ID"
      throwError err500 {errBody = "Not authorized"}

postMeasurement :: Maybe Token -> SensorId -> Measurement -> Waziup NoContent
postMeasurement tok (SensorId sid) meas = do
  info $ "Post measurement: " ++ (show meas)
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) tok
    debug "Permission granted, creating measurement"
    let att = getAttFromMeas meas
    runOrion $ O.postAttribute (EntityId sid) att 
  return NoContent
 
getMeasurement :: Maybe Token -> SensorId -> MeasId -> Waziup Measurement
getMeasurement tok (SensorId sid) mid = do
  info "Get measurement"
  sensor <- getSensorFromEntity <$> runOrion (O.getEntity $ EntityId sid)
  case (senKeycloakId sensor) of
    Just keyId -> do
      debug "Check permissions"
      runKeycloak $ checkPermission keyId (pack $ show SensorsView) tok
      debug "Permission granted, returning measurement"
      case L.find (\m -> measId m == mid) (senMeasurements sensor) of
        Just meas -> return meas
        Nothing -> do 
          warn "Measurement not found"
          throwError err404 {errBody = "Measurement not found"}
    Nothing -> do
      err "Error, sensor does not have a Keycloak ID"
      throwError err500 {errBody = "Not authorized"}

deleteMeasurement :: Maybe Token -> SensorId -> MeasId -> Waziup NoContent
deleteMeasurement tok (SensorId sid) (MeasId mid) = do
  info "Delete measurement"
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) tok
    debug "Permission granted, deleting measurement"
    runOrion $ O.deleteAttribute (EntityId sid) (AttributeId mid)
  return NoContent

putMeasName :: Maybe Token -> SensorId -> MeasId -> MeasName -> Waziup NoContent
putMeasName mtok sid@(SensorId sidt) mid name = do
  info $ "Put meas name: " ++ (show name)
  updateMeasField mtok sid mid $ \meas -> do 
    runOrion $ O.postAttribute (EntityId sidt) $ getAttFromMeas (meas {measName = Just name})

putMeasSensorKind :: Maybe Token -> SensorId -> MeasId -> SensorKindId -> Waziup NoContent
putMeasSensorKind mtok sid@(SensorId sidt) mid sk = do
  info $ "Put meas sensor kind: " ++ (show sk)
  updateMeasField mtok sid mid $ \meas -> do 
    runOrion $ O.postAttribute (EntityId sidt) $ getAttFromMeas (meas {measSensorKind = Just sk})

putMeasQuantityKind :: Maybe Token -> SensorId -> MeasId -> QuantityKindId -> Waziup NoContent
putMeasQuantityKind mtok sid@(SensorId sidt) mid qk = do
  info $ "Put meas quantity kind: " ++ (show qk)
  updateMeasField mtok sid mid $ \meas -> do 
    runOrion $ O.postAttribute (EntityId sidt) $ getAttFromMeas (meas {measQuantityKind = Just qk})

putMeasUnit :: Maybe Token -> SensorId -> MeasId -> UnitId -> Waziup NoContent
putMeasUnit mtok sid@(SensorId sidt) mid u = do
  info $ "Put meas unit: " ++ (show u)
  updateMeasField mtok sid mid $ \meas -> do 
    runOrion $ O.postAttribute (EntityId sidt) $ getAttFromMeas (meas {measUnit = Just u})

putMeasValue :: Maybe Token -> SensorId -> MeasId -> MeasurementValue -> Waziup NoContent
putMeasValue mtok sid@(SensorId sidt) mid measVal = do
  info $ "Put meas value: " ++ (show measVal)
  updateMeasField mtok sid mid $ \meas -> do 
    runOrion $ O.postAttribute (EntityId sidt) $ getAttFromMeas (meas {measLastValue = Just measVal})
    runMongo $ M.postDatapoint $ Datapoint sid mid measVal
  
updateMeasField :: Maybe Token -> SensorId -> MeasId -> (Measurement -> Waziup ()) -> Waziup NoContent
updateMeasField mtok (SensorId sid) mid w = do
  sensor <- getSensorFromEntity <$> runOrion (O.getEntity $ EntityId sid)
  case (senKeycloakId sensor) of
    Just keyId -> do
      debug "Check permissions"
      runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) mtok
      debug "Permission granted, updating measurement"
      case L.find (\m -> (measId m) == mid) (senMeasurements sensor) of
        Just meas -> w meas
        Nothing -> do 
          warn "Measurement not found"
          throwError err404 {errBody = "Measurement not found"}
    Nothing -> do
      err "Error, sensor does not have a Keycloak ID"
      throwError err500 {errBody = "Not authorized"}
  return NoContent

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Sensors" s
info  s = liftIO $ infoM    "Sensors" s
warn  s = liftIO $ warningM "Sensors" s
err   s = liftIO $ errorM   "Sensors" s

