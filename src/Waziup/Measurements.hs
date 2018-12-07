{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Measurements where

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


getMeasurements :: Maybe Token -> DeviceId -> Waziup [Measurement]
getMeasurements tok (DeviceId did) = do
  info "Get measurements"
  device <- getDeviceFromEntity <$> runOrion (O.getEntity $ EntityId did)
  case (devKeycloakId device) of
    Just keyId -> do
      debug "Check permissions"
      runKeycloak $ checkPermission keyId (pack $ show DevicesView) tok
      debug "Permission granted, returning measurements"
      return $ devMeasurements device
    Nothing -> do
      err "Error, device does not have a Keycloak ID"
      throwError err500 {errBody = "Not authorized"}

postMeasurement :: Maybe Token -> DeviceId -> Measurement -> Waziup NoContent
postMeasurement tok did meas = do
  info $ "Post measurement: " ++ (show meas)
  withKCId did $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show DevicesUpdate) tok
    debug "Permission granted, creating measurement"
    let att = getAttFromMeas meas
    runOrion $ O.postAttribute (toEntityId did) att 
  return NoContent
 
getMeasurement :: Maybe Token -> DeviceId -> MeasId -> Waziup Measurement
getMeasurement tok (DeviceId did) mid = do
  info "Get measurement"
  device <- getDeviceFromEntity <$> runOrion (O.getEntity $ EntityId did)
  case (devKeycloakId device) of
    Just keyId -> do
      debug "Check permissions"
      runKeycloak $ checkPermission keyId (pack $ show DevicesView) tok
      debug "Permission granted, returning measurement"
      case L.find (\m -> measId m == mid) (devMeasurements device) of
        Just meas -> return meas
        Nothing -> do 
          warn "Measurement not found"
          throwError err404 {errBody = "Measurement not found"}
    Nothing -> do
      err "Error, device does not have a Keycloak ID"
      throwError err500 {errBody = "Not authorized"}

deleteMeasurement :: Maybe Token -> DeviceId -> MeasId -> Waziup NoContent
deleteMeasurement tok did (MeasId mid) = do
  info "Delete measurement"
  withKCId did $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show DevicesUpdate) tok
    debug "Permission granted, deleting measurement"
    runOrion $ O.deleteAttribute (toEntityId did) (AttributeId mid)
  return NoContent

putMeasName :: Maybe Token -> DeviceId -> MeasId -> MeasName -> Waziup NoContent
putMeasName mtok did mid name = do
  info $ "Put meas name: " ++ (show name)
  updateMeasField mtok did mid $ \meas -> do 
    runOrion $ O.postAttribute (toEntityId did) $ getAttFromMeas (meas {measName = Just name})

putMeasSensorKind :: Maybe Token -> DeviceId -> MeasId -> SensorKindId -> Waziup NoContent
putMeasSensorKind mtok did@(DeviceId didt) mid sk = do
  info $ "Put meas sensor kind: " ++ (show sk)
  updateMeasField mtok did mid $ \meas -> do 
    runOrion $ O.postAttribute (EntityId didt) $ getAttFromMeas (meas {measSensorKind = Just sk})

putMeasQuantityKind :: Maybe Token -> DeviceId -> MeasId -> QuantityKindId -> Waziup NoContent
putMeasQuantityKind mtok did mid qk = do
  info $ "Put meas quantity kind: " ++ (show qk)
  updateMeasField mtok did mid $ \meas -> do 
    runOrion $ O.postAttribute (toEntityId did) $ getAttFromMeas (meas {measQuantityKind = Just qk})

putMeasUnit :: Maybe Token -> DeviceId -> MeasId -> UnitId -> Waziup NoContent
putMeasUnit mtok did mid u = do
  info $ "Put meas unit: " ++ (show u)
  updateMeasField mtok did mid $ \meas -> do 
    runOrion $ O.postAttribute (toEntityId did) $ getAttFromMeas (meas {measUnit = Just u})

putMeasValue :: Maybe Token -> DeviceId -> MeasId -> MeasurementValue -> Waziup NoContent
putMeasValue mtok did mid measVal = do
  info $ "Put meas value: " ++ (show measVal)
  updateMeasField mtok did mid $ \meas -> do 
    runOrion $ O.postAttribute (toEntityId did) $ getAttFromMeas (meas {measLastValue = Just measVal})
    runMongo $ M.postDatapoint $ Datapoint did mid measVal
  
updateMeasField :: Maybe Token -> DeviceId -> MeasId -> (Measurement -> Waziup ()) -> Waziup NoContent
updateMeasField mtok did mid w = do
  device <- getDeviceFromEntity <$> runOrion (O.getEntity $ toEntityId did)
  case (devKeycloakId device) of
    Just keyId -> do
      debug "Check permissions"
      runKeycloak $ checkPermission keyId (pack $ show DevicesUpdate) mtok
      debug "Permission granted, updating measurement"
      case L.find (\m -> (measId m) == mid) (devMeasurements device) of
        Just meas -> w meas
        Nothing -> do 
          warn "Measurement not found"
          throwError err404 {errBody = "Measurement not found"}
    Nothing -> do
      err "Error, device does not have a Keycloak ID"
      throwError err500 {errBody = "Not authorized"}
  return NoContent

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Sensors" s
info  s = liftIO $ infoM    "Sensors" s
warn  s = liftIO $ warningM "Sensors" s
err   s = liftIO $ errorM   "Sensors" s

