{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Measurements where

import           Waziup.Types
import           Waziup.Utils
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Data.Maybe
import           Data.Text hiding (map, filter, foldl, any)
import           Data.String.Conversions
import qualified Data.List as L
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import qualified Orion as O
import           System.Log.Logger
import           Paths_Waziup_Servant


getMeasurements :: Maybe Token -> SensorId -> Waziup [Measurement]
getMeasurements tok sid = do
  info "Get measurements"
  sensor <- runOrion (O.getSensorOrion sid)
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
postMeasurement tok sid meas = do
  info $ "Post measurement: " ++ (show meas)
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) tok
    debug "Permission granted, creating measurement"
    runOrion $ O.postMeasurementOrion sid meas 
  return NoContent
 
getMeasurement :: Maybe Token -> SensorId -> MeasId -> Waziup Measurement
getMeasurement tok sid mid = do
  info "Get measurement"
  sensor <- runOrion (O.getSensorOrion sid)
  case (senKeycloakId sensor) of
    Just keyId -> do
      debug "Check permissions"
      runKeycloak $ checkPermission keyId (pack $ show SensorsView) tok
      debug "Permission granted, returning measurement"
      case L.find (\m -> (measId m) == mid) (senMeasurements sensor) of
        Just meas -> return meas
        Nothing -> do 
          warn "Measurement not found"
          throwError err404 {errBody = "Measurement not found"}
    Nothing -> do
      err "Error, sensor does not have a Keycloak ID"
      throwError err500 {errBody = "Not authorized"}

deleteMeasurement :: Maybe Token -> SensorId -> MeasId -> Waziup NoContent
deleteMeasurement tok sid mid = do
  info "Delete measurement"
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) tok
    debug "Permission granted, deleting measurement"
    runOrion $ O.deleteMeasurementOrion sid mid 
  return NoContent

putMeasName :: Maybe Token -> SensorId -> MeasId -> MeasName -> Waziup NoContent
putMeasName mtok sid mid name = do
  info $ "Put meas name: " ++ (show name)
  sensor <- runOrion (O.getSensorOrion sid)
  case (senKeycloakId sensor) of
    Just keyId -> do
      debug "Check permissions"
      runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) mtok
      debug "Permission granted, updating measurement"
      case L.find (\m -> (measId m) == mid) (senMeasurements sensor) of
        Just meas -> do
          let meas' = meas {measName = Just name}
          runOrion $ O.postMeasurementOrion sid meas' 
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

