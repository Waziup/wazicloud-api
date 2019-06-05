{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.SensorData where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Devices hiding (info, warn, debug, err)
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Data.Maybe
import           Data.Text hiding (map, filter, foldl, any, find)
import           Data.String.Conversions
import qualified Data.List as L
import           Data.Aeson as JSON
import           Data.AesonBson
import           Data.Time
import           Data.Time.ISO8601
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           Orion as O hiding (info, warn, debug, err)
import           System.Log.Logger
import           Database.MongoDB as DB hiding (value)
import           Safe
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)


getDatapoints :: Maybe Token
              -> Maybe DeviceId
              -> Maybe SensorId
              -> Maybe Int
              -> Maybe Int
              -> Maybe Sort
              -> Maybe UTCTime
              -> Maybe UTCTime
              -> Maybe Bool
              -> Waziup [Datapoint]
getDatapoints tok did sid limit offset sort dateFrom dateTo calibEn = do
  info "Get datapoints"
  withKCId (fromJustNote "parameter sensor_id is mandatory" did) $ \(keyId, device) -> do
    debug "Check permissions"
    liftKeycloak tok $ checkPermission keyId (fromScope DevicesDataView)
    debug "Permission granted, returning datapoints"
    res <- runMongo $ getDatapointsMongo did sid limit offset sort dateFrom dateTo
    let calib = if (isJust sid) 
        then case L.find (\s -> (senId s) == fromJust sid) (maybeToList' $ devSensors device) of
           Just sen -> senCalib sen
           Nothing -> Nothing
        else Nothing
    if calibEn == Just False -- by default, values are calibrated
       then return res -- return raw values
       else return $ map (calibrateDatapoint calib) res -- return calibrated values
    
getDatapointsMongo :: Maybe DeviceId 
                   -> Maybe SensorId
                   -> Maybe Int
                   -> Maybe Int
                   -> Maybe Sort
                   -> Maybe UTCTime
                   -> Maybe UTCTime
                   -> Action IO [Datapoint]
getDatapointsMongo did sid limit offset sort dateFrom dateTo = do
  info "Get datapoints from Mongo"
  let filterDev = ["device_id" =: did' | (Just (DeviceId did')) <- [did]]
  let filterSen = ["sensor_id" =: sid' | (Just (SensorId sid')) <- [sid]]
  --filter creation date by creating fake ObjectIds
  let filterDateFrom = ["$gte" =: Oid (truncate $ utcTimeToPOSIXSeconds dateFrom') 0 | (Just dateFrom') <- [dateFrom]]
  let filterDateTo   = ["$lte" =: Oid (truncate $ utcTimeToPOSIXSeconds dateTo') 0 | (Just dateTo') <- [dateTo]]
  let filterTimestamp = if isJust dateFrom || isJust dateTo 
                        then ["_id" =: filterDateFrom <> filterDateTo] 
                        else []
  let filters = filterDev <> filterSen <> filterTimestamp
  let sort' = case sort of
               Just Asc -> 1
               Just Dsc -> -1
               Nothing  -> 1 :: Int
  let limit' = if isJust limit  then fromJust limit else 20
  let skip'  = if isJust offset then fromJust offset else 0
  let sel = (select filters "waziup_history") {sort  = ["_id" := val sort'],
                                               limit = fromIntegral limit',
                                               skip  = fromIntegral skip'}
  debug $ show sel
  cur <- find sel
  docs <- rest cur
  debug $ "Got docs:" <> (show docs)
  let res = filter isSuccess $ map (fromJSON . Object . aesonify) docs
  debug $ "Got datapoints:" <> (show res)
  case (sequence res) of
    JSON.Success a -> return a
    JSON.Error e -> do
      err $ "Error from Mongo:" ++ (show e)
      return []

isSuccess :: Result a -> Bool
isSuccess (JSON.Success _) = True
isSuccess (JSON.Error _) = False

calibrateDatapoint :: Maybe Calib -> Datapoint -> Datapoint
calibrateDatapoint cal d@(Datapoint _ _ val _ _) = d {dataValue = getCalibratedValue val cal}


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "SensorData" s
info  s = liftIO $ infoM    "SensorData" s
warn  s = liftIO $ warningM "SensorData" s
err   s = liftIO $ errorM   "SensorData" s

