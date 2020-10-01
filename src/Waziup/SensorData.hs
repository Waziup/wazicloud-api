{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.SensorData where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Auth hiding (info, warn, debug, err)
import           Waziup.Devices hiding (info, warn, debug, err)
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.List as L
import           Data.Aeson as JSON
import           Data.AesonBson
import           Data.Time
import           Servant
import           Keycloak as KC hiding (Scope) 
import           System.Log.Logger
import           Database.MongoDB as DB hiding (value)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)


getDatapoints :: Maybe Token
              -> Maybe [DeviceId]
              -> Maybe [SensorId]
              -> Maybe Int
              -> Maybe Int
              -> Maybe Sort
              -> Maybe UTCTime
              -> Maybe UTCTime
              -> Maybe Bool
              -> Waziup [Datapoint]
getDatapoints tok mdids msids lim offset srt dateFrom dateTo calibEn = do
  info "Get datapoints"
  dids <- case mdids of
    Nothing -> throwError err400 {errBody = "parameters 'devices' must be present"}
    Just dids -> return dids
  devs <- mapM (getDevice tok) dids
  let dids' = map devId $ filter (\d -> isNothing $ isPermitted tok (PermDevice d) DevicesDataView) devs
  res <- runMongo $ getDatapointsMongo dids' msids lim offset srt dateFrom dateTo
  res' <- if calibEn == Just False then return res else calibrateDatapoints tok dids' res 
  return res'

getDatapointsMongo :: [DeviceId]
                   -> Maybe [SensorId]
                   -> Maybe Int
                   -> Maybe Int
                   -> Maybe Sort
                   -> Maybe UTCTime
                   -> Maybe UTCTime
                   -> Action IO [Datapoint]
getDatapointsMongo dids msids lim offset srt dateFrom dateTo = do
  info $ "Get datapoints from Mongo"
  let filterDev = ["device_id" =: ["$in" =: map unDeviceId dids]]
  let filterSen = case msids of
         Nothing -> []
         Just sids -> ["sensor_id" =: ["$in" =: map unSensorId sids]]

  --filter creation date by creating fake ObjectIds
  let filterDateFrom = ["$gte" =: Oid (truncate $ utcTimeToPOSIXSeconds dateFrom') 0 | (Just dateFrom') <- [dateFrom]]
  let filterDateTo   = ["$lte" =: Oid (truncate $ utcTimeToPOSIXSeconds dateTo') 0 | (Just dateTo') <- [dateTo]]
  let filterTimestamp = if isJust dateFrom || isJust dateTo 
                        then ["_id" =: filterDateFrom <> filterDateTo] 
                        else []
  let filters = filterDev <> filterSen <> filterTimestamp
  let sort' = case srt of
               Just Asc -> 1
               Just Dsc -> -1
               Nothing  -> 1 :: Int
  let limit' = if isJust lim  then fromJust lim else 20
  let skip'  = if isJust offset then fromJust offset else 0
  debug $ show filterSen
  debug $ show filterDev

  let sel = (select filters "waziup_history") {sort  = ["_id" := val sort'],
                                               limit = fromIntegral limit',
                                               skip  = fromIntegral skip'}
  debug $ show sel
  cur <- find sel
  docs <- rest cur
  return $ catMaybes $ map (resultToMaybe . fromJSON . Object . aesonify) docs

calibrateDatapoints :: Maybe Token -> [DeviceId] -> [Datapoint] -> Waziup [Datapoint]
calibrateDatapoints tok dids ds = do
  devices <- mapM (getDevice tok) dids
  return $ map (calibrateDatapoint devices) ds

calibrateDatapoint :: [Device] -> Datapoint -> Datapoint
calibrateDatapoint devs d@(Datapoint _ _ value _ _) = calib (getCalib devs d) where
  calib :: Maybe Calib -> Datapoint
  calib cal = d {dataValue = getCalibratedValue value cal}

getCalib :: [Device] -> Datapoint -> Maybe Calib
getCalib devs (Datapoint did sid _ _ _) = do
  device <- L.find (\d -> (devId d) == did) devs
  sensor <- L.find (\s -> (senId s) == sid) (maybeToList' $ devSensors device)
  calib <- senCalib sensor
  return calib

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "SensorData" s
info  s = liftIO $ infoM    "SensorData" s
warn  s = liftIO $ warningM "SensorData" s
err   s = liftIO $ errorM   "SensorData" s

