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


getDatapoints :: Maybe Token
              -> Maybe DeviceId
              -> Maybe SensorId
              -> Maybe Int
              -> Maybe Int
              -> Maybe Sort
              -> Maybe UTCTime
              -> Maybe UTCTime
              -> Waziup [Datapoint]
getDatapoints tok did sid limit offset sort dateFrom dateTo = do
  info "Get datapoints"
  withKCId (fromJust did) $ \(keyId, _) -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show DevicesDataView) tok
    debug "Permission granted, returning datapoints"
    runMongo $ getDatapointsMongo did sid limit offset sort dateFrom dateTo
    
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
  let filterDateFrom = ["$gte" =: formatISO8601 dateFrom' | (Just dateFrom') <- [dateFrom]]
  let filterDateTo   = ["$lte" =: formatISO8601 dateTo' | (Just dateTo') <- [dateTo]]
  let filterTimestamp = if isJust dateFrom || isJust dateTo 
                        then ["timestamp" =: filterDateFrom <> filterDateTo] 
                        else []
  let filters = filterDev <> filterSen <> filterTimestamp
  let sort' = case sort of
               Just Asc -> 1
               Just Dsc -> -1
               Nothing  -> 1 :: Int
  let limit' = if isJust limit  then fromJust limit else 20
  let skip'  = if isJust offset then fromJust offset else 0
  let sel = (select filters "waziup_history") {sort  = ["timestamp" := val sort'],
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

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "SensorData" s
info  s = liftIO $ infoM    "SensorData" s
warn  s = liftIO $ warningM "SensorData" s
err   s = liftIO $ errorM   "SensorData" s

