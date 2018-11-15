{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mongo.Client where

import Waziup.Types
import Mongo.Types
import Database.MongoDB as DB
import Data.Aeson as JSON
import Data.Bson as BSON
import Data.AesonBson
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import System.Log.Logger
import Data.String.Conversions

getProjectsMongo :: Action IO [Project]
getProjectsMongo = do
  docs <- rest =<< find (select [] "projects")
  let res = sequence $ map (fromJSON . Object . aesonify) docs
  case res of
    JSON.Success a -> return a
    JSON.Error _ -> return []
    

postProjectMongo :: Project -> Action IO ProjectId
postProjectMongo p = do
  let ob = case toJSON p of
       JSON.Object o -> o
       _ -> error "Wrong object format"
  res <- insert "projects" (bsonify ob)
  return $ convertString $ show res

getProjectMongo :: ProjectId -> Action IO (Maybe Project)
getProjectMongo pid = do
  mdoc <- findOne (select ["_id" =: (ObjId $ read $ convertString pid)] "projects")
  case (fromJSON . Object . aesonify <$> mdoc) of
     Just (JSON.Success a) -> return $ Just a
     _ -> return Nothing

deleteProjectMongo :: ProjectId -> Action IO Bool 
deleteProjectMongo pid = do
  let sel = ["_id" =: (ObjId $ read $ convertString pid)]
  mdoc <- findOne (select sel "projects")
  case mdoc of
     Just _ -> do
       delete (select sel "projects")
       return True
     _ -> return False 

putProjectGatewaysMongo :: ProjectId -> [GatewayId] -> Action IO Bool
putProjectGatewaysMongo pid ids = do
  let sel = ["_id" =: (ObjId $ read $ convertString pid)]
  mdoc <- findOne (select sel "projects")
  case mdoc of
     Just _ -> do
       modify (select sel "projects") [ "$set" := Doc ["gateways" := val ids]]
       return True
     _ -> return False 

putProjectDevicesMongo :: ProjectId -> [DeviceId] -> Action IO Bool
putProjectDevicesMongo pid ids = do
  let sel = ["_id" =: (ObjId $ read $ convertString pid)]
  mdoc <- findOne (select sel "projects")
  case mdoc of
     Just _ -> do
       modify (select sel "projects") ["devices" := (val ids)]
       return True
     _ -> return False 

debug, warn, info, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "Mongo" s
info s  = liftIO $ infoM "Mongo" s
warn s  = liftIO $ warningM "Mongo" s
err s   = liftIO $ errorM "Mongo" s