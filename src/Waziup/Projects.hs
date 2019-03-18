{-# LANGUAGE OverloadedStrings #-}

module Waziup.Projects where

import           Waziup.Types
import           Waziup.API
import           Waziup.Utils
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Data.String.Conversions
import           Servant
import           System.Log.Logger
import           Database.MongoDB as DB
import           Data.Aeson as JSON
import           Data.Bson as BSON
import           Data.AesonBson

-- * Projects API

getProjects :: Maybe Token -> Waziup [Project]
getProjects tok = do
  info "Get projects"
  runMongo $ do
    docs <- rest =<< find (select [] "projects")
    let res = sequence $ map (fromJSON . Object . aesonify) docs
    case res of
      JSON.Success a -> return a
      JSON.Error _ -> return []

postProject :: Maybe Token -> Project -> Waziup ProjectId
postProject tok p = do
  info "Post project"
  runMongo $ do
    let ob = case toJSON p of
         JSON.Object o -> o
         _ -> error "Wrong object format"
    res <- insert "projects" (bsonify ob)
    return $ ProjectId $ convertString $ show res


getProject :: Maybe Token -> ProjectId -> Waziup Project
getProject tok pid = do
  info "Get project"
  mp <- runMongo $ getProjectMongo pid 
  case mp of
    Just p -> return p
    Nothing -> throwError err404 {errBody = "Cannot get project: id not found"}

deleteProject :: Maybe Token -> ProjectId -> Waziup NoContent
deleteProject tok pid = do
  info "Delete project"
  res <- runMongo $ deleteProjectMongo pid
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot delete project: id not found"}

putProjectDevices :: Maybe Token -> ProjectId -> [DeviceId] -> Waziup NoContent
putProjectDevices tok pid ids = do
  info "Put project devices"
  res <- runMongo $ putProjectDevicesMongo pid ids
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot update project: id not found"}

putProjectGateways :: Maybe Token -> ProjectId -> [GatewayId] -> Waziup NoContent
putProjectGateways tok pid ids = do
  info "Put project gateways"
  res <- runMongo $ putProjectGatewaysMongo pid ids
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot update project: id not found"}

-- * Helpers

getProjectMongo :: ProjectId -> Action IO (Maybe Project)
getProjectMongo (ProjectId pid) = do
  mdoc <- findOne (select ["_id" =: (ObjId $ read $ convertString pid)] "projects")
  case (fromJSON . Object . aesonify <$> mdoc) of
     Just (JSON.Success a) -> return $ Just a
     _ -> return Nothing

deleteProjectMongo :: ProjectId -> Action IO Bool 
deleteProjectMongo (ProjectId pid) = do
  let sel = ["_id" =: (ObjId $ read $ convertString pid)]
  mdoc <- findOne (select sel "projects")
  case mdoc of
     Just _ -> do
       delete (select sel "projects")
       return True
     _ -> return False 

putProjectGatewaysMongo :: ProjectId -> [GatewayId] -> Action IO Bool
putProjectGatewaysMongo (ProjectId pid) gids = do
  let sel = ["_id" =: (ObjId $ read $ convertString pid)]
  mdoc <- findOne (select sel "projects")
  case mdoc of
     Just _ -> do
       modify (select sel "projects") [ "$set" := Doc ["gateways" := val (map unGatewayId gids)]]
       return True
     _ -> return False 


putProjectDevicesMongo :: ProjectId -> [DeviceId] -> Action IO Bool
putProjectDevicesMongo (ProjectId pid) ids = do
  let sel = ["_id" =: (ObjId $ read $ convertString $ pid)]
  mdoc <- findOne (select sel "projects")
  case mdoc of
     Just _ -> do
       modify (select sel "projects") [ "$set" := Doc ["devices" := (val $ map unDeviceId ids)]]
       return True
     _ -> return False 


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Project" s
info  s = liftIO $ infoM    "Project" s
warn  s = liftIO $ warningM "Project" s
err   s = liftIO $ errorM   "Project" s

