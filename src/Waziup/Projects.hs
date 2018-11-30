{-# LANGUAGE OverloadedStrings #-}

module Waziup.Projects where

import           Waziup.Types
import           Waziup.API
import           Waziup.Utils
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           Mongo.Client as M hiding (info, warn, debug, err, Scope) 
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Servant
import           System.Log.Logger

-- * Projects

getProjects :: Maybe Token -> Waziup [Project]
getProjects tok = do
  info "Get projects"
  projects <- runMongo $ M.getProjectsMongo
  return projects -- TODO filter

postProject :: Maybe Token -> Project -> Waziup ProjectId
postProject tok p = do
  info "Post project"
  pid <- runMongo $ M.postProjectMongo p
  return pid

getProject :: Maybe Token -> ProjectId -> Waziup Project
getProject tok pid = do
  info "Get project"
  mp <- runMongo $ M.getProjectMongo pid
  case mp of
    Just p -> return p
    Nothing -> throwError err404 {errBody = "Cannot get project: id not found"}

deleteProject :: Maybe Token -> ProjectId -> Waziup NoContent
deleteProject tok pid = do
  info "Delete project"
  res <- runMongo $ M.deleteProjectMongo pid
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot delete project: id not found"}

putProjectDevices :: Maybe Token -> ProjectId -> [DeviceId] -> Waziup NoContent
putProjectDevices tok pid ids = do
  info "Put project devices"
  res <- runMongo $ M.putProjectDevicesMongo pid ids
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot update project: id not found"}

putProjectGateways :: Maybe Token -> ProjectId -> [GatewayId] -> Waziup NoContent
putProjectGateways tok pid ids = do
  info "Put project gateways"
  res <- runMongo $ M.putProjectGatewaysMongo pid ids
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot update project: id not found"}


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "API" s
info  s = liftIO $ infoM "API" s
warn  s = liftIO $ warningM "API" s
err   s = liftIO $ errorM "API" s

