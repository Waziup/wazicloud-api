{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Projects where

import           Waziup.Types
import           Waziup.API
import           Waziup.Utils
import           Waziup.Auth hiding (info, warn, debug, err) 
import           Waziup.Devices hiding (info, warn, debug, err) 
import           Waziup.Gateways hiding (info, warn, debug, err) 
import           Keycloak as KC hiding (info, warn, debug, err, Scope, try) 
import qualified Keycloak as K (Scope(..)) 
import           Control.Monad.Except (throwError, catchError, MonadError)
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Monad.Extra
import           Data.String.Conversions
import           Data.Either
import           Data.Maybe
import           Servant
import           System.Log.Logger
import           Database.MongoDB as DB
import           Data.Aeson as JSON
import           Data.Bson as BSON
import           Data.AesonBson
import           Data.Text hiding (find, map, filter, any)
import           Safe

-- * Projects API

getProjects :: Maybe Token -> Maybe Bool -> Waziup [Project]
getProjects tok mfull = do
  info "Get projects"
  projects <- runMongo $ do
    docs <- rest =<< find (select [] "projects")
    info $ "Got projects docs: " ++ (show docs)
    let res = sequence $ map (fromJSON . Object . aesonify) docs
    case res of
      JSON.Success a -> return a
      JSON.Error _ -> return []
  info $ "Got projects: " ++ (show projects)
  ps <- getPermsProjects tok
  let projects2 = filter (checkPermResource' ProjectsView ps . unProjectId . fromJust . pId) projects -- TODO limits
  projects3 <- case mfull of
    Just True -> mapM (getFullProject tok) projects2
    _ -> return projects2
  return projects3

postProject :: Maybe Token -> Project -> Waziup ProjectId
postProject tok proj = do
  info "Post project"
  let username = case tok of
       Just t -> getUsername t
       Nothing -> "guest"
  let proj' = proj {pOwner = Just username}
  res <- runMongo $ do
    let ob = case toJSON $ proj' of
         JSON.Object o -> o
         _ -> error "Wrong object format"
    insert "projects" (bsonify ob)
  createResource' tok
                  (Just $ ResourceId $ convertString $ "project-" <> (show res))
                  (convertString $ show res)
                  "Project"
                  [ProjectsView, ProjectsUpdate, ProjectsDelete] [] 
  return $ ProjectId $ convertString $ show res


getProject :: Maybe Token -> ProjectId -> Maybe Bool -> Waziup Project
getProject tok pid mfull = do
  info "Get project"
  mp <- runMongo $ getProjectMongo pid 
  p <- case mp of
    Just p -> return p
    Nothing -> throwError err404 {errBody = "Cannot get project: id not found"}
  debug $ "Check permissions"
  checkPermResource tok ProjectsView (unProjectId $ fromJust $ pId p) 
  case mfull of
    Just True -> getFullProject tok p 
    _ -> return p

deleteProject :: Maybe Token -> ProjectId -> Waziup NoContent
deleteProject tok pid = do
  info "Delete project"
  checkPermResource tok ProjectsDelete (unProjectId pid)
  liftKeycloak tok $ deleteResource (ResourceId $ "project-" <> unProjectId pid)
  res <- runMongo $ deleteProjectMongo pid
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot delete project: id not found"}

putProjectDevices :: Maybe Token -> ProjectId -> [DeviceId] -> Waziup NoContent
putProjectDevices tok pid ids = do
  info "Put project devices"
  checkPermResource tok ProjectsUpdate (unProjectId pid)
  res <- runMongo $ putProjectDevicesMongo pid ids
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot update project: id not found"}

putProjectGateways :: Maybe Token -> ProjectId -> [GatewayId] -> Waziup NoContent
putProjectGateways tok pid ids = do
  info "Put project gateways"
  checkPermResource tok ProjectsUpdate (unProjectId pid) 
  res <- runMongo $ putProjectGatewaysMongo pid ids
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot update project: id not found"}

putProjectName :: Maybe Token -> ProjectId -> Text -> Waziup NoContent
putProjectName tok pid name = do
  info "Put project name"
  checkPermResource tok ProjectsUpdate (unProjectId pid)
  res <- runMongo $ do 
    let sel = ["_id" =: (ObjId $ read $ convertString $ unProjectId pid)]
    mdoc <- findOne (select sel "projects")
    case mdoc of
       Just _ -> do
         modify (select sel "projects") [ "$set" := Doc ["name" := val name]]
         return True
       _ -> return False 
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot update project: id not found"}

-- * Helpers

getFullProject :: Maybe Token -> Project -> Waziup Project
getFullProject tok p@(Project _ _ _ (Left devids) (Left gtwids)) = do
  devs <- mapM (try . getDevice tok) devids
  gtwids <- mapM (try . (\id -> getGateway tok id Nothing)) gtwids
  return $ p {pDevices = Right $ rights devs, pGateways = Right $ rights gtwids}

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

