{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Projects where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Auth hiding (info, warn, debug, err) 
import           Waziup.Devices hiding (info, warn, debug, err) 
import           Waziup.Gateways hiding (info, warn, debug, err) 
import           Keycloak as KC (Token, Username, getUsername) 
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.String.Conversions
import           Data.Either
import           Data.Maybe
import           Servant
import           System.Log.Logger
import           Database.MongoDB as DB
import           Data.Aeson as JSON
import           Data.AesonBson
import           Data.Text hiding (find, map, filter, any)
import qualified Data.List as L

-- * Projects API

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsProjects :: AuthUser -> Waziup [Perm]
getPermsProjects tok = do
  info "Get projects permissions"
  projects <- getAllProjects
  let perms = map (\p -> getPerm tok (PermProject p) projectScopes) projects
  return $ filter (\(Perm _ scps) -> not $ L.null scps) perms

getProjects :: AuthUser -> Maybe Bool -> Waziup [Project]
getProjects tok mfull = do
  info "Get projects"
  projects <- getAllProjects
  let projects2 = filter (\p -> isNothing $ isPermitted tok (PermProject p) ProjectsView) projects -- TODO limits
  projects3 <- case mfull of
    Just True -> mapM (getFullProject tok) projects2
    _ -> return projects2
  return projects3

getAllProjects :: Waziup [Project]
getAllProjects = do
  runMongo $ do
    docs <- rest =<< find (select [] "projects")
    return $ catMaybes $ map (resultToMaybe . fromJSON . Object . replaceKey "_id" "id" .  aesonify) docs

postProject :: AuthUser -> Project -> Waziup ProjectId
postProject au proj = do
  info "Post project"
  let proj' = proj {pOwner = Just $ userUsername $ getAuthUser au}
  res <- runMongo $ do
    let ob = case toJSON $ proj' of
         JSON.Object o -> o
         _ -> error "Wrong object format"
    insert "projects" (bsonifyBound ob)
  return $ ProjectId $ convertString $ show res


getProject :: AuthUser -> ProjectId -> Maybe Bool -> Waziup Project
getProject tok pid mfull = do
  info "Get project"
  mp <- runMongo $ getProjectMongo pid 
  p <- case mp of
    Just p -> return p
    Nothing -> throwError err404 {errBody = "Cannot get project: id not found"}
  debug $ "Check permissions"
  checkPermResource tok ProjectsView (PermProject p) 
  case mfull of
    Just True -> getFullProject tok p 
    _ -> return p

deleteProject :: AuthUser -> ProjectId -> Waziup NoContent
deleteProject tok pid = do
  info "Delete project"
  p <- getProject tok pid Nothing
  checkPermResource tok ProjectsDelete (PermProject p)
  res <- runMongo $ deleteProjectMongo pid
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot delete project: id not found"}

putProjectDevices :: AuthUser -> ProjectId -> [DeviceId] -> Waziup NoContent
putProjectDevices tok pid ids = do
  info "Put project devices"
  p <- getProject tok pid Nothing
  checkPermResource tok ProjectsUpdate (PermProject p)
  res <- runMongo $ putProjectDevicesMongo pid ids
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot update project: id not found"}

putProjectGateways :: AuthUser -> ProjectId -> [GatewayId] -> Waziup NoContent
putProjectGateways tok pid ids = do
  info "Put project gateways"
  p <- getProject tok pid Nothing
  checkPermResource tok ProjectsUpdate (PermProject p) 
  res <- runMongo $ putProjectGatewaysMongo pid ids
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot update project: id not found"}

putProjectName :: AuthUser -> ProjectId -> Text -> Waziup NoContent
putProjectName tok pid name = do
  info "Put project name"
  p <- getProject tok pid Nothing
  checkPermResource tok ProjectsUpdate (PermProject p)
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

getFullProject :: AuthUser -> Project -> Waziup Project
getFullProject tok p@(Project _ _ _ devids gtwids _ _) = do
  devs <- mapM (try . getDevice tok) (maybe [] id devids)
  gtwids' <- mapM (try . (\gid -> getGateway tok gid Nothing)) (maybe [] id gtwids)
  return $ p {pDevices = Just $ rights devs, pGateways = Just $ rights gtwids'}

getProjectMongo :: ProjectId -> Action IO (Maybe Project)
getProjectMongo (ProjectId pid) = do
  mdoc <- findOne (select ["_id" =: (ObjId $ read $ convertString pid)] "projects")
  case (fromJSON . Object . replaceKey "_id" "id" . aesonify <$> mdoc) of
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
       modify (select sel "projects") [ "$set" := Doc ["gateway_ids" := val (map unGatewayId gids)]]
       return True
     _ -> return False 


putProjectDevicesMongo :: ProjectId -> [DeviceId] -> Action IO Bool
putProjectDevicesMongo (ProjectId pid) ids = do
  let sel = ["_id" =: (ObjId $ read $ convertString $ pid)]
  mdoc <- findOne (select sel "projects")
  case mdoc of
     Just _ -> do
       modify (select sel "projects") [ "$set" := Doc ["device_ids" := (val $ map unDeviceId ids)]]
       return True
     _ -> return False 

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Project" s
info  s = liftIO $ infoM    "Project" s
warn  s = liftIO $ warningM "Project" s
err   s = liftIO $ errorM   "Project" s

