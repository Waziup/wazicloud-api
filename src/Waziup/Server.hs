{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Waziup.Server where

import Waziup.Types
import Waziup.API
import Waziup.Utils
import Control.Monad.Except (ExceptT, throwError, withExceptT, runExceptT)
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error.Class (MonadError)
import Data.Maybe
import Data.Proxy (Proxy(..))
import Data.Text hiding (map, filter, foldl, any)
import Servant
import Servant.Server
import Keycloak as KC hiding (info, warn, debug, Scope) 
import qualified Orion as O
import Mongo as M hiding (info, warn, debug, Scope) 
import Database.MongoDB as DB
import Control.Monad.Catch as C
import Servant.API.Flatten
import Network.HTTP.Client (HttpException)
import GHC.Generics (Generic)
import System.Log.Logger


server :: ServerT WaziupAPI Waziup
server = authServer :<|> sensorsServer :<|> projectsServer

authServer :: ServerT AuthAPI Waziup
authServer = getPerms :<|> postAuth

sensorsServer :: ServerT SensorsAPI Waziup
sensorsServer = getSensors :<|> postSensor :<|> getSensor :<|> deleteSensor

projectsServer :: ServerT ProjectsAPI Waziup
projectsServer = getProjects :<|> postProject :<|> getProject :<|> deleteProject 

getPerms :: Maybe Token -> Waziup [Perm]
getPerms tok = do
  info "Get Permissions"
  ps <- runKeycloak (getAllPermissions tok)
  let getP :: KC.Permission -> Perm
      getP (KC.Permission rsname _ scopes) = Perm rsname (mapMaybe readScope scopes)
  return $ map getP ps 

postAuth :: AuthBody -> Waziup Token
postAuth (AuthBody username password) = do
  info "Post authentication"
  tok <- runKeycloak (getUserAuthToken username password)
  return tok

getSensors :: Maybe Token -> Maybe SensorsQuery -> Maybe SensorsLimit -> Maybe SensorsOffset -> Waziup [Sensor]
getSensors tok mq mlimit moffset = do
  info "Get sensors"
  sensors <- runOrion $ O.getSensorsOrion mq mlimit moffset
  ps <- runKeycloak (getAllPermissions tok)
  let sensors2 = filter (\s -> any (\p -> (rsname p) == (senId s)) ps) sensors
  return sensors2

checkAuth :: SensorId -> Scope -> Maybe Token -> Waziup a -> Waziup a
checkAuth sid scope tok act = do
  isAuth <- runKeycloak (isAuthorized sid (pack $ show scope) tok)
  if isAuth 
    then act
    else throwError err403 {errBody = "Not authorized"}
  
getSensor :: Maybe Token -> SensorId -> Waziup Sensor
getSensor tok sid = do
  info "Get sensor"
  checkAuth sid SensorsView tok $ runOrion (O.getSensorOrion sid)

postSensor :: Maybe Token -> Sensor -> Waziup NoContent
postSensor tok s@(Sensor sid _ _ _ _ _ _ _ _ vis _) = do
  info $ "Post sensor" ++ (show tok)
  let res = KC.Resource {
     resId      = Nothing,
     resName    = sid,
     resType    = Nothing,
     resUris    = [],
     resScopes  = map (pack.show) [SensorsView, SensorsUpdate, SensorsDelete, SensorsDataCreate, SensorsDataView],
     resOwner   = Owner Nothing "cdupont",
     resOwnerManagedAccess = True,
     resAttributes = if (isJust vis) then [Attribute "visibility" [pack $ show $ fromJust vis]] else []}
  debug "Check permissions"
  runKeycloak $ checkPermission "Sensors" (pack $ show SensorsCreate) tok
  debug "Create resource"
  resId <- runKeycloak $ createResource res tok
  debug "Create entity"
  res2 <- C.try $ runOrion $ O.postSensorOrion (s {senKeycloakId = Just resId})
  case res2 of
    Right _ -> return NoContent
    Left (e :: HttpException) -> do
      warn "Orion error, deleting Keycloak resource"
      runKeycloak $ deleteResource resId tok
      return NoContent
 
deleteSensor :: Maybe Token -> SensorId -> Waziup NoContent
deleteSensor tok sid = do
  info "Delete sensor"
  debug "Check permissions"
  runKeycloak $ checkPermission sid (pack $ show SensorsDelete) tok
  debug "Delete Keycloak resource"
  sensor <- runOrion (O.getSensorOrion sid)
  case (senKeycloakId sensor) of
    Just keyId -> do
      runKeycloak $ deleteResource keyId tok
      runOrion $ O.deleteSensorOrion sid
    Nothing -> do
      error "Cannot delete sensor: KC Id not present"
      throwError err500 {errBody = "Cannot delete sensor: KC Id not present"}
  return NoContent

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

getProject :: Maybe Token -> ProjectId -> Waziup (Maybe Project)
getProject tok pid = do
  info "Get project"
  project <- runMongo $ M.getProjectMongo pid
  return project

deleteProject :: Maybe Token -> ProjectId -> Waziup NoContent
deleteProject tok pid = do
  info "Delete project"
  res <- runMongo $ M.deleteProjectMongo pid
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot delete project: id not found"}

-- * Server

waziupAPI :: Proxy WaziupAPI
waziupAPI = Proxy

nt :: WaziupConfig -> Waziup a -> Servant.Handler a
nt s x = runReaderT x s

waziupServer :: WaziupConfig -> Application
waziupServer c = serve waziupAPI $ Servant.Server.hoistServer waziupAPI (nt c) server

-- * Lifting
runOrion :: O.Orion a -> Waziup a
runOrion orion = do
 WaziupConfig _ _ conf <- ask
 e <- liftIO $ runExceptT $ runReaderT orion conf
 case e of
   Right res -> return res
   Left err -> throwError $ fromOrionError err

runKeycloak :: KC.Keycloak a -> Waziup a
runKeycloak kc = do
 (WaziupConfig _ conf _) <- ask
 e <- liftIO $ runExceptT $ runReaderT kc conf
 case e of
   Right res -> return res
   Left err -> throwError $ fromKCError err

runMongo :: Action IO a -> Waziup a
runMongo dbAction = do
  (WaziupConfig (MongoContext pipe mode db) _ _) <- ask
  liftIO $ access pipe mode db dbAction

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "API" s
info s = liftIO $ infoM "API" s
warn s = liftIO $ warningM "API" s
err s = liftIO $ errorM "API" s

