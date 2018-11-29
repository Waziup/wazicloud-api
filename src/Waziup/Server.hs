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
import Control.Lens hiding ((.=))
import Data.Maybe
import Data.Proxy (Proxy(..))
import Data.Text hiding (map, filter, foldl, any)
import Data.String.Conversions
import Data.Aeson.BetterErrors as AB
import qualified Data.ByteString as BS
import Data.Aeson
import qualified Data.List as L
import qualified Data.Swagger as S
import Servant
import Servant.Server
import Servant.Swagger
import Servant.Swagger.UI
import Servant.Swagger.UI.Core
import Servant.Swagger.Internal
import Keycloak as KC hiding (info, warn, debug, err, Scope) 
import qualified Orion as O
import Mongo as M hiding (info, warn, debug, err, Scope) 
import Database.MongoDB as DB
import Control.Monad.Catch as C
import Servant.API.Flatten
import Network.HTTP.Client (HttpException)
import GHC.Generics (Generic)
import System.Log.Logger
import Paths_Waziup_Servant
import System.FilePath ((</>))
import Data.ByteString.Base64 as B64

server :: ServerT API Waziup
server = serverWaziup :<|> serverDocs

serverWaziup :: ServerT WaziupAPI Waziup
serverWaziup = authServer :<|> sensorsServer :<|> projectsServer :<|> ontologiesServer

serverDocs :: ServerT WaziupDocs Waziup
serverDocs = hoistDocs $ swaggerSchemaUIServer swaggerDoc

swaggerDoc :: S.Swagger
swaggerDoc = toSwagger (Proxy :: Proxy WaziupAPI)
  & S.info . S.title       .~ "Waziup API"
  & S.info . S.version     .~ "v2.0.0"
  & S.info . S.description ?~ "This is the API of Waziup"
  & S.basePath ?~ "/api/v1"
  & S.applyTagsFor sensorsOps ["Sensors"]
  & S.applyTagsFor authOps    ["Auth"]
  & S.applyTagsFor projectOps ["Projects"]
  & S.applyTagsFor ontoOps    ["Ontologies"]
  where
    sensorsOps, authOps, projectOps, ontoOps :: Traversal' S.Swagger S.Operation
    sensorsOps = subOperations (Proxy :: Proxy SensorsAPI)    (Proxy :: Proxy WaziupAPI)
    authOps    = subOperations (Proxy :: Proxy AuthAPI)       (Proxy :: Proxy WaziupAPI)
    projectOps = subOperations (Proxy :: Proxy ProjectsAPI)    (Proxy :: Proxy WaziupAPI)
    ontoOps    = subOperations (Proxy :: Proxy OntologiesAPI) (Proxy :: Proxy WaziupAPI)

authServer :: ServerT AuthAPI Waziup
authServer = getPerms :<|> postAuth

sensorsServer :: ServerT SensorsAPI Waziup
sensorsServer = getSensors :<|> postSensor :<|> getSensor :<|> deleteSensor

projectsServer :: ServerT ProjectsAPI Waziup
projectsServer = getProjects :<|> postProject :<|> getProject :<|> deleteProject :<|> putProjectDevices :<|> putProjectGateways

ontologiesServer :: ServerT OntologiesAPI Waziup
ontologiesServer = getSensingDevices :<|> getQuantityKinds :<|> getUnits

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
  let sensors2 = filter (checkPermSensor SensorsView ps) sensors
  return sensors2

checkPermSensor :: Scope -> [Permission] -> Sensor -> Bool
checkPermSensor scope perms sen = any (\p -> (rsname p) == (senId sen) && (convertString $ show scope) `elem` (scopes p)) perms

getSensor :: Maybe Token -> SensorId -> Waziup Sensor
getSensor tok sid = do
  info "Get sensor"
  sensor <- runOrion (O.getSensorOrion sid)
  case (senKeycloakId sensor) of
    Just keyId -> do
      debug "Check permissions"
      runKeycloak $ checkPermission keyId (pack $ show SensorsView) tok
      debug "Permission granted, returning sensor"
      return sensor
    Nothing -> do
      err "Error, sensor does not have a Keycloak ID"
      throwError err500 {errBody = "Not authorized"}

postSensor :: Maybe Token -> Sensor -> Waziup NoContent
postSensor tok s@(Sensor sid _ _ _ _ vis _ _ _ _ _) = do
  info $ "Post sensor: " ++ (show s)
  debug "Check permissions"
  runKeycloak $ checkPermission (ResourceId "Sensors") (pack $ show SensorsCreate) tok
  debug "Create entity"
  let username = case tok of
       Just t -> getUsername t
       Nothing -> Just "guest"
  debug $ "Onwer: " <> (show username)
  res2 <- C.try $ runOrion $ O.postSensorOrion (s {senOwner = username})
  case res2 of
    Right _ -> do 
      let res = KC.Resource {
         resId      = Nothing,
         resName    = sid,
         resType    = Nothing,
         resUris    = [],
         resScopes  = map (pack.show) [SensorsView, SensorsUpdate, SensorsDelete, SensorsDataCreate, SensorsDataView],
         resOwner   = Owner Nothing "cdupont",
         resOwnerManagedAccess = True,
         resAttributes = if (isJust vis) then [Attribute "visibility" [pack $ show $ fromJust vis]] else []}
      keyRes <- C.try $ runKeycloak $ createResource res tok
      case keyRes of
        Right resId -> do
          runOrion $ O.postSensorKeycloakOrion sid resId
          return NoContent
        Left err -> do
          error $ "Keycloak error: " ++ (show err) ++ " deleting sensor"
          (_ :: Either ServantErr ()) <- C.try $ runOrion $ O.deleteSensorOrion sid
          throwError err

    Left (err :: ServantErr)  -> do
      warn "Orion error"
      throwError err 
 
deleteSensor :: Maybe Token -> SensorId -> Waziup NoContent
deleteSensor tok sid = do
  info "Delete sensor"
  sensor <- runOrion (O.getSensorOrion sid)
  case (senKeycloakId sensor) of
    Just keyId -> do
      debug "Check permissions"
      runKeycloak $ checkPermission keyId (pack $ show SensorsDelete) tok
      debug "Delete Keycloak resource"
      runKeycloak $ deleteResource keyId tok
      debug "Delete Orion resource"
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

-- * Ontologies

getSensingDevices :: Waziup [SensingDeviceInfo]
getSensingDevices = do
  dir <- liftIO $ getDataDir
  msd <- liftIO $ BS.readFile $ dir </> "ontologies" </> "sensing_devices.json"
  case AB.parse (eachInArray parseSDI) (convertString msd) of
    Right sd -> return sd
    Left (e :: ParseError String) -> do
      error $ "Cannot decode data file: " ++ (show e)
      throwError err500 {errBody = "Cannot decode data file"}

getQuantityKinds :: Waziup [QuantityKindInfo]
getQuantityKinds = do
  dir <- liftIO getDataDir
  mqk <- liftIO $ BS.readFile $ dir </> "ontologies" </> "quantity_kinds.json"
  case AB.parse (eachInArray parseQKI) (convertString mqk) of
    Right qk -> return qk
    Left (e :: ParseError String) -> do
      error $ "Cannot decode data file: " ++ (show e)
      throwError err500 {errBody = "Cannot decode data file"}

getUnits :: Waziup [UnitInfo]
getUnits = do
  dir <- liftIO getDataDir
  mus <- liftIO $ BS.readFile $ dir </> "ontologies" </> "units.json"
  case AB.parse (eachInArray parseUnit) (convertString mus) of
    Right us -> return us
    Left (e :: ParseError String) -> do
      error $ "Cannot decode data file: " ++ (show e)
      throwError err500 {errBody = "Cannot decode data file"}

-- * Server

waziupAPI :: Proxy API
waziupAPI = Proxy

getHandler :: WaziupInfo -> Waziup a -> Servant.Handler a
getHandler s x = runReaderT x s

waziupServer :: WaziupInfo -> Application
waziupServer conf = serve waziupAPI $ Servant.Server.hoistServer waziupAPI (getHandler conf) server

hoistDocs :: ServerT WaziupDocs Servant.Handler -> ServerT WaziupDocs Waziup
hoistDocs s = Servant.Server.hoistServer (Proxy :: Proxy WaziupDocs) lift s


-- * Lifting
runOrion :: O.Orion a -> Waziup a
runOrion orion = do
 (WaziupInfo _ (WaziupConfig _ _ _ conf) _) <- ask
 e <- liftIO $ runExceptT $ runReaderT orion conf
 case e of
   Right res -> return res
   Left err -> throwError $ fromOrionError err

runKeycloak :: KC.Keycloak a -> Waziup a
runKeycloak kc = do
 (WaziupInfo _ (WaziupConfig _ _ conf _) _) <- ask
 e <- liftIO $ runExceptT $ runReaderT kc conf
 case e of
   Right res -> return res
   Left err -> throwError $ fromKCError err

runMongo :: Action IO a -> Waziup a
runMongo dbAction = do
  (WaziupInfo pipe _ _) <- ask
  liftIO $ access pipe DB.master "projects" dbAction

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "API" s
info  s = liftIO $ infoM "API" s
warn  s = liftIO $ warningM "API" s
err   s = liftIO $ errorM "API" s

