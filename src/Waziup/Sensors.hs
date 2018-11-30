{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Sensors where

import           Waziup.Types
import           Waziup.Utils
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Data.Maybe
import           Data.Text hiding (map, filter, foldl, any)
import           Data.String.Conversions
import qualified Data.List as L
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import qualified Orion as O
import           System.Log.Logger
import           Paths_Waziup_Servant

getPerms :: Maybe Token -> Waziup [Perm]
getPerms tok = do
  info "Get Permissions"
  let allScopes = [SensorsUpdate,
                   SensorsView,
                   SensorsDelete,
                   SensorsDataCreate,
                   SensorsDataView]
  ps <- runKeycloak $ getAllPermissions (map (convertString.show) allScopes) tok
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
  ps <- getPerms tok
  let sensors2 = filter (checkPermSensor SensorsView ps) sensors
  return sensors2

checkPermSensor :: Scope -> [Perm] -> Sensor -> Bool
checkPermSensor scope perms sen = any (\p -> (permResource p) == (senId sen) && scope `elem` (permScopes p)) perms

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


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Sensors" s
info  s = liftIO $ infoM    "Sensors" s
warn  s = liftIO $ warningM "Sensors" s
err   s = liftIO $ errorM   "Sensors" s

