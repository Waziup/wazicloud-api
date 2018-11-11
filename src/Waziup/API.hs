{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.API where

import Waziup.Types
import Waziup.Utils
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import Data.Proxy (Proxy(..))
import Data.Text hiding (map, filter, foldl, any)
import Servant
import Keycloak as KC hiding (info, warn, debug, Scope) 
import qualified Orion as O
import Control.Monad.Catch as C
import Servant.API.Flatten
import Network.HTTP.Client (HttpException)


server :: Server (WaziupAPI)
server = authServer 
    :<|> sensorsServer

authServer :: Server (AuthAPI)
authServer = getPerms 
        :<|> postAuth

sensorsServer :: Server (SensorsAPI)
sensorsServer =  (getSensors 
           :<|> postSensor
           :<|> getSensor
           :<|> deleteSensor)

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

waziupAPI :: Proxy WaziupAPI
waziupAPI = Proxy

waziupServer :: Application
waziupServer = serve waziupAPI server

