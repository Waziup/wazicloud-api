{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.API where

import Waziup.Types
import Waziup.Utils
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class
import Data.Maybe
import Data.Proxy (Proxy(..))
import Data.Text hiding (map, filter, foldl, any)
import Servant
import Keycloak as KC hiding (info, warn, debug, Scope) 
import qualified Orion as O
import Control.Monad.Catch as C
import Servant.API.Flatten
import Network.HTTP.Client (HttpException)

-- | Servant type-level API
type WaziupAPI = "api" :> "v1" :> (AuthAPI :<|> SensorsAPI)

type AuthAPI = 
  "auth" :>  ("permissions" :> Header "Authorization" Token :> Get '[JSON] [Perm]
         :<|> "token"       :> ReqBody '[JSON] AuthBody :> Post '[PlainText] Token)

type SensorsAPI = Flat ( 
  "sensors" :> Header "Authorization" Token :> 
                (Get '[JSON] [Sensor] :<|>
                 ReqBody '[JSON] Sensor :> PostNoContent '[JSON] NoContent :<|>
                 SensorAPI))

type SensorAPI = (
  Capture "id" Text :> (Get '[JSON] Sensor :<|>
                        DeleteNoContent '[JSON] NoContent))

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String   -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

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
           :<|> undefined)

getPerms :: Maybe Token -> ExceptT ServantErr IO [Perm]
getPerms tok = do
  liftIO $ putStrLn "Get Permissions"
  ps <- runKeycloak (getAllPermissions tok)
  let getP :: KC.Permission -> Perm
      getP (KC.Permission rsname _ scopes) = Perm rsname (mapMaybe readScope scopes)
  return $ map getP ps 

postAuth :: AuthBody -> ExceptT ServantErr IO Token
postAuth (AuthBody username password) = do
  tok <- runKeycloak (getUserAuthToken username password)
  liftIO $ putStrLn $ show tok
  return tok

getSensors :: Maybe Token -> ExceptT ServantErr IO [Sensor]
getSensors tok = do
  sensors <- runOrion O.getSensorsOrion
  ps <- runKeycloak (getAllPermissions tok)
  let sensors2 = filter (\s -> any (\p -> (rsname p) == (senId s)) ps) sensors
  return sensors2

checkAuth :: SensorId -> Scope -> Maybe Token -> ExceptT ServantErr IO a -> ExceptT ServantErr IO a
checkAuth sid scope tok act = do
  isAuth <- runKeycloak (isAuthorized sid (pack $ show scope) tok)
  if isAuth 
    then act
    else throwError err403 {errBody = "Not authorized"}
  
getSensor :: Maybe Token -> SensorId -> ExceptT ServantErr IO Sensor
getSensor tok sid = checkAuth sid SensorsView tok $ runOrion (O.getSensorOrion sid)

postSensor :: Maybe Token -> Sensor -> ExceptT ServantErr IO NoContent
postSensor tok s@(Sensor sid _ _ _ _ _ _ _ _ vis _) = do
  let res = KC.Resource {
     resId      = Nothing,
     resName    = sid,
     resType    = Nothing,
     resUris    = [],
     resScopes  = map (pack.show) [SensorsView, SensorsUpdate, SensorsDelete, SensorsDataCreate, SensorsDataView],
     resOwner   = Owner Nothing "cdupont",
     resOwnerManagedAccess = True,
     resAttributes = if (isJust vis) then [Attribute "visibility" [pack $ show $ fromJust vis]] else []
     }
  debug "Check permissions"
  runKeycloak $ checkPermission sid (pack $ show SensorsCreate) tok
  debug "Create resource"
  resId <- runKeycloak (createResource res tok)
  debug "Create entity"
  res2 <- C.try $ runOrion (O.postSensorOrion (s {senKeycloakId = Just resId}))
  case res2 of
    Right _ -> return NoContent
    Left (e :: HttpException) -> do
      warn "Orion error" -- TODO: need to delete Keycloak resource
      return NoContent
 

waziupAPI :: Proxy WaziupAPI
waziupAPI = Proxy

waziupServer :: Application
waziupServer = serve waziupAPI server

