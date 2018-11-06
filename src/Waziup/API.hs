{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=328 #-}

module Waziup.API
  where

import Waziup.Types

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class
import Data.Maybe
import Data.Aeson
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text hiding (map)
import Data.Text.Encoding
import GHC.Exts (IsString(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp as Warp
import Servant-- (ServantErr, serve)
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Servant.Client (Scheme(Http), ServantError, client)
import Servant.Common.BaseUrl (BaseUrl(..))
import Web.HttpApiData
import Keycloak as KC
import qualified Orion.Client as O
import Control.Exception.Lifted
import Network.HTTP.Client hiding (Proxy)
import Network.Wreq hiding (Proxy)
import Control.Monad.Reader

-- | Servant type-level API
type WaziupAPI = AuthAPI :<|> SensorsAPI

type AuthAPI = "auth" :> "permissions" :> Get '[JSON] [Perm]
          :<|> "auth" :> "token" :> ReqBody '[JSON] AuthBody :> Post '[JSON] Text

type SensorsAPI = "sensors" :> Get '[JSON] [Sensor]
              :<|>"sensors" :> Capture "id" Text :> Get '[JSON] Sensor
              :<|>"sensors" :> ReqBody '[JSON] Sensor :> PostNoContent '[JSON] NoContent

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String  -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

server :: Server WaziupAPI
server = authServer 
    :<|> sensorsServer

authServer :: Server AuthAPI
authServer = (getPerms "cdupont" "password") 
        :<|> postAuth

sensorsServer :: Server SensorsAPI
sensorsServer = getSensors 
           :<|> getSensor
           :<|> postSensor

getPerms :: Text -> Text -> ExceptT ServantErr IO [Perm]
getPerms username password = do
  mps <- liftIO $ runReaderT (getAllPermissions username password) defaultConfig
  case mps of
    Right ps -> do
      let getP :: KC.Permission -> Perm
          getP (Permission rsname _ scopes) = Perm rsname (mapMaybe (readScope.scpName) scopes) 
      return $ map getP ps 
    Left e -> return []

postAuth :: AuthBody -> ExceptT ServantErr IO Text
postAuth (AuthBody username password) = do
  res <- liftIO $ runReaderT (getUserAuthToken username password) defaultConfig
  case res of 
    Right token -> return token
    Left err -> return undefined

getSensors :: ExceptT ServantErr IO [Sensor]
getSensors = do
  res <- liftIO $ runReaderT O.getSensorsOrion O.defaultOrionConfig
  case res of
    Right s -> return s
    Left err -> throwError err404 


getSensor :: Text -> ExceptT ServantErr IO Sensor
getSensor sid = do 
  res <- liftIO $ runReaderT (O.getSensorOrion sid) O.defaultOrionConfig
  case res of
    Right s -> return s
    Left err -> throwError err404

postSensor :: Sensor -> ExceptT ServantErr IO NoContent
postSensor s@(Sensor id _ _ _ _ _ _ _ _ vis _) = do
  let res = Resource {
     resId      = Nothing,
     resName    = id,
     resType    = Nothing,
     resUris    = [],
     resScopes  = [],
     resOwner   = Owner Nothing "cdupont",
     resOwnerManagedAccess = True,
     resAttributes = if (isJust vis) then [Attribute "visibility" [pack $ show $ fromJust vis]] else []
     }
  liftIO $ putStrLn "Creating Sensor"
  tok <- runKeycloak getClientAuthToken
  resId <- runKeycloak (createResource res tok)
  runOrion (O.postSensorOrion (s {senKeycloakId = Just resId}))
  return NoContent

runOrion :: O.Orion a -> ExceptT ServantErr IO a
runOrion orion = do
  eRes <- liftIO $ runReaderT orion O.defaultOrionConfig
  case eRes of
    Right res -> return res 
    Left (O.HTTPError e) -> throwError err500 {errBody = encode $ show e} 
    Left (O.ParseError s) -> throwError err500 {errBody = encode s} 
    Left O.EmptyError -> throwError err500 {errBody = "EmptyError"} 

runKeycloak :: Keycloak a -> ExceptT ServantErr IO a
runKeycloak kc = do
  eRes <- liftIO $ runReaderT kc defaultConfig
  case eRes of
    Right res -> return res 
    Left (HTTPError e) -> throwError err500 {errBody = encode $ show e} 
    Left (ParseError s) -> throwError err500 {errBody = encode s} 
    Left EmptyError -> throwError err500 {errBody = "EmptyError"} 

waziupAPI :: Proxy WaziupAPI
waziupAPI = Proxy

waziupServer :: Application
waziupServer = serve waziupAPI server
