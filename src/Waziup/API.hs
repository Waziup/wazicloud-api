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

import Control.Monad.Except (ExceptT, throwError, withExceptT)
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
import Network.HTTP.Client as HC hiding (Proxy)
import Network.HTTP.Types.Status as HTS
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
  { configHost :: String   -- ^ Hostname to serve on, e.g. "127.0.0.1"
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
  liftIO $ putStrLn "Get token"
  tok <- runKeycloak $ getUserAuthToken username password
  liftIO $ putStrLn "Get Permissions"
  ps <- runKeycloak (getAllPermissions tok)
  let getP :: KC.Permission -> Perm
      getP (Permission rsname _ scopes) = Perm rsname (mapMaybe readScope scopes)
  return $ map getP ps 

postAuth :: AuthBody -> ExceptT ServantErr IO Text
postAuth (AuthBody username password) = runKeycloak (getUserAuthToken username password)

getSensors :: ExceptT ServantErr IO [Sensor]
getSensors = runOrion O.getSensorsOrion

getSensor :: Text -> ExceptT ServantErr IO Sensor
getSensor sid = runOrion (O.getSensorOrion sid)

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
  liftIO $ putStrLn "Get token"
  tok <- runKeycloak getClientAuthToken
  liftIO $ putStrLn "Create resource"
  resId <- runKeycloak (createResource res tok)
  liftIO $ putStrLn "Create entity"
  res <- try $ runOrion (O.postSensorOrion (s {senKeycloakId = Just resId}))
  case res of
    Right _ -> return NoContent
    Left (err :: HttpException) -> do
      liftIO $ putStrLn "Orion error"
      return NoContent
 

runOrion :: O.Orion a -> ExceptT ServantErr IO a
runOrion orion = withExceptT fromOrionError (runReaderT orion O.defaultOrionConfig)

runKeycloak :: Keycloak a -> ExceptT ServantErr IO a
runKeycloak kc = withExceptT fromKCError (runReaderT kc defaultConfig)

fromOrionError :: O.OrionError -> ServantErr
fromOrionError (O.HTTPError (HttpExceptionRequest _ (StatusCodeException r _))) = ServantErr { errHTTPCode = HTS.statusCode $ HC.responseStatus r, 
                                                                                           errReasonPhrase = show $ HTS.statusMessage $ HC.responseStatus r, 
                                                                                           errBody = "",
                                                                                           errHeaders = []}
fromOrionError (O.ParseError s) = err500 {errBody = encode s} 
fromOrionError O.EmptyError = err500 {errBody = "EmptyError"}

fromKCError :: KCError -> ServantErr
fromKCError (HTTPError (HttpExceptionRequest _ (StatusCodeException r _))) = ServantErr { errHTTPCode = HTS.statusCode $ HC.responseStatus r, 
                                                                                           errReasonPhrase = show $ HTS.statusMessage $ HC.responseStatus r, 
                                                                                           errBody = "",
                                                                                           errHeaders = []}
fromKCError (ParseError s) = err500 {errBody = encode s} 
fromKCError EmptyError = err500 {errBody = "EmptyError"}

waziupAPI :: Proxy WaziupAPI
waziupAPI = Proxy

waziupServer :: Application
waziupServer = serve waziupAPI server
