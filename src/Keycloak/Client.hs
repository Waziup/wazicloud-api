{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Keycloak.Client where

import Network.Wreq as W
import Network.Wreq.Types
import Control.Lens
import Control.Monad
import Data.Aeson as JSON
--import Data.Aeson.Lens
import Data.Aeson.Types
import Data.Aeson.Casing
import Data.Aeson.BetterErrors as AB
import Data.Text hiding (head, tail, map)
import GHC.Generics (Generic)
import Data.Text.Encoding
import Data.Maybe
import Data.Foldable
import Control.Monad.Reader as R
import Keycloak.Types
import Network.HTTP.Client (HttpException)
--import Control.Exception as E
import Data.Monoid
import Control.Monad.Catch
import Control.Monad.Except (ExceptT, throwError, withExceptT)

data KCError = HTTPError HttpException  -- ^ Keycloak returned an HTTP error.
             | ParseError Text          -- ^ Failed when parsing the response
             | EmptyError               -- ^ Empty error to serve as a zero element for Monoid.


type Keycloak a = ReaderT KCConfig (ExceptT KCError IO) a
type Path = Text

keycloakPost :: Postable a => Path -> a -> Maybe Token -> Parse Text b -> Keycloak b
keycloakPost path dat mtok parser = do 
  (KCConfig baseUrl realm _ _ _ _) <- ask
  let opts = if (isJust mtok) 
             then W.defaults & W.header "Authorization" .~ [encodeUtf8 (append "Bearer " (fromJust mtok))]
             else W.defaults
  let url = (unpack $ baseUrl <> "/realms/" <> realm <> "/" <> path) 
  liftIO $ putStrLn $ "Issuing KEYCLOAK post with:\n  " ++ (show url)
  postRes <- try $ liftIO $ W.postWith opts url dat
  case postRes of 
    Right res -> do
      let body = fromJust $ res ^? responseBody
      case AB.parse parser body of
        Right ret -> return ret
        Left err -> throwError $ ParseError $ pack (show err)
    Left err -> throwError $ HTTPError err

getAllPermissions :: Token -> Keycloak [Permission]
getAllPermissions tok = do
  (KCConfig _ _ client _ _ _) <- ask 
  let dat = ["grant_type" := ("urn:ietf:params:oauth:grant-type:uma-ticket" :: Text),
             "audience" := client,
             "response_mode" := ("permissions" :: Text)]
  keycloakPost "protocol/openid-connect/token" dat (Just tok) (eachInArray parsePermission)

  
getUserAuthToken :: Text -> Text -> Keycloak Token
getUserAuthToken username password = do 
  (KCConfig _ realm client secret login password) <- ask 
  let dat = ["client_id" := client, 
             "client_secret" := secret,
             "grant_type" := ("password" :: Text),
             "password" := password,
             "username" := login]
  keycloakPost "protocol/openid-connect/token" dat Nothing (AB.key "access_token" asText) 

getClientAuthToken :: Keycloak Token
getClientAuthToken = do
  (KCConfig _ realm client secret login password) <- ask
  let dat = ["client_id" := client, 
             "client_secret" := secret,
             "grant_type" := ("client_credentials" :: Text)]
  keycloakPost "protocol/openid-connect/token" dat Nothing (AB.key "access_token" asText) 

createResource :: Resource -> Token -> Keycloak ResourceId
createResource r tok = do
  keycloakPost "authz/protection/resource_set" (toJSON r) (Just tok) (AB.key "_id" asText) 
   
