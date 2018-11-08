{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Keycloak.Client where

import Network.Wreq as W hiding (statusCode)
import Network.Wreq.Types
import Control.Lens
import Control.Monad
import Data.Aeson as JSON
import Data.Aeson.BetterErrors as AB
import Data.Text hiding (head, tail, map)
import Data.Text.Encoding
import Data.Maybe
import Control.Monad.Reader as R
import Keycloak.Types
import Network.HTTP.Client as HC hiding (responseBody)
import Network.HTTP.Types.Status
import Data.Monoid
import qualified Control.Monad.Catch as C
import Control.Monad.Except (ExceptT, throwError, catchError, MonadError)
import System.Log.Logger
 
data KCError = HTTPError HttpException  -- ^ Keycloak returned an HTTP error.
             | ParseError Text          -- ^ Failed when parsing the response
             | EmptyError               -- ^ Empty error to serve as a zero element for Monoid.


type Keycloak a = ReaderT KCConfig (ExceptT KCError IO) a
type Path = Text

try :: MonadError a m => m b -> m (Either a b)
try act = catchError (Right <$> act) (return . Left)

checkPermission :: ResourceId -> Scope -> Token -> Keycloak ()
checkPermission res scope tok = do 
  (KCConfig _ _ client _ _ _) <- ask 
  let dat = ["grant_type" := ("urn:ietf:params:oauth:grant-type:uma-ticket" :: Text),
             "audience" := client,
             "permission"  := res <> "#" <> scope]
  keycloakPost "protocol/openid-connect/token" dat (Just tok) AB.asValue
  return ()

isAuthorized :: ResourceId -> Scope -> Token -> Keycloak Bool
isAuthorized res scope tok = do
  res <- try $ checkPermission res scope tok
  case res of
    Right _ -> return True
    Left err | (statusCode <$> getErrorStatus err) == Just 403 -> return False
    Left err -> throwError err --rethrow the error

getErrorStatus :: KCError -> Maybe Status 
getErrorStatus (HTTPError (HttpExceptionRequest _ (StatusCodeException r _))) = Just $ HC.responseStatus r
getErrorStatus _ = Nothing

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
  keycloakPost "protocol/openid-connect/token" dat Nothing (Token <$> AB.key "access_token" asText) 

getClientAuthToken :: Keycloak Token
getClientAuthToken = do
  (KCConfig _ realm client secret login password) <- ask
  let dat = ["client_id" := client, 
             "client_secret" := secret,
             "grant_type" := ("client_credentials" :: Text)]
  keycloakPost "protocol/openid-connect/token" dat Nothing (Token <$> AB.key "access_token" asText) 

createResource :: Resource -> Token -> Keycloak ResourceId
createResource r tok = do
  keycloakPost "authz/protection/resource_set" (toJSON r) (Just tok) (AB.key "_id" asText) 
   
keycloakPost :: (Postable a, Show a) => Path -> a -> Maybe Token -> Parse Text b -> Keycloak b
keycloakPost path dat mtok parser = do 
  (KCConfig baseUrl realm _ _ _ _) <- ask
  let opts = case mtok of
                   --Just (stripPrefix "Bearer " -> Just tok) -> W.defaults & W.header "Authorization" .~ [encodeUtf8 ("Bearer " <> tok)]
                   (Just (Token tok)) -> W.defaults & W.header "Authorization" .~ [encodeUtf8 ("Bearer " <> tok)]
                   Nothing -> W.defaults 
  let url = (unpack $ baseUrl <> "/realms/" <> realm <> "/" <> path) 
  info $ "Issuing KEYCLOAK post with url: " ++ (show url) 
  debug $ "  data: " ++ (show dat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  postRes <- C.try $ liftIO $ W.postWith opts url dat
  case postRes of 
    Right res -> do
      let body = fromJust $ res ^? responseBody
      case AB.parse parser body of
        Right ret -> return ret
        Left err -> throwError $ ParseError $ pack (show err)
    Left err -> throwError $ HTTPError err


warn, info :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "API" s
info s = liftIO $ infoM "API" s
warn s = liftIO $ warningM "API" s
err s = liftIO $ errorM "API" s
