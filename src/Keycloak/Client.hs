{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Keycloak.Client where

import           Control.Lens hiding ((.=))
import           Control.Monad.Reader as R
import qualified Control.Monad.Catch as C
import           Control.Monad.Except (throwError, catchError, MonadError)
import           Data.Aeson as JSON
import           Data.Aeson.Types hiding ((.=))
import           Data.Aeson.BetterErrors as AB
import           Data.Text hiding (head, tail, map)
import           Data.Text.Encoding
import           Data.Maybe
import           Data.ByteString.Base64 as B64
import           Data.String.Conversions
import           Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Keycloak.Types
import           Network.HTTP.Client as HC hiding (responseBody)
import           Network.HTTP.Types.Status
import           Network.Wreq as W hiding (statusCode)
import           Network.Wreq.Types
import           Network.HTTP.Types.Method 
import           System.Log.Logger
import           Debug.Trace

checkPermission :: ResourceId -> Scope -> Maybe Token -> Keycloak ()
checkPermission (ResourceId res) scope tok = do
  debug $ "Checking permissions: " ++ (show res) ++ " " ++ (show scope)
  client <- asks _clientId
  let dat = ["grant_type" := ("urn:ietf:params:oauth:grant-type:uma-ticket" :: Text),
             "audience" := client,
             "permission"  := res <> "#" <> scope]
  keycloakPostDef "protocol/openid-connect/token" dat tok AB.asValue
  return ()

isAuthorized :: ResourceId -> Scope -> Maybe Token -> Keycloak Bool
isAuthorized res scope tok = do
  r <- try $ checkPermission res scope tok
  case r of
    Right _ -> return True
    Left e | (statusCode <$> getErrorStatus e) == Just 403 -> return False
    Left e -> throwError e --rethrow the error

getAllPermissions :: [Scope] -> Maybe Token -> Keycloak [Permission]
getAllPermissions scopes mtok = do
  debug "Get all permissions"
  client <- asks _clientId
  let dat = ["grant_type" := ("urn:ietf:params:oauth:grant-type:uma-ticket" :: Text),
             "audience" := client,
             "response_mode" := ("permissions" :: Text)]
             <> map (\s -> "permission" := ("#" <> s)) scopes
  keycloakPostDef "protocol/openid-connect/token" dat mtok (eachInArray parsePermission)

  
getUserAuthToken :: Text -> Text -> Keycloak Token
getUserAuthToken username password = do 
  debug "Get user token"
  client <- asks _clientId
  secret <- asks _clientSecret
  let dat = ["client_id" := client, 
             "client_secret" := secret,
             "grant_type" := ("password" :: Text),
             "password" := password,
             "username" := username]
  keycloakPost "protocol/openid-connect/token" dat Nothing (Token . encodeUtf8 <$> AB.key "access_token" asText) 

getClientAuthToken :: Keycloak Token
getClientAuthToken = do
  debug "Get client token"
  client <- asks _clientId
  secret <- asks _clientSecret
  let dat = ["client_id" := client, 
             "client_secret" := secret,
             "grant_type" := ("client_credentials" :: Text)]
  keycloakPostDef "protocol/openid-connect/token" dat Nothing (Token . encodeUtf8 <$> AB.key "access_token" asText) 

createResource :: Resource -> Maybe Token -> Keycloak ResourceId
createResource r mtok = do
  debug $ convertString $ "Creating resource: " <> (JSON.encode r)
  keycloakPostDef "authz/protection/resource_set" (toJSON r) mtok (ResourceId <$> AB.key "_id" asText) 

deleteResource :: ResourceId -> Maybe Token -> Keycloak ()
deleteResource (ResourceId rid) mtok = do
  keycloakDeleteDef ("authz/protection/resource_set/" <> rid) mtok 
  return ()

-- Perform post to Keycloak with token.
-- If there is no token, retrieve a guest token
keycloakPostDef :: (Postable dat, Show dat, Show b) => Path -> dat -> Maybe Token -> Parse Text b -> Keycloak b
keycloakPostDef path dat mtok parser = do
  (KCConfig baseUrl realm _ _ _ _ guestId guestPass) <- ask
  tok <- case mtok of
       Just tok -> return tok
       Nothing -> getUserAuthToken guestId guestPass
  keycloakPost path dat (Just tok) parser

-- Perform post to Keycloak.
keycloakPost :: (Postable dat, Show dat, Show b) => Path -> dat -> Maybe Token -> Parse Text b -> Keycloak b
keycloakPost path dat mtok parser = do 
  (KCConfig baseUrl realm _ _ _ _ _ _) <- ask
  let opts = case mtok of
       Just tok -> W.defaults & W.header "Authorization" .~ ["Bearer " <> (unToken tok)]
       Nothing -> W.defaults
  let url = (unpack $ baseUrl <> "/realms/" <> realm <> "/" <> path) 
  info $ "Issuing KEYCLOAK POST with url: " ++ (show url) 
  debug $ "  data: " ++ (show dat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.postWith opts url dat
  case eRes of 
    Right res -> do
      let body = fromJust $ res ^? responseBody
      case AB.parse parser body of
        Right ret -> do
          debug $ "Keycloak success: " ++ (show ret) 
          return ret
        Left err2 -> do
          debug $ "Keycloak parse error: " ++ (show err2) 
          throwError $ ParseError $ pack (show err2)
    Left err -> do
      warn $ "Keycloak HTTP error: " ++ (show err)
      throwError $ HTTPError err

-- Perform delete to Keycloak with default user.
keycloakDeleteDef :: Path -> Maybe Token -> Keycloak ()
keycloakDeleteDef path mtok = do
  (KCConfig baseUrl realm _ _ _ _ guestId guestPass) <- ask
  tok <- case mtok of
       Just tok -> return tok
       Nothing -> getUserAuthToken guestId guestPass
  keycloakDelete path tok

-- Perform delete to Keycloak.
keycloakDelete :: Path -> Token -> Keycloak ()
keycloakDelete path tok = do 
  (KCConfig baseUrl realm _ _ _ _ _ _) <- ask
  let opts = W.defaults & W.header "Authorization" .~ ["Bearer " <> (unToken tok)]
  let url = (unpack $ baseUrl <> "/realms/" <> realm <> "/" <> path) 
  info $ "Issuing KEYCLOAK DELETE with url: " ++ (show url) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.deleteWith opts url
  case eRes of 
    Right res -> return ()
    Left err -> do
      warn $ "Keycloak HTTP error: " ++ (show err)
      throwError $ HTTPError err

debug, warn, info, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "API" s
info s  = liftIO $ infoM "API" s
warn s  = liftIO $ warningM "API" s
err s   = liftIO $ errorM "API" s

getErrorStatus :: KCError -> Maybe Status 
getErrorStatus (HTTPError (HttpExceptionRequest _ (StatusCodeException r _))) = Just $ HC.responseStatus r
getErrorStatus _ = Nothing

try :: MonadError a m => m b -> m (Either a b)
try act = catchError (Right <$> act) (return . Left)

decodeToken :: Token -> Either String TokenDec
decodeToken (Token tok) = case (BS.split '.' tok) ^? element 1 of
    Nothing -> Left "Token is not formed correctly"
    Just part2 -> case AB.parse parseTokenDec (traceShowId $ convertString $ B64.decodeLenient $ traceShowId part2) of
      Right td -> Right td
      Left (e :: ParseError String) -> Left $ show e

getUsername :: Token -> Maybe Username
getUsername tok = do 
  case decodeToken tok of
    Right t -> Just $ preferredUsername t
    Left e -> do
      traceM $ "Error while decoding token: " ++ (show e)
      Nothing
