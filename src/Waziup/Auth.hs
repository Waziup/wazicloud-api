{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Auth where

import           Waziup.Types as W
import           Waziup.Utils
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Control.Monad
import           Data.Maybe
import           Data.Map as M hiding (map, mapMaybe, filter, lookup, insert, delete)
import           Data.Text hiding (map, filter, foldl, any)
import           Data.String.Conversions
import qualified Data.List as L
import qualified Data.Vector as V
import           Data.Scientific
import qualified Data.HashMap.Strict as H
import           Data.Aeson as JSON hiding (Options)
import           Data.Time.ISO8601
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, try) 
import           Orion as O hiding (info, warn, debug, err, try)
import           System.Log.Logger
import           Paths_Waziup_Servant
import           Database.MongoDB as DB hiding (value, Limit, Array, lookup, Value, Null, (!?))
import           Data.AesonBson


-- | get a token
postAuth :: AuthBody -> Waziup Token
postAuth (AuthBody username password) = do
  info "Post authentication"
  tok <- liftKeycloak' $ getUserAuthToken username password
  return tok

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsDevices :: Maybe Token -> Waziup [Perm]
getPermsDevices tok = do
  info "Get devices permissions"
  getPerms tok [DevicesUpdate,
                DevicesView,
                DevicesDelete,
                DevicesDataView,
                DevicesDataCreate]

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsProjects :: Maybe Token -> Waziup [Perm]
getPermsProjects tok = do
  info "Get projects permissions"
  getPerms tok [ProjectsUpdate,
                ProjectsView,
                ProjectsDelete]

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsGateways :: Maybe Token -> Waziup [Perm]
getPermsGateways tok = do
  info "Get gateways permissions"
  getPerms tok [GatewaysUpdate,
                GatewaysView,
                GatewaysDelete]

getPerms :: Maybe Token -> [W.Scope] -> Waziup [Perm]
getPerms tok scps = do
  ps <- liftKeycloak tok $ getAllPermissions (map fromScope scps)
  let getP :: KC.Permission -> Perm
      getP (KC.Permission rsname _ scopes) = Perm rsname (mapMaybe toScope scopes)
  return $ map getP ps 

createResource' :: Maybe Token -> ResourceName -> ResourceType -> [W.Scope] ->  [KC.Attribute] -> Waziup ResourceId
createResource' tok resNam resTyp scopes attrs = do
  let username = case tok of
       Just t -> getUsername t
       Nothing -> "guest"
  let kcres = KC.Resource {
         resId      = Nothing,
         resName    = resNam,
         resType    = Just resTyp,
         resUris    = [],
         resScopes  = map (\s -> KC.Scope Nothing (fromScope s)) scopes,
         resOwner   = Owner Nothing username,
         resOwnerManagedAccess = True,
         resAttributes = attrs}
  liftKeycloak tok $ KC.createResource kcres

-- | Check that `perms` contain a permission for the resource with the correspondnog scope.
checkPermResource' :: W.Scope -> [Perm] -> W.PermResource -> Bool
checkPermResource' scope perms rid = any (\p -> (permResource p) == rid && scope `elem` (permScopes p)) perms

-- | Throws error 403 if `perms` if there is no permission for the resource under the corresponding scope.
checkPermResource :: Maybe Token -> W.Scope -> W.PermResource -> Waziup ()
checkPermResource tok scope rid = do
  ps <- getPerms tok [scope]
  debug $ "perms: " ++ (show ps)
  if checkPermResource' scope ps rid
     then return ()
     else throwError err403 {errBody = "Forbidden: Cannot access project"}

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Auth" s
info  s = liftIO $ infoM    "Auth" s
warn  s = liftIO $ warningM "Auth" s
err   s = liftIO $ errorM   "Auth" s

