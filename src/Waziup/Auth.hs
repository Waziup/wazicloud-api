{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Waziup.Auth where

import           Waziup.Types as W
import           Waziup.Utils
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Control.Monad
import           Data.Maybe
import           Data.Map as M hiding (map, mapMaybe, filter, delete)
import           Data.Text hiding (map, filter, foldl, any)
import           Data.String.Conversions
import qualified Data.List as L
import qualified Data.Vector as V
import           Data.Scientific
import qualified Data.HashMap.Strict as H
import           Data.Time.ISO8601
import           Data.Time
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, try) 
import           Orion as O hiding (info, warn, debug, err, try)
import           System.Log.Logger
import           Paths_Waziup_Servant
import           Database.MongoDB as DB hiding (value, Limit, Array, lookup, Value, Null, (!?))
import           Data.AesonBson
import           Control.Lens
import           Control.Concurrent.STM

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
getPerms mtok scps = do
  debug "getPerms"
  permsTV <- view permCache
  permsM <- liftIO $ atomically $ readTVar permsTV
  tok <- fromMaybeToken mtok
  let username = getUsername tok
  now <- liftIO getCurrentTime
  --look up username in cache
  debug $ "lookup cache for " ++ (show username)
  cachedPerms <- case M.lookup username permsM of
    Just (PermCache perms retrievedTime) -> do
      debug "cache found"
      let delay = 60 --seconds
      --checl if not expired
      if now < addUTCTime delay retrievedTime
        --return permissions
        then do
          debug "cache valid"
          return $ Just perms
        --else get perms from Keycloak and update the cache
        else do
          debug "cache expired"
          return Nothing
    Nothing -> do
      debug $ "cache not found"
      return Nothing
  case cachedPerms of
    Just perms -> return perms
    Nothing -> do
      debug "get perms from Keycloak"
      perms <- liftKeycloak (Just tok) $ getAllPermissions (map fromScope allScopes)
      let perms' = map getPerm perms
      let permsM' = M.insert username (PermCache perms' now) permsM
      debug "update cache"
      liftIO $ atomically $ writeTVar permsTV permsM' 
      return perms'

invalidateCache :: Waziup ()
invalidateCache = do
  permsTV <- view permCache
  liftIO $ atomically $ writeTVar permsTV M.empty 
  
  
getPerm :: KC.Permission -> Perm
getPerm (KC.Permission rsname rsid scopes) = Perm (getPermResource rsid rsname) (mapMaybe toScope scopes)

--Resource ID is extracted from the KC ID.
-- For legacy reason, old resources uses the resource name to store the device ID (to be removed after migration)
getPermResource :: ResourceId -> ResourceName -> PermResource
getPermResource (ResourceId (stripPrefix "device-" -> Just id))  _ = PermDeviceId $ DeviceId id
getPermResource (ResourceId (stripPrefix "gateway-" -> Just id)) _ = PermGatewayId $ GatewayId id
getPermResource (ResourceId (stripPrefix "project-" -> Just id)) _ = PermProjectId $ ProjectId id
getPermResource _ rsName = PermDeviceId $ DeviceId rsName

-- opposite conversion: from Waziup Ids to KC IDs.
getKCResourceId :: PermResource -> KC.ResourceId
getKCResourceId (PermDeviceId  (DeviceId id))  = ResourceId $ "device-"  <> id
getKCResourceId (PermGatewayId (GatewayId id)) = ResourceId $ "gateway-" <> id
getKCResourceId (PermProjectId (ProjectId id)) = ResourceId $ "project-" <> id

createResource' :: Maybe Token -> Maybe ResourceId -> ResourceName -> ResourceType -> [W.Scope] ->  [KC.Attribute] -> Waziup ResourceId
createResource' tok resId resNam resTyp scopes attrs = do
  let username = case tok of
       Just t -> getUsername t
       Nothing -> "guest"
  createResource'' tok resId resNam resTyp scopes attrs username 

createResource'' :: Maybe Token -> Maybe ResourceId -> ResourceName -> ResourceType -> [W.Scope] ->  [KC.Attribute] -> KC.Username -> Waziup ResourceId
createResource'' tok resId resNam resTyp scopes attrs username = do
  --creating a new resource in Keycloak invalidates the cache
  invalidateCache
  let kcres = KC.Resource {
         resId      = resId,
         resName    = resNam,
         resType    = Just resTyp,
         resUris    = [],
         resScopes  = map (\s -> KC.Scope Nothing (fromScope s)) scopes,
         resOwner   = Owner Nothing username,
         resOwnerManagedAccess = True,
         resAttributes = attrs}
  liftKeycloak tok $ KC.createResource kcres

-- | Check that `perms` contain a permission for the resource with the corresponding scope.
checkPermResource' :: W.Scope -> [Perm] -> W.PermResource -> Bool
checkPermResource' scope perms rid = any (isPermitted rid scope) perms where
  isPermitted :: PermResource -> W.Scope -> Perm -> Bool
  isPermitted rid scope (Perm pid scopes) = pid == rid && scope `elem` scopes

-- | Throws error 403 if `perms` if there is no permission for the resource under the corresponding scope.
checkPermResource :: Maybe Token -> W.Scope -> W.PermResource -> Waziup ()
checkPermResource tok scope rid = do
  ps <- getPerms tok [scope]
  debug $ "perms: " ++ (show ps)
  if checkPermResource' scope ps rid
     then return ()
     else throwError err403 {errBody = "Forbidden: Cannot access project"}

deleteResource :: Maybe Token -> PermResource -> Waziup ()
deleteResource tok pr = do
  --invalidate all cache
  invalidateCache
  liftKeycloak tok $ KC.deleteResource $ getKCResourceId pr

deleteResource' :: Maybe Token -> ResourceId -> Waziup ()
deleteResource' tok resId = do
  --invalidate all cache
  invalidateCache
  liftKeycloak tok $ KC.deleteResource resId

-- | Update a resource
updateResource :: Maybe Token -> KC.Resource ->  Waziup ResourceId
updateResource tok res = do 
  --invalidate all cache
  invalidateCache
  liftKeycloak tok $ KC.createResource res 


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Auth" s
info  s = liftIO $ infoM    "Auth" s
warn  s = liftIO $ warningM "Auth" s
err   s = liftIO $ errorM   "Auth" s

