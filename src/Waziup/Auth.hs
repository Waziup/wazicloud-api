{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Auth where

import           Waziup.Types as W
import           Waziup.Utils as U
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Data.String.Conversions
import           Data.Maybe
import           Data.Map as M hiding (map, mapMaybe, filter, delete)
import           Data.Time
import           Servant
import           Keycloak as KC
import           System.Log.Logger
import           Control.Lens
import           Control.Concurrent.STM

-- | get a token
postAuth :: AuthBody -> Waziup Token
postAuth (AuthBody username password) = do
  info "Post authentication"
  tok <- liftKeycloak' $ getUserAuthToken username password
  return tok

-- * Permissions

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsDevices :: Maybe Token -> Waziup [Perm]
getPermsDevices tok = do
  info "Get devices permissions"
  getPerms tok (getPermReq Nothing deviceScopes)

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsProjects :: Maybe Token -> Waziup [Perm]
getPermsProjects tok = do
  info "Get projects permissions"
  getPerms tok (getPermReq Nothing projectScopes)

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsGateways :: Maybe Token -> Waziup [Perm]
getPermsGateways tok = do
  info "Get gateways permissions"
  getPerms tok (getPermReq Nothing gatewayScopes)

-- | Throws error 403 if there is no permission for the resource under the corresponding scope.
checkPermResource :: Maybe Token -> W.Scope -> W.PermResource -> Waziup ()
checkPermResource tok scp rid = do
  perms <- getPerms tok (getPermReq (Just rid) [scp])
  if isPermittedResource scp rid perms 
    then return ()
    else throwError err403 {errBody = "Forbidden: Cannot access resource"}

-- | Check that `perms` contain a permission for the resource with the corresponding scope.
isPermittedResource :: W.Scope -> W.PermResource -> [Perm] -> Bool
isPermittedResource scp rid perms = any isPermitted perms where
  isPermitted :: Perm -> Bool
  isPermitted (Perm (Just rid') scopes) = rid == rid' && scp `elem` scopes
  isPermitted (Perm Nothing scopes) = scp `elem` scopes

-- | Retrieve permissions
getPerms :: Maybe Token -> PermReq -> Waziup [Perm]
getPerms tok permReq = do
  let username = case tok of
       Just t -> getUsername t
       Nothing -> "guest"
  useCache <- view (waziupConfig.serverConf.cacheActivated)
  if useCache
    then getPermsWithCache tok username permReq
    else do
      kcPerms <- liftKeycloak tok $ getPermissions [permReq]
      return $ map getPerm kcPerms

-- * Permission resources

createResource :: Maybe Token -> PermResource -> Maybe Visibility -> Maybe KC.Username -> Waziup ResourceId
createResource tok permRes vis muser = do
  --creating a new resource in Keycloak invalidates the cache
  invalidateCache permRes
  let (resTyp, scopes) = case permRes of
       PermDeviceId _  -> ("Device" , deviceScopes)
       PermGatewayId _ -> ("Gateway", gatewayScopes)
       PermProjectId _ -> ("Project", projectScopes)
  let attrs = if (isJust vis) then [KC.Attribute "visibility" [fromVisibility $ fromJust vis]] else []
  let username = case muser of
       Just user -> user          --if username is provided, use it. 
       Nothing -> case tok of
         Just t -> getUsername t  --Else, if token is provided, extract the username.
         Nothing -> "guest"       --Finally, use "guest" as a username.
  let kcres = KC.Resource {
         resId      = (Just $ getKCResourceId $ permRes),
         resName    = (unResId $ getKCResourceId $ permRes),
         resType    = Just resTyp,
         resUris    = [],
         resScopes  = map (\s -> KC.Scope Nothing (fromScope s)) scopes,
         resOwner   = Owner Nothing (Just username),
         resOwnerManagedAccess = True,
         resAttributes = attrs}
  liftKeycloak' $ getClientAuthToken >>= KC.createResource kcres

deleteResource :: Maybe Token -> PermResource -> Waziup ()
deleteResource _ pr = do
  --invalidate all cache
  invalidateCache pr
  --delete all resources
  liftKeycloak' $ getClientAuthToken >>= KC.deleteResource (getKCResourceId pr)

updateResource :: Maybe Token -> PermResource -> Maybe Visibility -> Maybe KC.Username -> Waziup ResourceId
updateResource = Waziup.Auth.createResource

--Cache management

-- Get perms using cache management
getPermsWithCache :: Maybe Token -> KC.Username -> PermReq -> Waziup [Perm]
getPermsWithCache tok username permReq = do
  debug "get perms with cache"
  res <- getCachedPerms username permReq 
  case res of
    -- return cached perms
    Just ps  -> return ps
    -- no cached permission or outdated permission; getting from Keycloak and updating cache
    Nothing -> do
      kcPerms <- liftKeycloak tok $ getPermissions [permReq]
      let perms = map getPerm kcPerms
      writeCache username permReq perms 
      return perms 

-- get perms from cache
getCachedPerms :: KC.Username -> PermReq -> Waziup (Maybe [Perm])
getCachedPerms username perm = do
  permsTV <- view permCache
  permsM <- liftIO $ atomically $ readTVar permsTV
  now <- liftIO getCurrentTime
  --look up username in cache
  debug $ "lookup cache for " ++ (show username)
  case M.lookup (username, perm) permsM of
    Just (perms, retrievedTime) -> do
      debug "cache found"
      cacheDuration <- view (waziupConfig.serverConf.cacheValidDuration)
      --checl if not expired
      if now < addUTCTime cacheDuration retrievedTime
        --return permissions
        then do
          debug "cache valid"
          return $ Just perms
        --else get perms from Keycloak and update the cache
        else do
          debug "cache expired"
          return Nothing
    Nothing -> do
      debug $ "request not found in cache"
      return Nothing

-- Update the cache
writeCache :: KC.Username -> PermReq -> [Perm] -> Waziup ()
writeCache username permReq perms = do
  debug $ "Write cache: " ++ (show username) ++ " " ++ (show permReq) ++ " : " ++ (show perms)
  permsTV <- view permCache 
  cache <- liftIO $ atomically $ readTVar permsTV
  now <- liftIO getCurrentTime
  let cache' = cache & at (username, permReq) ?~ (perms, now)
  liftIO $ atomically $ writeTVar permsTV cache'
  return ()

-- invalidate cache on resource actions (create, update, delete)
invalidateCache :: PermResource -> Waziup ()
invalidateCache rid = do
  debug $ "Invalidate cache for " ++ (show rid)
  permsTV <- view permCache
  perms <- liftIO $ atomically $ readTVar permsTV
  let rid' = getKCResourceId rid
  debug $ "Current cache: " ++ (convertString $ showPermCache perms)
  let perms' = M.filterWithKey (\(_, req) _ -> isValidPermReq rid' req) perms
  debug $ "Filtered cache: " ++ (convertString $ showPermCache perms')
  liftIO $ atomically $ writeTVar permsTV perms'

-- filter perm reqs based on resource ID
isValidPermReq :: ResourceId -> PermReq -> Bool
isValidPermReq rid (PermReq (Just rid') _ )  = rid /= rid' -- Remove "resource" perm request
isValidPermReq _   (PermReq _           [])  = False       -- Remove "all scopes" perm requests
isValidPermReq _   (PermReq Nothing     _ )  = False       -- Remove "all resources" perm requests

-- | Create a perm request
getPermReq :: Maybe PermResource -> [W.Scope] -> PermReq
getPermReq pr scopes = PermReq (getKCResourceId <$> pr) (map fromScope scopes)

showPermCache :: PermCache -> String
showPermCache pc = concatMap showCacheEntry (M.toList pc) where

showCacheEntry :: (CacheIndex, CacheValue) -> String
showCacheEntry (i, v) =  (showCacheIndex i) <> " : " <> (showCacheValue v) <> "/n" 

showCacheIndex :: CacheIndex -> String
showCacheIndex (username, permReq) = (convertString username) <> ", " <> (show permReq)

showCacheValue :: CacheValue -> String
showCacheValue (perms, time) = (show perms) <> " @ " <> (show time)

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Auth" s
info  s = liftIO $ infoM    "Auth" s
warn  s = liftIO $ warningM "Auth" s
err   s = liftIO $ errorM   "Auth" s

