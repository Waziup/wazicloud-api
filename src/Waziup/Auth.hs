{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Auth where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.String.Conversions
import           Data.Maybe
import           Data.Time
import           Data.Cache as C
import           Data.Cache.Internal as CI
import qualified Data.Map as M
import           Keycloak as KC
import           Servant
import           System.Log.Logger
import           Waziup.Types as W
import           Waziup.Utils as U

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
    then do
      cache <- view permCache
      debug "fetchWithCache"
      fst <$> fetchWithCache cache (username, permReq) (fetch tok)
    else do
      kcPerms <- liftKeycloak tok $ getPermissions [permReq]
      return $ map getPerm kcPerms

fetch :: Maybe Token -> CacheIndex -> Waziup CacheValue
fetch tok (_, req) = do
  kcPerms <- liftKeycloak tok $ getPermissions [req]
  let perms = map getPerm kcPerms
  now <- liftIO getCurrentTime
  return (perms, now)

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

-- invalidate cache on resource actions (create, update, delete)
invalidateCache :: PermResource -> Waziup ()
invalidateCache rid = do
  debug $ "Invalidate cache for " ++ (show rid)
  cache <- view permCache
  let rid' = getKCResourceId rid
  liftIO $ showCache cache
  liftIO $ C.filterWith (\(_, req) -> isValidPermReq rid' req) cache

-- filter perm reqs based on resource ID
isValidPermReq :: ResourceId -> PermReq -> Bool
isValidPermReq rid (PermReq (Just rid') _ )  = rid /= rid' -- Remove "resource" perm request
isValidPermReq _   (PermReq _           [])  = False       -- Remove "all scopes" perm requests
isValidPermReq _   (PermReq Nothing     _ )  = False       -- Remove "all resources" perm requests

-- | Create a perm request
getPermReq :: Maybe PermResource -> [W.Scope] -> PermReq
getPermReq pr scopes = PermReq (getKCResourceId <$> pr) (map fromScope scopes)

showCache :: Cache CacheIndex CacheValue -> IO ()
showCache c = do
  ks <- keys c
  forM_ ks $ \k -> do
    (Just res) <- C.lookup c k 
    debug $ showCacheEntry (k, res)

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

