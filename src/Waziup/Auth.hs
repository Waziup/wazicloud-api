{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Auth where

import           Waziup.Types as W
import           Waziup.Utils as U
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Control.Monad
import           Data.Maybe
import           Data.Map as M hiding (map, mapMaybe, filter, delete)
import           Data.Text as T hiding (map, filter, foldl, any)
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
import           Database.MongoDB as DB hiding (at, value, Limit, Array, lookup, Value, Null, (!?))
import           Data.AesonBson
import           Control.Lens
import           Control.Concurrent.STM
import           Control.Monad

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
  getPerms tok (PermReq Nothing (map fromScope deviceScopes))

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsProjects :: Maybe Token -> Waziup [Perm]
getPermsProjects tok = do
  info "Get projects permissions"
  getPerms tok (PermReq Nothing (map fromScope projectScopes))

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsGateways :: Maybe Token -> Waziup [Perm]
getPermsGateways tok = do
  info "Get gateways permissions"
  getPerms tok (PermReq Nothing (map fromScope gatewayScopes))

-- | Throws error 403 if `perms` if there is no permission for the resource under the corresponding scope.
checkPermResource :: Maybe Token -> W.Scope -> W.PermResource -> Waziup ()
checkPermResource tok scope rid = do
  perms <- getPerms tok (PermReq (Just $ getKCResourceId rid) [fromScope scope])
  if isPermittedResource scope rid perms 
    then return ()
    else throwError err403 {errBody = "Forbidden: Cannot access resource"}

-- | Check that `perms` contain a permission for the resource with the corresponding scope.
isPermittedResource :: W.Scope -> W.PermResource -> [Perm] -> Bool
isPermittedResource scope rid perms = any (isPermitted rid scope) perms where
  isPermitted :: PermResource -> W.Scope -> Perm -> Bool
  isPermitted rid scope (Perm (Just rid') scopes) = rid == rid' && scope `elem` scopes
  isPermitted rid scope (Perm Nothing    scopes) = False


getPerms :: Maybe Token -> PermReq -> Waziup [Perm]
getPerms tok permReq = do
  let username = case tok of
       Just t -> getUsername t
       Nothing -> "guest"
  res <- getCachedPerms username permReq 
  case res of
    Just ps  -> return ps
    --No cached permission or outdated permission; getting from Keycloak and updating cache
    Nothing -> do
      res <- U.try $ liftKeycloak tok $ getPermissions [permReq]
      case res of
        Right kcPerms -> do
          let perms = map getPerm kcPerms
          writeCache username permReq perms 
          return perms 
        Left _ -> do 
          writeCache username permReq []
          return [] 

-- * Permission resources

createResource :: Maybe Token -> PermResource -> Maybe Visibility -> Maybe KC.Username -> Waziup ResourceId
createResource tok permRes vis muser = do
  --creating a new resource in Keycloak invalidates the cache
  invalidateCache
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
  liftKeycloak tok $ KC.createResource kcres

deleteResource :: Maybe Token -> PermResource -> Waziup ()
deleteResource tok pr = do
  --invalidate all cache
  invalidateCache
  --delete all resources
  liftIO $ flip runKeycloak defaultKCConfig $ do
    tok2 <- KC.getClientAuthToken
    KC.deleteResource (getKCResourceId pr) tok2
  return ()

-- | Update a resource
updateResource :: Maybe Token -> KC.Resource ->  Waziup ResourceId
updateResource tok res = do 
  --invalidate all cache
  invalidateCache
  liftKeycloak tok $ KC.createResource res 


--Cache management

getCachedPerms :: KC.Username -> PermReq -> Waziup (Maybe [Perm])
getCachedPerms username perm = do
  debug "getCachedPerms"
  permsTV <- view permCache
  permsM <- liftIO $ atomically $ readTVar permsTV
  now <- liftIO getCurrentTime
  --look up username in cache
  debug $ "lookup cache for " ++ (show username)
  case M.lookup (username, perm) permsM of
    Just (perms, retrievedTime) -> do
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
      debug $ "request not found in cache"
      return Nothing

writeCache :: KC.Username -> PermReq -> [Perm] -> Waziup ()
writeCache username permReq perms = do
  permsTV <- view permCache
  permCache <- liftIO $ atomically $ readTVar permsTV
  now <- liftIO getCurrentTime
  debug "update cache"
  let permsCache' = permCache & at (username, permReq) ?~ (perms, now)
  liftIO $ atomically $ writeTVar permsTV permsCache'
  return ()

invalidateCache :: Waziup ()
invalidateCache = do
  permsTV <- view permCache
  liftIO $ atomically $ writeTVar permsTV M.empty 
  

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Auth" s
info  s = liftIO $ infoM    "Auth" s
warn  s = liftIO $ warningM "Auth" s
err   s = liftIO $ errorM   "Auth" s

