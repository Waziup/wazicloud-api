{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Gateways where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Devices hiding (info, warn, debug, err) 
import           Waziup.Auth hiding (info, warn, debug, err) 
import           Keycloak as KC hiding (info, warn, debug, err, Scope, createResource, updateResource, deleteResource)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Control.Monad
import           Data.String.Conversions
import           Servant
import           System.Log.Logger
import           Database.MongoDB as DB
import           Data.Aeson as JSON
import           Data.AesonBson
import           Data.Maybe
import           Data.Time
import           Data.Text hiding (find, map, filter)

-- * Projects API

getGateways :: Maybe Token -> Maybe Bool -> Waziup [Gateway]
getGateways tok mfull = do
  info "Get gateways"
  gws <- getAllGateways
  info $ "Got gateways: " ++ (show gws)
  gws' <- case mfull of
    Just True -> mapM (getFullGateway tok) gws 
    _ -> return gws
  gs <- getPerms tok gatewaysViewReq
  let gws'' = filter (\g -> isPermittedResource GatewaysView (PermGatewayId $ gwId g) gs) gws'
  return gws''

gatewaysViewReq :: PermReq
gatewaysViewReq = PermReq Nothing [fromScope GatewaysView]

getAllGateways :: Waziup [Gateway]
getAllGateways = do
  runMongo $ do
    docs <- rest =<< find (select [] "gateways")
    return $ catMaybes $ map (resultToMaybe . fromJSON . Object . replaceKey "_id" "id" . aesonify) docs

postGateway :: Maybe Token -> Gateway -> Waziup NoContent
postGateway tok g = do
  info "Post gateway"
  let username = case tok of
       Just t -> getUsername t
       Nothing -> "guest"
  debug $ "gate: " ++ (show g)
  currentTime <- liftIO $ getCurrentTime
  let g' = g {gwOwner = Just username,
              gwDateCreated = Just currentTime,
              gwDateModified = Just currentTime} 
  eres <- C.try $ runMongo $ do
    let ob = case toJSON g' of
         JSON.Object o -> replaceKey "id" "_id" o
         _ -> error "Wrong object format"
    debug $ "id: " ++ (show ob)
    insert "gateways" (bsonify ob)
  case eres of
    Right _ -> return ()
    Left (CompoundFailure [WriteFailure _ _ _]) -> throwError err422 {errBody = "Gateway ID already exists"}
    Left e -> throwError err500 {errBody = (convertString $ show e)}
  void $ createResource tok (PermGatewayId $ gwId g) (gwVisibility g) (Just username)
  return NoContent

getGateway :: Maybe Token -> GatewayId -> Maybe Bool -> Waziup Gateway
getGateway tok gid mfull = do
  info "Get gateway"
  mg <- runMongo $ getGatewayMongo gid 
  g <- case mg of
    Just g -> return g
    Nothing -> throwError err404 {errBody = "Cannot get gateway: id not found"}
  checkPermResource tok GatewaysView (PermGatewayId gid)
  case mfull of
    Just True -> getFullGateway tok g
    _ -> return g

getFullGateway :: Maybe Token -> Gateway -> Waziup Gateway
getFullGateway tok g = do
  devs <- getDevices tok (Just ("gateway_id==" <> (convertString $ unGatewayId $ gwId g))) Nothing Nothing 
  return $ g {gwDevices = Just devs}

deleteGateway :: Maybe Token -> GatewayId -> Waziup NoContent
deleteGateway tok gid = do
  info "Delete gateway"
  mg <- runMongo $ getGatewayMongo gid
  when (isNothing mg) $ throwError err404 {errBody = "Cannot get gateway: id not found"}
  checkPermResource tok GatewaysDelete (PermGatewayId gid) 
  debug "Delete Keycloak resource"
  deleteResource tok (PermGatewayId gid)
  res <- runMongo $ deleteGatewayMongo gid
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot delete project: id not found"}

putHeartbeat :: Maybe Token -> GatewayId -> Waziup NoContent
putHeartbeat tok gid = do
  checkPermResource tok GatewaysUpdate (PermGatewayId gid) 
  currentTime <- liftIO $ getCurrentTime
  runMongo $ modify (select ["_id" =: unGatewayId gid] "gateways") [ "$set" := Doc ["date_modified" := val currentTime]]
  return NoContent
  
putGatewayName :: Maybe Token -> GatewayId -> Text -> Waziup NoContent
putGatewayName tok gid name = do
  info "Put gateway name"
  checkPermResource tok GatewaysUpdate (PermGatewayId gid)
  res <- runMongo $ do 
    let sel = ["_id" =: unGatewayId gid]
    mdoc <- findOne (select sel "gateways")
    case mdoc of
       Just _ -> do
         modify (select sel "gateways") [ "$set" := Doc ["name" := val name]]
         return True
       _ -> return False 
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot update gateway: id not found"}

-- Change the owner of a gateway. The gateway will also automatically be passed as private.
putGatewayOwner :: Maybe Token -> GatewayId -> KC.Username -> Waziup NoContent
putGatewayOwner tok gid owner = do
  info "Put gateway owner"
  checkPermResource tok GatewaysUpdate (PermGatewayId gid)
  void $ runMongo $ do 
    let sel = ["_id" =: unGatewayId gid]
    mdoc <- findOne (select sel "gateways")
    case mdoc of
       Just _ -> do
         modify (select sel "gateways") [ "$set" := Doc ["owner" := val owner, "visibility" := val ("private" :: String)]]
         return True
       _ -> return False 
  info "Delete Keycloak resource"
  deleteResource tok (PermGatewayId gid)
  void $ createResource tok
                 (PermGatewayId gid)
                 (Just Private)
                 (Just owner)
  return NoContent


-- * Helpers

getGatewayMongo :: GatewayId -> Action IO (Maybe Gateway)
getGatewayMongo (GatewayId pid) = do
  mdoc <- findOne (select ["_id" =: pid] "gateways")
  debug $ "get:   " ++ (show mdoc)
  case (fromJSON . Object . replaceKey "_id" "id" . aesonify <$> mdoc) of
     Just (JSON.Success a) -> return $ Just a
     _ -> return Nothing

deleteGatewayMongo :: GatewayId -> Action IO Bool 
deleteGatewayMongo (GatewayId pid) = do
  let sel = ["_id" =: pid]
  mdoc <- findOne (select sel "gateways")
  case mdoc of
     Just _ -> do
       delete (select sel "gateways")
       return True
     _ -> return False 

-- putProjectGatewaysMongo :: ProjectId -> [GatewayId] -> Action IO Bool
-- putProjectGatewaysMongo (ProjectId pid) gids = do
--   let sel = ["_id" =: (ObjId $ read $ convertString pid)]
--   mdoc <- findOne (select sel "projects")
--   case mdoc of
--      Just _ -> do
--        modify (select sel "projects") [ "$set" := Doc ["gateways" := val (map unGatewayId gids)]]
--        return True
--      _ -> return False 


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Gateway" s
info  s = liftIO $ infoM    "Gateway" s
warn  s = liftIO $ warningM "Gateway" s
err   s = liftIO $ errorM   "Gateway" s

