{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Gateways where

import           Waziup.Types
import           Waziup.API
import           Waziup.Utils
import           Waziup.Devices hiding (info, warn, debug, err, Scope) 
import           Waziup.Auth hiding (info, warn, debug, err, Scope) 
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Control.Monad
import           Control.Exception
import           Data.String.Conversions
import           Servant
import           System.Log.Logger
import           Database.MongoDB as DB
import           Data.Aeson as JSON
import           Data.Bson as BSON
import           Data.AesonBson
import           Data.Maybe
import           Data.Typeable
import           Data.Time
import           Data.Text hiding (find, map, filter)
import           Safe

-- * Projects API

getGateways :: Maybe Token -> Maybe Bool -> Waziup [Gateway]
getGateways tok mfull = do
  info "Get gateways"
  gws <- runMongo $ do
    docs <- rest =<< find (select [] "gateways")
    return $ catMaybes $ map (resultToMaybe . fromJSON . Object . replaceKey "_id" "id" . aesonify) docs
  info $ "Got gateways: " ++ (show gws)
  gws' <- case mfull of
    Just True -> mapM (getFullGateway tok) gws 
    _ -> return gws
  gs <- getPermsGateways tok
  let gws'' = filter (checkPermResource' GatewaysView gs . unGatewayId . gwId) gws'
  return gws''

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
  res <- case eres of
    Right a -> return a
    Left (CompoundFailure [WriteFailure _ e _]) -> throwError err422 {errBody = "Gateway ID already exists"}
  createResource' tok
                  (Just $ ResourceId $ convertString $ "gateway-" <> (unGatewayId $ gwId g))
                  (unGatewayId $ gwId g)
                  "Gateway"
                  [GatewaysView, GatewaysUpdate, GatewaysDelete]
                  (if (isJust $ gwVisibility g) then [KC.Attribute "visibility" [fromVisibility $ fromJust $ gwVisibility g]] else [])
  return NoContent

getGateway :: Maybe Token -> GatewayId -> Maybe Bool -> Waziup Gateway
getGateway tok gid mfull = do
  info "Get gateway"
  mg <- runMongo $ getGatewayMongo gid 
  g <- case mg of
    Just g -> return g
    Nothing -> throwError err404 {errBody = "Cannot get gateway: id not found"}
  checkPermResource tok GatewaysView (unGatewayId gid)
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
  checkPermResource tok GatewaysDelete (unGatewayId gid) 
  debug "Delete Keycloak resource"
  liftKeycloak tok $ deleteResource (ResourceId $ "gateway-" <> unGatewayId gid)
  res <- runMongo $ deleteGatewayMongo gid
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot delete project: id not found"}

putGatewayTunnel :: Maybe Token -> GatewayId -> Int -> Waziup NoContent
putGatewayTunnel = undefined

deleteGatewayTunnel :: Maybe Token -> GatewayId -> Waziup NoContent
deleteGatewayTunnel = undefined

putHealth :: Maybe Token -> GatewayId -> Waziup NoContent
putHealth tok (GatewayId gid)= do
  checkPermResource tok GatewaysUpdate gid 
  currentTime <- liftIO $ getCurrentTime
  runMongo $ modify (select ["_id" =: gid] "gateways") [ "$set" := Doc ["date_modified" := val currentTime]]
  return NoContent
  
putGatewayName :: Maybe Token -> GatewayId -> Text -> Waziup NoContent
putGatewayName tok gid name = do
  info "Put gateway name"
  checkPermResource tok GatewaysUpdate (unGatewayId gid)
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
  checkPermResource tok GatewaysUpdate (unGatewayId gid)
  res <- runMongo $ do 
    let sel = ["_id" =: unGatewayId gid]
    mdoc <- findOne (select sel "gateways")
    case mdoc of
       Just _ -> do
         modify (select sel "gateways") [ "$set" := Doc ["owner" := val owner, "visibility" := val ("private" :: String)]]
         return True
       _ -> return False 
  info "Delete Keycloak resource"
  liftKeycloak tok $ deleteResource (ResourceId $ "gateway-" <> unGatewayId gid)
  createResource'' tok
                  (Just $ ResourceId $ convertString $ "gateway-" <> (unGatewayId gid))
                  (unGatewayId gid)
                  "Gateway"
                  [GatewaysView, GatewaysUpdate, GatewaysDelete]
                  [KC.Attribute "visibility" [fromVisibility Private]]
                  owner
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

