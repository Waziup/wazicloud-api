{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Gateways where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Devices hiding (info, warn, debug, err) 
import           Waziup.Auth hiding (info, warn, debug, err) 
import           Keycloak (Token, Username, getUsername)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Control.Monad
import           Data.String.Conversions
import           Servant
import           System.Log.Logger
import           Database.MongoDB as DB hiding (Username)
import           Data.Aeson as JSON
import           Data.AesonBson
import           Data.Maybe
import           Data.Time
import           Data.Text hiding (find, map, filter)

-- * Projects API

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsGateways :: Maybe Token -> Waziup [Perm]
getPermsGateways tok = do
  info "Get gateways permissions"
  gws <- getAllGateways
  return $ map (\g -> getPerm tok (PermGateway g) gatewayScopes) gws

getGateways :: Maybe Token -> Maybe Bool -> Waziup [Gateway]
getGateways tok mfull = do
  info "Get gateways"
  gws <- getAllGateways
  info $ "Got gateways: " ++ (show gws)
  gws' <- case mfull of
    Just True -> mapM (getFullGateway tok) gws 
    _ -> return gws
  let gws'' = filter (\g -> isPermitted tok (PermGateway g) GatewaysView) gws'
  return gws''

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
    insert "gateways" (bsonifyBound ob)
  case eres of
    Right _ -> return ()
    Left (CompoundFailure [WriteFailure _ _ _]) -> throwError err422 {errBody = "Gateway ID already exists"}
    Left e -> throwError err500 {errBody = (convertString $ show e)}
  return NoContent

getGateway :: Maybe Token -> GatewayId -> Maybe Bool -> Waziup Gateway
getGateway tok gid mfull = do
  info "Get gateway"
  mg <- runMongo $ getGatewayMongo gid 
  g <- case mg of
    Just g -> return g
    Nothing -> throwError err404 {errBody = "Cannot get gateway: id not found"}
  checkPermResource tok GatewaysView (PermGateway g)
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
  g <- getGateway tok gid Nothing
  checkPermResource tok GatewaysDelete (PermGateway g) 
  res <- runMongo $ deleteGatewayMongo gid
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot delete project: id not found"}

putHeartbeat :: Maybe Token -> GatewayId -> Waziup NoContent
putHeartbeat tok gid = do
  g <- getGateway tok gid Nothing
  checkPermResource tok GatewaysUpdate (PermGateway g) 
  currentTime <- liftIO $ getCurrentTime
  runMongo $ modify (select ["_id" =: unGatewayId gid] "gateways") [ "$set" := Doc ["date_modified" := val currentTime]]
  return NoContent
  
putGatewayName :: Maybe Token -> GatewayId -> Text -> Waziup NoContent
putGatewayName tok gid name = do
  info "Put gateway name"
  g <- getGateway tok gid Nothing
  checkPermResource tok GatewaysUpdate (PermGateway g)
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
putGatewayOwner :: Maybe Token -> GatewayId -> Username -> Waziup NoContent
putGatewayOwner tok gid owner = do
  info "Put gateway owner"
  g <- getGateway tok gid Nothing
  checkPermResource tok GatewaysUpdate (PermGateway g)
  void $ runMongo $ do 
    let sel = ["_id" =: unGatewayId gid]
    mdoc <- findOne (select sel "gateways")
    case mdoc of
       Just _ -> do
         modify (select sel "gateways") [ "$set" := Doc ["owner" := val owner, "visibility" := val ("private" :: String)]]
         return True
       _ -> return False 
  return NoContent

putGatewayLocation :: Maybe Token -> GatewayId -> Location -> Waziup NoContent
putGatewayLocation mtok gid loc = do
  info $ "Put gateway location: " ++ (show loc)
  g <- getGateway mtok gid Nothing
  checkPermResource mtok GatewaysUpdate (PermGateway g)
  res <- runMongo $ do 
    let sel = ["_id" =: unGatewayId gid]
    mdoc <- findOne (select sel "gateways")
    case mdoc of
       Just _ -> do
         modify (select sel "gateways") [ "$set" := Doc ["location" := (bsonifyValue bound $ toJSON loc)]]
         return True
       _ -> return False 
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot update gateway: id not found"}
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

