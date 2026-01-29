{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Gateways where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Devices hiding (info, warn, debug, err) 
import           Waziup.Auth hiding (info, warn, debug, err) 
import           Keycloak hiding (User(..))
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Control.Monad
import           Control.Lens hiding ((.=))
import           Servant
import           System.Log.Logger
import           System.FilePath ((</>))
import           System.Cmd
import           Database.MongoDB as DB hiding (Username)
import           Data.String.Conversions
import           Data.Aeson as JSON
import           Data.AesonBson
import           Data.Maybe
import           Data.Time
import qualified Data.List as L
import           Data.Text hiding (find, map, filter)
import           Paths_Waziup_Servant
import qualified Data.Text.IO as DTIO
import           Network.Wreq as W (get, post, delete, defaults, responseBody)
import           Network.HTTP.Client as HC hiding (responseBody)
import qualified Network.HTTP.Types.Status as HT

-- * Projects API

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsGateways :: AuthUser -> Waziup [Perm]
getPermsGateways tok = do
  info "Get gateways permissions"
  gws <- getAllGateways
  let perms = map (\g -> getPerm tok (PermGateway g) gatewayScopes) gws
  return $ filter (\(Perm _ scps) -> not $ L.null scps) perms

getGateways :: AuthUser -> Maybe Bool -> Waziup [Gateway]
getGateways tok mfull = do
  info "Get gateways"
  gws <- getAllGateways
  info $ "Got gateways: " ++ (show gws)
  gws' <- case mfull of
    Just True -> mapM (getFullGateway tok) gws 
    _ -> return gws
  let gws'' = filter (\g -> isNothing $ isPermitted tok (PermGateway g) GatewaysView) gws'
  return gws''

getAllGateways :: Waziup [Gateway]
getAllGateways = do
  runMongo $ do
    docs <- rest =<< find (select [] "gateways")
    return $ catMaybes $ map (resultToMaybe . fromJSON . Object . replaceKey "_id" "id" . aesonify) docs

postGateway :: AuthUser -> Gateway -> Waziup NoContent
postGateway au g = do
  info "Post gateway"
  debug $ "gate: " ++ (show g)
  currentTime <- liftIO $ getCurrentTime
  let g' = g {gwOwner = Just $ userUsername $ getAuthUser au,
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
  -- Create VPN client
  (VpnConfig host) <- view (waziupConfig.vpnConf)
  let path = convertString $ host <> "/v1/clients"
  let dat = VPNClient ("gateway-" <> (unGatewayId $ gwId g)) Nothing Nothing
  info $ "Issuing VPN server POST " ++ (show path) ++ " " ++ (show dat)
  eRes <- liftIO $ C.try $ W.post path (toJSON dat)
  case eRes of 
    Right res -> do
      return NoContent
    Left (HttpExceptionRequest _ (StatusCodeException resp er)) -> do
      case HT.statusCode (HC.responseStatus resp) of
        409 -> do --Client already exists
          warn $ "VPN Server HTTP error: " ++ (show er)
          return NoContent 
        _ -> do
          warn $ "VPN Server HTTP error: " ++ (show er)
          throwError err500 {errBody = convertString $ "VPN Server error: " ++ (show er)}
    --All other errors
    Left (HttpExceptionRequest _ er) -> do
      warn $ "VPN Server HTTP error: " ++ (show er)
      throwError err500 {errBody = convertString $ "VPN Server error: " ++ (show er)}

getGateway :: AuthUser -> GatewayId -> Maybe Bool -> Waziup Gateway
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

getFullGateway :: AuthUser -> Gateway -> Waziup Gateway
getFullGateway tok g = do
  devs <- getDevices tok (Just ("gateway_id==" <> (convertString $ unGatewayId $ gwId g))) Nothing Nothing 
  return $ g {gwDevices = Just devs}

deleteGateway :: AuthUser -> GatewayId -> Waziup NoContent
deleteGateway tok gid = do
  info "Delete gateway"
  g <- getGateway tok gid Nothing
  checkPermResource tok GatewaysDelete (PermGateway g) 
  res <- runMongo $ deleteGatewayMongo gid
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot delete project: id not found"}
  -- Delete VPN client
  (VpnConfig host) <- view (waziupConfig.vpnConf)
  let path = convertString $ host <> "/v1/clients/" <> (unGatewayId gid)
  info $ "Issuing VPN server DELETE " ++ (show path) 
  eRes <- liftIO $ C.try $ W.delete path
  case eRes of 
    Right res -> do
      return NoContent
    Left (HttpExceptionRequest _ (StatusCodeException resp er)) -> do
      case HT.statusCode (HC.responseStatus resp) of
        404 -> do --Client not found
          warn $ "VPN Server HTTP error: " ++ (show er)
          return NoContent 
        _ -> do
          warn $ "VPN Server HTTP error: " ++ (show er)
          throwError err500 {errBody = convertString $ "VPN Server error: " ++ (show er)}
    Left (HttpExceptionRequest _ er) -> do
      warn $ "VPN Server HTTP error: " ++ (show er)
      throwError err500 {errBody = convertString $ "VPN Server error: " ++ (show er)}


putHeartbeat :: AuthUser -> GatewayId -> Waziup NoContent
putHeartbeat tok gid = do
  g <- getGateway tok gid Nothing
  checkPermResource tok GatewaysUpdate (PermGateway g) 
  currentTime <- liftIO $ getCurrentTime
  runMongo $ modify (select ["_id" =: unGatewayId gid] "gateways") [ "$set" := Doc ["date_modified" := val currentTime]]
  return NoContent
  
putGatewayName :: AuthUser -> GatewayId -> Text -> Waziup NoContent
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
putGatewayOwner :: AuthUser -> GatewayId -> Username -> Waziup NoContent
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

putGatewayLocation :: AuthUser -> GatewayId -> Location -> Waziup NoContent
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

getGatewayVPNFile :: AuthUser -> GatewayId -> Waziup VPNFile
getGatewayVPNFile mtok gid = do
  info $ "get gateway VPN file"
  g <- getGateway mtok gid Nothing
  checkPermResource mtok GatewaysUpdate (PermGateway g)
  (VpnConfig host) <- view (waziupConfig.vpnConf)
  let path = convertString $ host <> "/v1/clients/gateway-" <> (unGatewayId gid) <> "/ovpn"
  info $ "Issuing VPN server GET " ++ (show path) 
  eRes <- liftIO $ C.try $ W.get path
  case eRes of 
    Right res -> do
      return $ convertString $ fromJust $ res ^? responseBody
    Left (HttpExceptionRequest _ (StatusCodeException _ er)) -> do
      warn $ "VPN Server HTTP error: " ++ (show er)
      throwError err500 {errBody = convertString $ "VPN Server error: " ++ (show er)}
    Left (HttpExceptionRequest _ er) -> do
      warn $ "VPN Server HTTP error: " ++ (show er)
      throwError err500 {errBody = convertString $ "VPN Server error: " ++ (show er)}

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
       DB.delete (select sel "gateways")
       return True
     _ -> return False 

--toGatewayMongo :: Gateway -> JSON.Object
--toGatewayMongo (Gateway id name owner vis loc dc dm devs conn ls) =
--  object ["_id" .= object ["id" .= id, "owner" .= owner]
--          "name" .= toJSON name,
--          ""] 

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

