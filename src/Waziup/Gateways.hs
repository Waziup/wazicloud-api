{-# LANGUAGE OverloadedStrings #-}

module Waziup.Gateways where

import           Waziup.Types
import           Waziup.API
import           Waziup.Utils
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           Control.Monad.IO.Class
import           Data.String.Conversions
import           Servant
import           System.Log.Logger
import           Database.MongoDB as DB
import           Data.Aeson as JSON
import           Data.Bson as BSON
import           Data.AesonBson
import           Safe

-- * Projects API

getGateways :: Maybe Token -> Waziup [Gateway]
getGateways tok = do
  info "Get gateways"
  runMongo $ do
    docs <- rest =<< find (select [] "gateways")
    let res = sequence $ map (fromJSON . Object . aesonify) docs
    case res of
      JSON.Success a -> return a
      JSON.Error _ -> return []

postGateway :: Maybe Token -> Gateway -> Waziup GatewayId
postGateway tok p = do
  info "Post gateway"
  runMongo $ do
    let ob = case toJSON p of
         JSON.Object o -> o
         _ -> error "Wrong object format"
    res <- insert "gateways" (bsonify ob)
    return $ GatewayId $ convertString $ show res

getGateway :: Maybe Token -> GatewayId -> Waziup Gateway
getGateway tok gid = do
  info "Get gateway"
  mp <- runMongo $ getGatewayMongo gid 
  case mp of
    Just p -> return p
    Nothing -> throwError err404 {errBody = "Cannot get gateway: id not found"}

deleteGateway :: Maybe Token -> GatewayId -> Waziup NoContent
deleteGateway tok pid = do
  info "Delete gateway"
  res <- runMongo $ deleteGatewayMongo pid
  if res
    then return NoContent
    else throwError err404 {errBody = "Cannot delete project: id not found"}

putGatewayTunnel :: Maybe Token -> GatewayId -> Int -> Waziup NoContent
putGatewayTunnel = undefined

deleteGatewayTunnel :: Maybe Token -> GatewayId -> Waziup NoContent
deleteGatewayTunnel = undefined


-- putProjectDevices :: Maybe Token -> ProjectId -> [DeviceId] -> Waziup NoContent
--  putProjectDevices tok pid ids = do
--    info "Put project devices"
--    res <- runMongo $ putProjectDevicesMongo pid ids
--    if res
--      then return NoContent
--      else throwError err404 {errBody = "Cannot update project: id not found"}


-- * Helpers

getGatewayMongo :: GatewayId -> Action IO (Maybe Gateway)
getGatewayMongo (GatewayId pid) = do
  case readMay $ convertString pid of
    Just id -> do
      mdoc <- findOne (select ["_id" =: (ObjId id)] "gateways")
      case (fromJSON . Object . aesonify <$> mdoc) of
         Just (JSON.Success a) -> return $ Just a
         _ -> return Nothing
    Nothing -> return Nothing

deleteGatewayMongo :: GatewayId -> Action IO Bool 
deleteGatewayMongo (GatewayId pid) = do
  let sel = ["_id" =: (ObjId $ read $ convertString pid)]
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

