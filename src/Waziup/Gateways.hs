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
import           Data.String.Conversions
import           Servant
import           System.Log.Logger
import           Database.MongoDB as DB hiding (Username)
import           Data.Aeson as JSON
import           Data.AesonBson
import           Data.Maybe
import           Data.Time
import qualified Data.List as L
import           Data.Text hiding (find, map, filter)

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (try, IOException)
import System.IO (withFile, IOMode(..))
import System.Directory (renameFile)
import System.FilePath ((</>))
import qualified Data.Text as T

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
  return NoContent

getAllGatewaysInVPN :: IO (Either String T.Text)
getAllWazigate = do
  result <- try (TIO.readFile "/app/openvpn_clients_dynamic.hosts") :: IO (Either IOException T.Text)
  case result of
    Left err  -> pure (Left (show err))     -- on error
    Right txt -> pure (Right txt)           -- on success

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

getGatewayWithoutAuth :: GatewayId -> Maybe Bool -> Waziup Gateway
getGatewayWithoutAuth gid mfull = do
  info "Get gateway"
  mg <- runMongo $ getGatewayMongo gid
  g <- case mg of
    Just g  -> return g
    Nothing -> throwError err404 { errBody = "Cannot get gateway: id not found" }

  -- Skip permission check since no AuthUser
  case mfull of
    Just True -> getFullGateway g
    _         -> return g

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

getGatewayVpnFile ::  GatewayId -> Handler (Headers '[Header "Content-Disposition" T.Text] BS.ByteString)
getGatewayVpnFile gwId = do
  info "Registers gateway to vpn network"
  g <- getGatewayWithoutAuth gid Nothing
  let name = gwName g
  liftIO $ putStrLn ("Gateway name is: " ++ T.unpack name)
  result <- liftIO (newWazigate name)
  case result of
    Left errMsg -> throwError $ err400 { errBody = TE.encodeUtf8 (T.pack errMsg) }
    Right _ -> do
      let safeName  = takeBaseName gwId
          filePath  = "/app/data" </> safeName <.> "ovpn"
          dispValue = T.pack ("attachment; filename=" ++ safeName ++ ".ovpn")
      fileData <- liftIO (BS.readFile filePath)
      pure (addHeader dispValue fileData)

removeWazigate :: T.Text -> IO (Either String ())
removeWazigate host = do
  info "Removes gateway to vpn network"
  -- 1️⃣ Run the shell script
  (exitCode, out, errOut) <- readProcessWithExitCode "bash" ["./services/remove-client.sh", T.unpack host] ""

  case exitCode of
    ExitSuccess -> putStrLn out
    ExitFailure _ ->
      return $ Left $ "Failed to revoke client " ++ T.unpack host ++ 
                      ": " ++ errOut ++ "\nOutput: " ++ out

  -- 2️⃣ Open and read the file
  let filename = "/app/openvpn_clients_dynamic.hosts"
  fileResult <- try (TIO.readFile filename) :: IO (Either IOException T.Text)
  case fileResult of
    Left e -> return $ Left $ "Failed to open file: " ++ show e
    Right content -> do
      -- 3️⃣ Process lines
      let lines' = T.lines content
          filtered = filter (not . matchesHost host) lines'

      -- 4️⃣ Write to a temporary file
      let tmpFile = filename ++ ".tmp"
      writeResult <- try (TIO.writeFile tmpFile (T.unlines filtered)) :: IO (Either IOException ())
      case writeResult of
        Left e -> return $ Left $ "Failed to write file: " ++ show e
        Right _ -> do
          -- 5️⃣ Replace old file atomically
          renameResult <- try (renameFile tmpFile filename) :: IO (Either IOException ())
          case renameResult of
            Left e -> return $ Left $ "Failed to replace file: " ++ show e
            Right _ -> return $ Right ()

  where
    matchesHost :: T.Text -> T.Text -> Bool
    matchesHost h line =
      case T.words line of
        [] -> False
        fields ->
          let client = last fields
          in client == h

newWazigate :: String -> IO (Either String ())
newWazigate name = do
  result <- try (callProcess "bash" ["./services/shell.sh", name]) :: IO (Either SomeException ())
  case result of
    Left err -> pure (Left (show err))
    Right _  -> pure (Right ())


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

