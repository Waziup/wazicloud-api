{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Waziup.Config where

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Network.Wai.Middleware.Cors
import           Waziup.Server hiding (info, warn, debug, err)
import           Waziup.Types hiding (info, warn, debug, err)
import           Waziup.Utils hiding (info, warn, debug, err)
import           Data.String.Conversions
import           Data.Aeson hiding (Success)
import qualified Data.ByteString as BS
import           Data.Validation
import           Data.Foldable
import           Data.Maybe
import           Data.Pool
import           System.Log.Logger
import           System.Log.Formatter
import           System.Log.Handler hiding (setLevel)
import           System.Log.Handler.Simple
import           System.Log.Handler.Log4jXML
import           System.IO
import           Keycloak hiding (try, info, warn, debug, err)
import           Orion hiding (try, info, warn, debug, err)
import           Database.MongoDB as DB hiding (value)
import           Options.Applicative as Opts hiding (Success, Failure)
import           Control.Exception as C
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Concurrent
import           System.FilePath ((</>))
import           System.Environment
import           Paths_Waziup_Servant
import           MQTT hiding (info, warn, debug, err)
import           Web.Twitter.Conduit hiding (map)
import           Data.Map as M hiding (map)
import           Control.Concurrent.STM
import           Data.Time

configureWaziup :: IO WaziupInfo
configureWaziup = do
  (C.try $ startLog "Waziup-log.xml") :: IO (Either SomeException ())
  envUrl              <- lookupEnv "HTTP_URL"
  envPort             <- lookupEnv "HTTP_PORT" 
  envPortMQTT         <- lookupEnv "MQTT_PORT"
  envKCUrl            <- lookupEnv "KEYCLOAK_URL"
  envOrUrl            <- lookupEnv "ORION_URL"
  envMongUrl          <- lookupEnv "MONGODB_URL"
  envMongUser         <- lookupEnv "MONGODB_USER"
  envMongPass         <- lookupEnv "MONGODB_PASS"
  envMosqHost         <- lookupEnv "MOSQ_HOST"
  envMosqPort         <- lookupEnv "MOSQ_PORT"
  envTwitKey          <- lookupEnv "TWITTER_CONSUMER_KEY"
  envTwitSec          <- lookupEnv "TWITTER_CONSUMER_SECRET"
  envTwitTok          <- lookupEnv "TWITTER_ACCESS_TOKEN"
  envTwitTokSec       <- lookupEnv "TWITTER_ACCESS_TOKEN_SECRET"
  envPlivoID          <- lookupEnv "PLIVO_ID"
  envPlivoToken       <- lookupEnv "PLIVO_TOKEN"
  envNotifMinInterval <- lookupEnv "NOTIF_MIN_INTERVAL"
  envCacheDuration    <- lookupEnv "NOTIF_CACHE_DURATION"
  let kcConfig     = defaultKCConfig     & baseUrl            .~? (convertString    <$> envKCUrl)
  let orionConfig  = defaultOrionConfig  & orionUrl           .~? (convertString    <$> envOrUrl)
  let mongoConfig  = defaultMongoConfig  & mongoUrl           .~? (convertString    <$> envMongUrl)
                                         & mongoUser          .~  (convertString    <$> envMongUser)
                                         & mongoPass          .~  (convertString    <$> envMongPass)
  let serverConfig = defaultServerConfig & serverHost         .~? (convertString    <$> envUrl)
                                         & serverPort         .~? (read             <$> envPort)
                                         & serverPortMQTT     .~? (read             <$> envPortMQTT)
                                         & notifMinInterval   .~? (fromInteger.read <$> envNotifMinInterval)
                                         & cacheValidDuration .~? (fromInteger.read <$> envCacheDuration)
  let mqttConfig   = defaultMQTTConfig   & mqttHost           .~? (convertString    <$> envMosqHost)
                                         & mqttPort           .~? (read             <$> envMosqPort)
  let twitterConf = 
        if isJust envTwitKey && isJust envTwitSec && isJust envTwitTok && isJust envTwitTokSec 
          then def { twProxy = Nothing,
                     twToken = def { twOAuth = twitterOAuth { oauthConsumerKey    = convertString $ fromJust envTwitKey, 
                                                              oauthConsumerSecret = convertString $ fromJust envTwitSec}, 
                                     twCredential = Credential [ ("oauth_token",        convertString $ fromJust envTwitTok),
                                                                 ("oauth_token_secret", convertString $ fromJust envTwitTokSec)]}}
          else defaultTwitterConf
  let plivoConf = defaultPlivoConf & plivoID    .~? (convertString <$> envPlivoID)
                                   & plivoToken .~? (convertString <$> envPlivoToken)
  let confParser = waziupConfigParser serverConfig mongoConfig kcConfig orionConfig mqttConfig twitterConf plivoConf
  let confParser' = Opts.info (confParser <**> helper) (fullDesc <> progDesc "Create a server for Waziup API" <> header "Waziup API server")
  conf <- execParser confParser'
  let mongUrl = conf ^. mongoConf.mongoUrl
  pool <- createPool (DB.connect $ readHostPort (convertString mongUrl)) DB.close 1 300 5
  ontologies <- loadOntologies
  permsCache <- atomically $ newTVar M.empty 
  return $ WaziupInfo pool conf ontologies permsCache

waziupConfigParser :: ServerConfig -> MongoConfig -> KCConfig -> OrionConfig -> MQTTConfig -> TWInfo -> PlivoConfig -> Parser WaziupConfig
waziupConfigParser servDef mDef kcDef oDef mqttDef twittDef plivoDef = do
  serv <- serverConfigParser servDef
  m    <- mongoConfigParser mDef
  kc   <- kcConfigParser kcDef
  o    <- orionConfigParser oDef
  return $ WaziupConfig serv m kc o mqttDef twittDef plivoDef

serverConfigParser :: ServerConfig -> Parser ServerConfig
serverConfigParser (ServerConfig defUrl defPort defPortMQTT defGueLog defGuePass defNotif defCache) = do
  url           <- strOption   (long "url"         <> metavar "<url>"      <> help "url of this server"  <> value defUrl)
  port          <- option auto (long "port"        <> metavar "<port>"     <> help "HTTP port of this server" <> value defPort) 
  portMQTT      <- option auto (long "portMQTT"    <> metavar "<portMQTT>" <> help "MQTT port of this server" <> value defPortMQTT) 
  guestLogin    <- strOption   (long "kcGuestLog"  <> metavar "<login>"    <> help "Guest login of Keycloak"    <> value defGueLog)
  guestPassword <- strOption   (long "kcGuestPass" <> metavar "<password>" <> help "Guest password of Keycloak" <> value defGuePass)
  notifInterval <- option auto (long "notif"       <> metavar "<notif>"    <> help "minimum interval for notifications (in seconds)" <> value (floor defNotif)) 
  cacheDuration <- option auto (long "cache"       <> metavar "<cache>"    <> help "duration of the cache valididy (in seconds)" <> value (floor defCache)) 
  return $ ServerConfig url port portMQTT guestLogin guestPassword (fromInteger notifInterval) (fromInteger cacheDuration)

kcConfigParser :: KCConfig -> Parser KCConfig
kcConfigParser (KCConfig defUrl defRealm defCID defCSec) = do
  baseUrl       <- strOption (long "kcUrl"       <> metavar "<url>"      <> help "url of Keycloak"            <> value defUrl)
  realm         <- strOption (long "kcRealm"     <> metavar "<realm>"    <> help "realm of Keycloak"          <> value defRealm) 
  clientId      <- strOption (long "kcClientId"  <> metavar "<id>"       <> help "Client ID of Keycloak"      <> value defCID)
  clientSecret  <- strOption (long "kcClientSec" <> metavar "<secret>"   <> help "Client Secret of Keycloak"  <> value defCSec)
  return $ KCConfig baseUrl realm clientId clientSecret

orionConfigParser :: OrionConfig -> Parser OrionConfig
orionConfigParser (OrionConfig defUrl defServ) = do
  url     <- strOption (long "orionUrl"     <> metavar "<url>"     <> help "url of Orion"                  <> value defUrl)
  service <- strOption (long "orionService" <> metavar "<service>" <> help "Fiware Service used for Orion" <> value defServ) 
  return $ OrionConfig url service

mongoConfigParser :: MongoConfig -> Parser MongoConfig
mongoConfigParser def = do
  url     <- strOption (long "mongoUrl"  <> metavar "<url>"  <> help "url of Mongo DB"            <> (value $ _mongoUrl def))
  user    <- optional $ strOption (long "mongoUser" <> metavar "<user>" <> help "admin user of Mongo DB")
  pass    <- optional $ strOption (long "mongoPass" <> metavar "<pass>" <> help "admin password of Mongo DB")
  return $ MongoConfig url (user <|> _mongoUser def) (user <|>  _mongoPass def) 

startLog :: FilePath -> IO ()
startLog fp = do
   stdoutHandler <- do
        lh <- streamHandler stdout DEBUG
        return $ setFormatter lh (tfLogFormatter "%Y-%m-%dT%H:%M:%S.%q" "[$time : $loggername : $prio] $msg")
   log4jHandler <- log4jFileHandler fp DEBUG
   updateGlobalLogger rootLoggerName removeHandler
   updateGlobalLogger rootLoggerName (addHandler stdoutHandler)
   updateGlobalLogger rootLoggerName (addHandler log4jHandler)
   updateGlobalLogger rootLoggerName (setLevel DEBUG)

loadOntologies :: IO Ontologies
loadOntologies = do
  sds <- loadSensingDevices
  qks <- loadQuantityKinds
  us <- loadUnits
  sdOK <- case checkSensingDevices sds qks of
       Success _ -> do
         debug "Sensing devices ontology is correct"
         return True
       Failure errs -> do
         mapM_ err errs
         return False
  qkOK <- case checkQuantityKinds qks us of
       Success _ -> do
          debug "Quantity kinds ontology is correct"
          return True
       Failure errs -> do
         mapM_ err errs
         return False
  if sdOK && qkOK 
    then return $ Ontologies sds [] qks us
    else error "Sensing devices ontology is not correct"
 
loadSensingDevices :: IO [SensorKind]
loadSensingDevices = do
  dir <- liftIO $ getDataDir
  msd <- liftIO $ BS.readFile $ dir </> "ontologies" </> "sensing_devices.json"
  case eitherDecode (convertString msd) of
    Right sd -> return sd
    Left (e :: String) -> error $ "Cannot decode data file: " ++ (show e)

loadQuantityKinds :: IO [QuantityKind]
loadQuantityKinds = do
  dir <- liftIO getDataDir
  mqk <- liftIO $ BS.readFile $ dir </> "ontologies" </> "quantity_kinds.json"
  case eitherDecode (convertString mqk) of
    Right qk -> return qk
    Left (e :: String) -> error $ "Cannot decode data file: " ++ (show e)

loadUnits :: IO [Unit]
loadUnits = do
  dir <- liftIO getDataDir
  mus <- liftIO $ BS.readFile $ dir </> "ontologies" </> "units.json"
  case eitherDecode (convertString mus) of
    Right us -> return us
    Left (e :: String) -> error $ "Cannot decode data file: " ++ (show e)

checkSensingDevices :: [SensorKind] -> [QuantityKind] -> Validation [String] ()
checkSensingDevices sds qks = sequenceA_ $ map (isSDValid qks) sds

isSDValid :: [QuantityKind] -> SensorKind -> Validation [String] ()
isSDValid qks (SensorKind id _ qks') = sequenceA_ $ map (\qk -> if isQKValid qks qk then Success () else Failure [("quantity kind " ++ (show $ qk) ++ " referenced by sensing device " ++ (show id) ++ " is not defined")]) qks' 

isQKValid :: [QuantityKind] -> QuantityKindId -> Bool
isQKValid qks id = any (\(QuantityKind id' _ _) -> id == id') qks

checkQuantityKinds :: [QuantityKind] -> [Unit] -> Validation [String] ()
checkQuantityKinds qks us = sequenceA_ $ map (isQKInfoValid us) qks

isQKInfoValid :: [Unit] -> QuantityKind -> Validation [String] ()
isQKInfoValid us (QuantityKind id _ us') = sequenceA_ $ map (\u -> if isUnitValid us u then Success () else Failure [("unit " ++ (show $ u) ++ " referenced by quantity kind " ++ (show id) ++ " is not defined")]) us' 

isUnitValid :: [Unit] -> UnitId -> Bool
isUnitValid us id = any (\(Unit id' _) -> id == id') us

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "Config" s
info s  = liftIO $ infoM "Config" s
warn s  = liftIO $ warningM "Config" s
err s   = liftIO $ errorM "Config" s
