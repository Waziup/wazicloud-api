{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Main where

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Waziup.Server
import           Waziup.Types 
import           Waziup.Utils 
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
import           Keycloak hiding (try)
import           Orion hiding (try)
import           Database.MongoDB as DB hiding (value)
import           Options.Applicative as Opts hiding (Success, Failure)
import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Concurrent
import           System.FilePath ((</>))
import           System.Environment
import           Paths_Waziup_Servant
import           MQTT
import           Web.Twitter.Conduit hiding (map)

main :: IO ()
main = do
  (try $ startLog "Waziup-log.xml") :: IO (Either SomeException ())
  Main.info $ "API server starting..."
  envUrl      <- lookupEnv "HTTP_URL"
  envPort     <- lookupEnv "HTTP_PORT" 
  envPortMQTT <- lookupEnv "MQTT_PORT"
  envKCUrl    <- lookupEnv "KEYCLOAK_URL"
  envOrUrl    <- lookupEnv "ORION_URL"
  envMongUrl  <- lookupEnv "MONGODB_URL"
  envMosqHost <- lookupEnv "MOSQ_HOST"
  envMosqPort <- lookupEnv "MOSQ_PORT"
  envTwitKey    <- lookupEnv "TWITTER_CONSUMER_KEY"
  envTwitSec    <- lookupEnv "TWITTER_CONSUMER_SECRET"
  envTwitTok    <- lookupEnv "TWITTER_ACCESS_TOKEN"
  envTwitTokSec <- lookupEnv "TWITTER_ACCESS_TOKEN_SECRET"
  let kcConfig     = defaultKCConfig     & baseUrl        .~? (convertString <$> envKCUrl)
  let orionConfig  = defaultOrionConfig  & orionUrl       .~? (convertString <$> envOrUrl)
  let mongoConfig  = defaultMongoConfig  & mongoUrl       .~? (convertString <$> envMongUrl)
  let serverConfig = defaultServerConfig & serverHost     .~? (convertString <$> envUrl)
                                         & serverPort     .~? (read          <$> envPort)
                                         & serverPortMQTT .~? (read          <$> envPortMQTT)
  let mqttConfig   = defaultMQTTConfig   & mqttHost       .~? (convertString <$> envMosqHost)
                                         & mqttPort       .~? (read          <$> envMosqPort)
  let twitterConf = 
        if isJust envTwitKey && isJust envTwitSec && isJust envTwitTok && isJust envTwitTokSec 
          then def { twToken = def { 
                       twOAuth = twitterOAuth { oauthConsumerKey    = convertString $ fromJust envTwitKey, 
                                                oauthConsumerSecret = convertString $ fromJust envTwitSec}, 
                       twCredential = Credential [ ("oauth_token",        convertString $ fromJust envTwitTok),
                                                   ("oauth_token_secret", convertString $ fromJust envTwitTokSec)]},
                      twProxy = Nothing}
          else defaultTwitterConf

  conf <- execParser $ opts serverConfig mongoConfig kcConfig orionConfig mqttConfig twitterConf 
  let mongUrl = conf ^. mongoConf.mongoUrl
  pool <- createPool (DB.connect $ readHostPort (convertString mongUrl)) DB.close 1 300 5
  ontologies <- loadOntologies
  let host = conf ^. serverConf.serverHost
  let port = conf ^. serverConf.serverPort
  let mqttPort = conf ^. serverConf.serverPortMQTT
  let waziupInfo = WaziupInfo pool conf ontologies
  Main.info $ convertString $ "HTTP API is running on " <> host <> "/api/v1"
  Main.info $ convertString $ "MQTT is running on port " <> (show mqttPort)
  Main.info $ convertString $ "Documentation is on " <> host <> "/docs"
  forkIO $ mqttProxy waziupInfo
  run port $ logStdoutDev $ waziupServer waziupInfo

opts :: ServerConfig -> MongoConfig -> KCConfig -> OrionConfig -> MQTTConfig -> TWInfo -> ParserInfo WaziupConfig
opts serv m kc o mqtt tw = Opts.info ((waziupConfigParser serv m kc o mqtt tw) <**> helper) parserInfo

parserInfo :: InfoMod WaziupConfig
parserInfo = fullDesc
  <> progDesc "Create a server for Waziup API based on backend components Mongo, Orion and Keycloak"
  <> header "Waziup API server"

waziupConfigParser :: ServerConfig -> MongoConfig -> KCConfig -> OrionConfig -> MQTTConfig -> TWInfo -> Parser WaziupConfig
waziupConfigParser servDef mDef kcDef oDef mqttDef twittDef = do
  serv <- serverConfigParser servDef
  m    <- mongoConfigParser mDef
  kc   <- kcConfigParser kcDef
  o    <- orionConfigParser oDef
  return $ WaziupConfig serv m kc o mqttDef twittDef

serverConfigParser :: ServerConfig -> Parser ServerConfig
serverConfigParser (ServerConfig defUrl defPort defPortMQTT defGueLog defGuePass) = do
  url           <- strOption   (long "url"         <> metavar "<url>"      <> help "url of this server"  <> value defUrl)
  port          <- option auto (long "port"        <> metavar "<port>"     <> help "HTTP port of this server" <> value defPort) 
  portMQTT      <- option auto (long "portMQTT"    <> metavar "<portMQTT>" <> help "MQTT port of this server" <> value defPortMQTT) 
  guestLogin    <- strOption   (long "kcGuestLog"  <> metavar "<login>"    <> help "Guest login of Keycloak"    <> value defGueLog)
  guestPassword <- strOption   (long "kcGuestPass" <> metavar "<password>" <> help "Guest password of Keycloak" <> value defGuePass)
  return $ ServerConfig url port portMQTT guestLogin guestPassword

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
  url     <- strOption (long "mongoUrl" <> metavar "<url>" <> help "url of Mongo DB" <> (value $ _mongoUrl def))
  return $ MongoConfig url

startLog :: FilePath -> IO ()
startLog fp = do
   stdoutHandler <- do
        lh <- streamHandler stdout DEBUG
        return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
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
         Main.debug "Sensing devices ontology is correct"
         return True
       Failure errs -> do
         mapM_ Main.err errs
         return False
  qkOK <- case checkQuantityKinds qks us of
       Success _ -> do
          Main.debug "Quantity kinds ontology is correct"
          return True
       Failure errs -> do
         mapM_ Main.err errs
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
debug s = liftIO $ debugM "Main" s
info s  = liftIO $ infoM "Main" s
warn s  = liftIO $ warningM "Main" s
err s   = liftIO $ errorM "Main" s
