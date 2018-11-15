{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.Wai.Handler.Warp
import Waziup.Server
import Waziup.Types 
import Data.Maybe
import Data.Text
import Data.String.Conversions
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler hiding (setLevel)
import System.Log.Handler.Simple
import System.Log.Handler.Log4jXML
import System.IO
import System.Environment
import Mongo
import Keycloak hiding (try)
import Orion hiding (try)
import Database.MongoDB as DB hiding (value)
import Options.Applicative as Opts
import Control.Exception
import Control.Monad.IO.Class

main :: IO ()
main = do
  startLog "Waziup-log.xml"
  --murl <- lookupEnv "KEYCLOAK_URL"
  --let url = case murl of
  --     Just bue -> bue
  --     Nothing -> convertString $ baseUrl defaultKCConfig
  conf <- execParser $ opts defaultServerConfig defaultMongoConfig defaultKCConfig defaultOrionConfig 
  (epipe :: Either SomeException Pipe) <- try $ DB.connect (host "127.0.0.1")
  let pipe = case epipe of
       Right pipe -> pipe
       Left e -> error "Cannot connect to MongoDB"
  --let mongoContext = MongoContext pipe DB.master "projects"
  infoM "Main" "Running on http://localhost:8081"
  run 8081 $ waziupServer $ WaziupInfo pipe conf

opts :: ServerConfig -> MongoConfig -> KCConfig -> OrionConfig -> ParserInfo WaziupConfig
opts serv m kc o = Opts.info ((waziupConfigParser serv m kc o) <**> helper) parserInfo

parserInfo :: InfoMod WaziupConfig
parserInfo = fullDesc
  <> progDesc "Create a server for Waziup API based on backend components Mongo, Orion and Keycloak"
  <> header "Waziup API server"

waziupConfigParser :: ServerConfig -> MongoConfig -> KCConfig -> OrionConfig -> Parser WaziupConfig
waziupConfigParser servDef mDef kcDef oDef = do
  serv <- serverConfigParser servDef
  m <- mongoConfigParser mDef
  kc <- kcConfigParser kcDef
  o <- orionConfigParser oDef
  return $ WaziupConfig serv m kc o

serverConfigParser :: ServerConfig -> Parser ServerConfig
serverConfigParser (ServerConfig defUrl defPort) = do
  url  <- strOption (long "url"  <> metavar "<url>"  <> help "url of this server"  <> value defUrl)
  port <- option auto (long "port" <> metavar "<port>" <> help "port of this server" <> value defPort) 
  return $ ServerConfig url port

kcConfigParser :: KCConfig -> Parser KCConfig
kcConfigParser (KCConfig defUrl defRealm defCID defCSec defAdmLog defAdmPass defGueLog defGuePass) = do
  baseUrl       <- strOption (long "kcUrl"       <> metavar "<url>"      <> help "url of Keycloak"            <> value defUrl)
  realm         <- strOption (long "kcRealm"     <> metavar "<realm>"    <> help "realm of Keycloak"          <> value defRealm) 
  clientId      <- strOption (long "kcClientId"  <> metavar "<id>"       <> help "Client ID of Keycloak"      <> value defCID)
  clientSecret  <- strOption (long "kcClientSec" <> metavar "<secret>"   <> help "Client Secret of Keycloak"  <> value defCSec)
  adminLogin    <- strOption (long "kcAdminLog"  <> metavar "<login>"    <> help "Admin login of Keycloak"    <> value defAdmLog)
  adminPassword <- strOption (long "kcAdminPass" <> metavar "<password>" <> help "Admin password of Keycloak" <> value defAdmPass)
  guestLogin    <- strOption (long "kcGuestLog"  <> metavar "<login>"    <> help "Guest login of Keycloak"    <> value defGueLog)
  guestPassword <- strOption (long "kcGuestPass" <> metavar "<password>" <> help "Guest password of Keycloak" <> value defGuePass)
  return $ KCConfig baseUrl realm clientId clientSecret adminLogin adminPassword guestLogin guestPassword

orionConfigParser :: OrionConfig -> Parser OrionConfig
orionConfigParser (OrionConfig defUrl defServ) = do
  url     <- strOption (long "orionUrl"      <> metavar "<url>"     <> help "url of Orion"                  <> value defUrl)
  service <- strOption (long "orionService" <> metavar "<service>" <> help "Fiware Service used for Orion" <> value defServ) 
  return $ OrionConfig url service

mongoConfigParser :: MongoConfig -> Parser MongoConfig
mongoConfigParser def = do
  url     <- strOption (long "mongoUrl" <> metavar "<url>" <> help "url of Mongo DB" <> (value $ mongoUrl def))
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

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "API" s
info s = liftIO $ infoM "API" s
warn s = liftIO $ warningM "API" s
err s = liftIO $ errorM "API" s
