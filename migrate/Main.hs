{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO
import           Keycloak hiding (createResource)
import           System.Log.Logger
import           System.Log.Formatter
import           System.Log.Handler hiding (setLevel)
import           System.Log.Handler.Simple
import           Data.Aeson
import           Data.String.Conversions
import           Data.Either
import           Data.Maybe
import           Waziup.Devices
import           Waziup.Gateways
import           Waziup.Projects
import           Waziup.Types
import           Waziup.Config
import           Waziup.Auth
import           Waziup.Utils as U
import           Control.Monad.IO.Class
import           Control.Monad
import           Orion as O

main :: IO ()
main = return ()

recreateKCResources :: IO ()
recreateKCResources = do

  --delete all resources
  flip runKeycloak defaultKCConfig $ getClientAuthToken >>= deleteAllResources 
  
  wi <- configureWaziup 
  flip runWaziup wi $ do
    tok <- postAuth $ AuthBody "cdupont" "password" 
    devs <- getDevices (Just tok) Nothing (Just 1000) Nothing
    liftIO $ putStrLn $ "Recreating " ++ (show $ length devs) ++ " Devices"
    mapM_ (\d -> createResource (Just tok) (PermDeviceId $ devId d) (devVisibility d) (devOwner d)) devs

    gws <- getGateways (Just tok) (Just False) 
    liftIO $ putStrLn $ "Recreating " ++ (show $ length gws) ++ " Gateways"

    return ()
  return ()

fixVis :: IO ()
fixVis = do
  wi <- configureWaziup 
  void $ flip runWaziup wi $ do
    tok <- postAuth $ AuthBody "cdupont" "password" 
    devs <- getDevices (Just tok) Nothing (Just 1000) Nothing
    let noVisDevs = filter (\d -> isNothing $ devVisibility d) devs
    liftIO $ putStrLn $ "Fixing visibility on " ++ (show $ length noVisDevs) ++ " devices " ++ " out of " ++ (show $ length devs) ++ " devices"
    forM_ noVisDevs $ \d -> void $ U.try $ liftOrion $ O.postTextAttributeOrion (toEntityId $ devId d) devTyp (AttributeId "visibility") "public"

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Migrate" s
info  s = liftIO $ infoM    "Migrate" s
warn  s = liftIO $ warningM "Migrate" s
err   s = liftIO $ errorM   "Migrate" s
