{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO
import           Keycloak hiding (createResource)
import           System.Log.Logger
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
main = return () --recreateKCResources


fixVis :: IO ()
fixVis = do
  wi <- configureWaziup 
  void $ flip runWaziup wi $ do
    devs <- getAllDevices Nothing
    let noVisDevs = filter (\d -> isNothing $ devVisibility d) devs
    liftIO $ putStrLn $ "Fixing visibility on " ++ (show $ length noVisDevs) ++ " devices " ++ " out of " ++ (show $ length devs) ++ " devices"
    forM_ noVisDevs $ \d -> void $ U.try $ liftOrion $ O.postTextAttributeOrion (toEntityId $ devId d) devTyp (AttributeId "visibility") "public"

fixOwner :: IO ()
fixOwner = do
  wi <- configureWaziup 
  void $ flip runWaziup wi $ do
    devs <- getAllDevices Nothing
    let noOwnerDevs = filter (\d -> if (isNothing $ devOwner d) then True else ((fromJust $ devOwner d) == "")) devs
    liftIO $ putStrLn $ "Fixing owner on " ++ (show $ length noOwnerDevs) ++ " devices out of " ++ (show $ length devs) ++ " devices"
    forM_ noOwnerDevs $ \d -> void $ U.try $ liftOrion $ O.postTextAttributeOrion (toEntityId $ devId d) devTyp (AttributeId "owner") "guest"

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Migrate" s
info  s = liftIO $ infoM    "Migrate" s
warn  s = liftIO $ warningM "Migrate" s
err   s = liftIO $ errorM   "Migrate" s
