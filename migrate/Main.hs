{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO
import           Keycloak
import           System.Log.Logger
import           System.Log.Formatter
import           System.Log.Handler hiding (setLevel)
import           System.Log.Handler.Simple
import           Data.Aeson
import           Data.String.Conversions
import           Data.Either
import           Data.Maybe
import           Waziup.Devices
import           Waziup.Types
import           Waziup.Config
import           Waziup.Auth
import           Waziup.Utils as U
import           Control.Monad.IO.Class
import           Control.Monad


main :: IO ()
main = do
  wi <- configureWaziup 
  flip runWaziup wi $ do
    tok <- postAuth $ AuthBody "cdupont" "password" 
    devs <- getDevices (Just tok) Nothing (Just 1000) Nothing

    forM devs $ \d -> do
      keyRes <- U.try $ createResourceDevice (Just tok) (devId d) (devVisibility d) -- (devOwner d)
      return ()
    return ()
  return ()
  --res <- flip runKeycloak defaultKCConfig $ do 
  --  ids <- getAllResourceIds
  --  res2 <- mapM (try . getResource) ids
  --  return $ rights res2
  --case res of
  --  Right (res' :: [Resource]) -> putStrLn $ convertString $ encode res'
  --  Left _ -> return ()

