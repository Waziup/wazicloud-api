{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO
import           System.Log.Logger
import           Data.Maybe
import           Waziup.Utils as U
import           Data.Time
import           Data.Map
import           Waziup.Types
import           Waziup.Auth
import           Waziup.Config
import qualified Keycloak as KC
import           Keycloak.Types
import           Control.Monad
import           Control.Monad.IO.Class

parse822Time = zonedTimeToUTC . fromJust . parseTimeM True defaultTimeLocale rfc822DateFormat
date1 = parse822Time "Tue, 02 Sep 1997 09:00:00 -0400"

main :: IO ()
main = do
  wi <- configureWaziup 
  void $ flip runWaziup wi $ do
    let resId = PermGatewayId $ GatewayId "GW1"
    void $ createResource Nothing resId (Just Public) (Just "cdupont") 
    let permReq1 = PermReq (Just $ getKCResourceId resId) [fromScope GatewaysView]
    let permReq2 = PermReq (Just $ getKCResourceId resId) [fromScope GatewaysView, fromScope GatewaysUpdate]
    let permReq3 = PermReq Nothing [fromScope GatewaysView]
    let permReq4 = PermReq (Just $ getKCResourceId resId) []
    let permReq5 = PermReq Nothing []
    let wrong1 = PermReq (Just $ getKCResourceId resId) [fromScope DevicesView]
    let wrong2 = PermReq (Just $ ResourceId "gateways-wrong") [fromScope GatewaysView]
    let wrong3 = PermReq (Just $ ResourceId "wrong") [fromScope GatewaysView]
    let wrong4 = PermReq (Just $ getKCResourceId resId) [ScopeName "wrong"]
    let wrong5 = PermReq Nothing [ScopeName "wrong"]
    tok <- postAuth $ AuthBody "cdupont" "password" 
    perms <- getPerms (Just tok) permReq1
    let res1 = perms == [Perm (Just resId) [GatewaysView]]
    let wrong4 = PermReq (Just $ getKCResourceId resId) [ScopeName "wrong"]
    --liftIO $ putStrLn $ "Perm results: " ++ (show perms) ++ " -> "++ (show res1)
    perms <- getPerms (Just tok) permReq2
    let res2 = perms == [Perm (Just resId) [GatewaysView, GatewaysUpdate]]
    liftIO $ putStrLn $ "Perm results: " ++ (show perms) ++ " -> "++ (show res2)
    --perms <- getPerms (Just (Token "")) permReq5
    --liftIO $ putStrLn $ "\n\nPerm results: " ++ (show perms)
