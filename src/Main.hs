
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Network.Wai
import Network.Wai.Handler.Warp
import Waziup.API
import Keycloak.Client

main :: IO ()
main = do
  Just p <- getPermissions "cdupont" "password"
  putStrLn $ show p
  run 8081 waziupServer


