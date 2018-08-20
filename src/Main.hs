
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Network.Wai
import Network.Wai.Handler.Warp
import Waziup.API
import Keycloak.Client

main :: IO ()
main = do
  run 8081 waziupServer


