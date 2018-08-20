{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp
import Waziup.API

main :: IO ()
main = do
  run 8081 waziupServer


