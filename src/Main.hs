{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp
import Waziup.API

main :: IO ()
main = do
  putStrLn "Running on http://localhost:8081"
  run 8081 waziupServer


