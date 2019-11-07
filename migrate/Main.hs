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
main = recreateKCResources

recreateKCResources :: IO ()
recreateKCResources = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  liftIO $ putStrLn $ "fixing missing visibility and owners (press any key or CTRL-C)?"
  void $ getChar
  fixVis
  fixOwner
  liftIO $ putStrLn $ "Delete all resources?"
  void $ getChar
  void $ flip runKeycloak defaultKCConfig $ getClientAuthToken >>= deleteAllResources 
  
  wi <- configureWaziup 
  void $ flip runWaziup wi $ do
    tok <- postAuth $ AuthBody "cdupont" "password" 

    devs <- getAllDevices Nothing
    liftIO $ putStrLn $ "Recreating " ++ (show $ length devs) ++ " Devices?"
    void $ liftIO getChar
    res1 <- mapM (createDev tok) devs
    liftIO $ putStrLn $ "\ncreated " ++ (show $ length $ rights res1) ++ " devices: " ++ (show $ map unDeviceId $ rights res1) ++ 
                            "\n\n***************failed " ++ (show $ length $ lefts res1) ++ " devices: " ++ (show $ map unDeviceId $ lefts res1)
    void $ liftIO getChar

    gws <- getAllGateways 
    liftIO $ putStrLn $ "Recreating " ++ (show $ length gws) ++ " Gateways?"
    void $ liftIO getChar
    res2 <- mapM (createGateway tok) gws
    liftIO $ putStrLn $ "\ncreated " ++ (show $ length $ rights res2) ++ " gateways: " ++ (show $ map unGatewayId $ rights res2) ++ 
                            "\n\n***************failed " ++ (show $ length $ lefts res2) ++ " gateways: " ++ (show $ map unGatewayId $ lefts res2)
    void $ liftIO getChar

    ps <- getAllProjects 
    liftIO $ putStrLn $ "Recreating " ++ (show $ length ps) ++ " Projects?"
    void $ liftIO getChar
    res3 <- mapM (createProject tok) ps
    liftIO $ putStrLn $ "\ncreated " ++ (show $ length $ rights res3) ++ " projects: " ++ (show $ map unProjectId $ rights res3) ++ 
                            "\n\n***************failed " ++ (show $ length $ lefts res3) ++ " projects: " ++ (show $ map unProjectId $ lefts res3)

    return ()
  return ()

createDev :: Token -> Device -> Waziup (Either DeviceId DeviceId)
createDev tok d = do
  liftIO $ putStrLn $ "creating " ++ (show $ unDeviceId $ devId d)
  --liftIO getChar
  res <- U.try $ createResource (Just tok) (PermDeviceId $ devId d) (devVisibility d) (devOwner d)
  case res of
    Right _ -> liftIO $ putStrLn "Success" >> (return $ Right $ devId d) 
    Left _ -> liftIO $ putStrLn "Failure"  >> (return $ Left $ devId d)

createGateway :: Token -> Gateway -> Waziup (Either GatewayId GatewayId)
createGateway tok d = do
  liftIO $ putStrLn $ "creating " ++ (show $ unGatewayId $ gwId d)
  --liftIO getChar
  res <- U.try $ createResource (Just tok) (PermGatewayId $ gwId d) (gwVisibility d) (gwOwner d)
  case res of
    Right _ -> liftIO $ putStrLn "Success" >> (return $ Right $ gwId d) 
    Left _ -> liftIO $ putStrLn "Failure"  >> (return $ Left $ gwId d)

createProject :: Token -> Project -> Waziup (Either ProjectId ProjectId)
createProject tok d = do
  liftIO $ putStrLn $ "creating " ++ (show $ unProjectId $ fromJust $ pId d)
  --liftIO getChar
  res <- U.try $ createResource (Just tok) (PermProjectId $ fromJust $ pId d) (Just Public) (pOwner d)
  case res of
    Right _ -> liftIO $ putStrLn "Success" >> (return $ Right $ fromJust $ pId d) 
    Left _ -> liftIO $ putStrLn "Failure"  >> (return $ Left $ fromJust $ pId d)

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
