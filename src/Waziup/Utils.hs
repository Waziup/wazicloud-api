{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Utils where

import           Waziup.Types
import qualified Orion as O
import           Keycloak as KC hiding (try)
import           Control.Monad.Except (throwError, catchError, runExceptT, MonadError)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Lens
import           Control.Exception.Lifted as L (throwIO, catch) 
import           Control.Exception as C 
import           Data.String.Conversions
import           Data.Aeson
import           Data.Maybe
import           Data.Hashable
import           Data.Pool as P
import           Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Types.Status as HTS
import           Network.HTTP.Client as HC
import           Servant
import           Database.MongoDB as DB

-- * Lifting

-- Run Orion functions
liftOrion :: O.Orion a -> Waziup a
liftOrion orion = do
 conf <- view (waziupConfig.orionConf)
 e <- liftIO $ O.runOrion orion conf
 case e of
   Right res -> return res
   Left err -> throwError $ fromOrionError err

-- * Run a Keycloak function with default guest token
liftKeycloak :: Maybe KC.Token -> (Token -> KC.Keycloak a) -> Waziup a
liftKeycloak mtok kc = do
 tok <- fromMaybeToken mtok 
 liftKeycloak' (kc tok)

fromMaybeToken :: Maybe Token -> Waziup Token
fromMaybeToken mtok = case mtok of
   Just tok2 -> return tok2
   Nothing -> do
     guestId   <- view (waziupConfig.serverConf.guestLogin)
     guestPass <- view (waziupConfig.serverConf.guestPassword)
     -- retrieve guest token
     liftKeycloak' (getUserAuthToken guestId guestPass)

-- * Run Keycloak function
liftKeycloak' :: KC.Keycloak a -> Waziup a
liftKeycloak' kc = do
 conf <- view $ waziupConfig.keycloakConf
 e <- liftIO $ runKeycloak kc conf
 case e of
   Right res -> return res
   Left err -> throwError $ fromKCError err

-- * run Mongo function
runMongo :: Action IO a -> Waziup a
runMongo dbAction = do
  (WaziupInfo dBPool conf _) <- ask
  dBPool <- view dbPool 
  muser  <- view $ waziupConfig.mongoConf.mongoUser
  mpass  <- view $ waziupConfig.mongoConf.mongoPass
  liftIO $ P.withResource dBPool $ \p -> DB.access p DB.master "waziup" $ do
     case (muser, mpass) of
       (Just user, Just pass) -> do 
         res <- DB.auth user pass
         if res 
           then dbAction
           else liftIO $ C.throwIO $ DB.ConnectionFailure $ userError "auth failed."
       _ -> dbAction

-- * error convertions
fromOrionError :: O.OrionError -> ServantErr
fromOrionError (O.HTTPError (HttpExceptionRequest _ (StatusCodeException r m))) 
  = ServantErr { errHTTPCode     = HTS.statusCode $ HC.responseStatus r, 
                 errReasonPhrase = show $ HTS.statusMessage $ HC.responseStatus r, 
                 errBody         = BL.fromStrict ("Error from Orion: " <> m),
                 errHeaders      = []}
fromOrionError (O.HTTPError (HttpExceptionRequest _ (HC.ConnectionFailure a))) = err500 {errBody = encode $ "Failed to connect to Orion: " ++ show a} 
fromOrionError (O.HTTPError s) = err500 {errBody = encode $ show s} 
fromOrionError (O.ParseError s) = err500 {errBody = encode s} 
fromOrionError O.EmptyError = err500 {errBody = "EmptyError"}

fromKCError :: KC.KCError -> ServantErr
fromKCError (KC.HTTPError (HttpExceptionRequest _ (StatusCodeException r m)))
  = ServantErr { errHTTPCode     = HTS.statusCode $ HC.responseStatus r, 
                 errReasonPhrase = show $ HTS.statusMessage $ HC.responseStatus r, 
                 errBody         = BL.fromStrict ("Error from Keycloak: " <> m),
                 errHeaders      = []}
fromKCError (KC.HTTPError (HttpExceptionRequest _ (HC.ConnectionFailure a))) = err500 {errBody = encode $ "Failed to connect to Keycloak: " ++ show a} 
fromKCError (KC.HTTPError s) = err500 {errBody = encode $ show s} 
fromKCError (KC.ParseError s) = err500 {errBody = encode s} 
fromKCError KC.EmptyError = err500 {errBody = "EmptyError"}


-- Lens setter for Maybe values
(.~?) :: Setter s t b b -> Maybe b -> s -> t
(.~?) x y =  x %~ (\v -> fromMaybe v y)

maybeToList' :: Maybe [a] -> [a]
maybeToList' (Just a) = a
maybeToList' Nothing = []

try :: (MonadError a m) => m b -> m (Either a b) 
try a = catchError (Right `liftM` a) (return . Left) 

replaceKey :: (Ord k, Hashable k) => k -> k -> H.HashMap k v -> H.HashMap k v
replaceKey k0 k1 m = case H.lookup k0 m of
   Nothing -> m
   Just e  -> H.insert k1 e (H.delete k0 m)

maybe' :: b -> (a -> b -> b) -> Maybe a -> b
maybe' b f ma = maybe b (flip f b) ma

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe (Data.Aeson.Error _) = Nothing

