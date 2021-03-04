{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Utils where

import           Waziup.Types
import qualified Orion as O
import           Keycloak as KC hiding (User)
import           Control.Monad.Except (throwError, catchError, MonadError)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Lens
import           Control.Exception as C 
import           Data.Aeson
import           Data.Maybe
import           Data.Hashable
import           Data.Pool as P
import           Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Types.Status as HTS
import           Network.HTTP.Client as HC
import           Servant
import           Servant.Auth.Server hiding (JWT)
import           Database.MongoDB as DB
import           Debug.Trace

getAuthUser :: AuthUser -> User
getAuthUser (Authenticated user) = user 
getAuthUser _ = guestUser

-- * Lifting

-- Run Orion functions
liftOrion :: O.Orion a -> Waziup a
liftOrion orion = do
 conf <- view (waziupConfig.orionConf)
 e <- liftIO $ O.runOrion orion conf
 case e of
   Right res -> return res
   Left er -> throwError $ fromOrionError er

-- * Run Keycloak function
liftKeycloak :: KC.Keycloak a -> Waziup a
liftKeycloak kc = do
 conf <- view $ waziupConfig.keycloakConf
 e <- liftIO $ runKeycloak kc conf
 case e of
   Right res -> return res
   Left er -> throwError $ fromKCError er

getAdminToken :: Waziup JWT
getAdminToken = do 
  adminId   <- view (waziupConfig.serverConf.adminLogin)
  adminPass <- view (waziupConfig.serverConf.adminPassword)
  liftKeycloak (getJWT adminId adminPass)

-- * run Mongo function
runMongo :: Action IO a -> Waziup a
runMongo dbAction = do
  pool <- view dbPool
  muser  <- view $ waziupConfig.mongoConf.mongoUser
  mpass  <- view $ waziupConfig.mongoConf.mongoPass
  liftIO $ P.withResource pool $ \p -> DB.access p DB.master "waziup" $ do
     case (muser, mpass) of
       (Just user, Just pass) -> do 
         res <- DB.auth user pass
         if res 
           then dbAction
           else liftIO $ C.throwIO $ DB.ConnectionFailure $ userError "auth failed."
       _ -> dbAction

-- * error convertions
fromOrionError :: O.OrionError -> ServerError
fromOrionError (O.HTTPError (HttpExceptionRequest _ (StatusCodeException r m))) 
  = ServerError { errHTTPCode     = HTS.statusCode $ HC.responseStatus r, 
                  errReasonPhrase = show $ HTS.statusMessage $ HC.responseStatus r, 
                  errBody         = BL.fromStrict ("Error from Orion: " <> m),
                  errHeaders      = []}
fromOrionError (O.HTTPError (HttpExceptionRequest _ (HC.ConnectionFailure a))) = err500 {errBody = encode $ "Failed to connect to Orion: " ++ show a} 
fromOrionError (O.HTTPError s) = err500 {errBody = encode $ show s} 
fromOrionError (O.ParseError s) = err500 {errBody = encode s} 
fromOrionError O.EmptyError = err500 {errBody = "EmptyError"}

fromKCError :: KC.KCError -> ServerError
fromKCError (KC.HTTPError (HttpExceptionRequest _ (StatusCodeException r m)))
  = ServerError { errHTTPCode     = HTS.statusCode $ HC.responseStatus r, 
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

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

(!?) :: (Eq k, Hashable k) => HashMap k v -> k -> Maybe v
(!?) m k = H.lookup k m
{-# INLINE (!?) #-}

trace' :: Show a => String -> a -> a 
trace' s a = trace (s ++ (show a)) a

-- URI encode the forbidden characters of Orion
urlEncodeForbiddens :: String -> String
urlEncodeForbiddens bs = concatMap enc bs where
  enc :: Char -> String
  enc '<'  = "%3C"
  enc '>'  = "%3E"
  enc '\"' = "%22"
  enc '\'' = "%27"
  enc '='  = "%3D"
  enc ';'  = "%3B"
  enc '('  = "("
  enc ')'  = ")"
  enc a    = [a]
