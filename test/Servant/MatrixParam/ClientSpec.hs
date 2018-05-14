{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.MatrixParam.ClientSpec (spec) where

import Control.Exception
import Control.Monad
import Data.ByteString     (ByteString)
import Data.IORef
import Data.Proxy
import Network.HTTP.Client (Manager, Request, defaultManagerSettings,
                            managerModifyRequest, managerResponseTimeout,
                            newManager, path)
import Servant
import Servant.Client
import System.IO.Unsafe
import Test.Hspec

#if MIN_VERSION_http_client(0,5,0)
import Network.HTTP.Client (responseTimeoutMicro)
#endif
#if MIN_VERSION_servant_client(0,9,0)
import Network.Wai.Handler.Warp (testWithApplication)
import Data.Maybe          (fromMaybe)
#else
import Control.Monad.Trans.Except (runExceptT)
#endif


import Servant.MatrixParam
import Servant.MatrixParam.Client ()
import Servant.MatrixParam.Server ()

spec :: Spec
spec = do

  describe "Servant.API.MatrixParam" $ do

    it "generates correct paths when arguments are not Nothing" $ do
      cliA (Just "alice") `hasRequestPath` "/a;name=alice"
      cliB (Just 3) (Just 2) `hasRequestPath` "/b;foo=3;bar=2"

    it "generates correct paths when arguments are Nothing" $ do
      cliA Nothing `hasRequestPath` "/a"
      cliB Nothing Nothing `hasRequestPath` "/b"

    it "can be combined with captures" $ do
      cliC 5 (Just 3) False `hasRequestPath` "/5;foo=3"

    it "supports matrix flags" $ do
      cliC 5 (Just 3) True `hasRequestPath` "/5;foo=3;bool"


#if MIN_VERSION_servant_client(0,9,0)
    it "generates paths that servant-server understands: cliA" $ do
      testWithApplication (return $ serve api server) $ \port -> do
        let res = cliA (Just "There is a there there")
            url = BaseUrl Http "localhost" port ""
        mgr' <- newManager defaultManagerSettings
        runClientM res (ClientEnv mgr' url Nothing)
          `shouldReturn` Right "There is a there there"
    it "generates paths that servant-server understands: cliB" $ do
      testWithApplication (return $ serve api server) $ \port -> do
        let res = cliB (Just 1) (Just 2)
            url = BaseUrl Http "localhost" port ""
        mgr' <- newManager defaultManagerSettings
        runClientM res (ClientEnv mgr' url Nothing)
          `shouldReturn` Right "3"
    it "generates paths that servant-server understands: cliC" $ do
      testWithApplication (return $ serve api server) $ \port -> do
        let res = cliC 5 (Just 1) True
            url = BaseUrl Http "localhost" port ""
        mgr' <- newManager defaultManagerSettings
        runClientM res (ClientEnv mgr' url Nothing)
          `shouldReturn` Right "6"
#endif
------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------

type Api =
       WithMatrixParams "a" '[MatrixParam "name" String] :> Get '[JSON] String
  :<|> WithMatrixParams "b" '[MatrixParam "foo" Int, MatrixParam "bar" Int]
         :> Get '[JSON] String
  :<|> CaptureWithMatrixParams "c" Int '[MatrixParam "foo" Int, MatrixFlag "bool"]
         :> Get '[JSON] String

api :: Proxy Api
api = Proxy


#if MIN_VERSION_servant_client(0,9,0)
server :: Server Api
server = e1 :<|> e2 :<|> e3
  where
    e1 name = return $ fromMaybe "" name
    e2 foo bar = return . show $ fromMaybe 0 foo + fromMaybe 0 bar
    e3 int foo bool
      | bool      = return . show $ int + fromMaybe 0 foo
      | otherwise = return ""
#endif

------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------

#if MIN_VERSION_servant_client(0,9,0)
hasRequestPath :: ClientM any -> ByteString -> IO ()
#else
hasRequestPath :: (Manager -> BaseUrl -> ClientM any) -> ByteString -> IO ()
#endif
hasRequestPath c expectedPath = do
  let anyBurl = BaseUrl Http "localhost" 6660 ""
#if MIN_VERSION_servant_client(0,9,0)
  _ <- void (runClientM c $ ClientEnv mgr anyBurl Nothing)
    `catch` \(_ :: SomeException) -> return ()
#else
  _ <- void (runExceptT $ c mgr anyBurl)
    `catch` \(_ :: SomeException) -> return ()
#endif
  Just r <- readIORef lastRequest
  path r `shouldBe` expectedPath


lastRequest :: IORef (Maybe Network.HTTP.Client.Request)
lastRequest = unsafePerformIO $ newIORef Nothing
{-# NOINLINE lastRequest #-}


mgr :: Manager
mgr = unsafePerformIO . newManager $ defaultManagerSettings
  { managerModifyRequest = \req -> writeIORef lastRequest (Just req) >> return req
#if MIN_VERSION_http_client(0,5,0)
  , managerResponseTimeout = responseTimeoutMicro 1
#else
  , managerResponseTimeout = Just 1
#endif
  }
{-# NOINLINE mgr #-}




#if MIN_VERSION_servant_client(0,9,0)
cliA :: Maybe String -> ClientM String
cliB :: Maybe Int -> Maybe Int -> ClientM String
cliC :: Int -> Maybe Int -> Bool -> ClientM String
#else
cliA :: Maybe String -> Manager -> BaseUrl -> ClientM String
cliB :: Maybe Int -> Maybe Int -> Manager -> BaseUrl -> ClientM String
cliC :: Int -> Maybe Int -> Bool -> Manager -> BaseUrl -> ClientM String
#endif
cliA :<|> cliB :<|> cliC = client api
