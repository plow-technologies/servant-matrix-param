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
import Data.Maybe          (fromMaybe)
import Data.Proxy
import Network.HTTP.Client (Manager, Request, defaultManagerSettings,
                            managerModifyRequest, managerResponseTimeout,
                            newManager, path)
import Servant
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.Client
import System.IO.Unsafe
import Test.Hspec

#if MIN_VERSION_http_client(0,5,0)
import Network.HTTP.Client (responseTimeoutMicro)
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

    it "generates paths that servant-server understands" $ do
      testWithApplication (return $ serve api server) $ \port -> do
        let res = (,) <$> cliA (Just "There is a there there")
                      <*> cliB (Just 1) (Just 2)
            url = BaseUrl Http "localhost" port ""
        mgr' <- newManager defaultManagerSettings
        runClientM res (ClientEnv mgr' url)
          `shouldReturn` Right ("There is a there there", "3")
------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------

type Api =
       WithMatrixParams "a" '[MatrixParam "name" String] :> Get '[JSON] String
  :<|> WithMatrixParams "b" '[MatrixParam "foo" Int, MatrixParam "bar" Int]
         :> Get '[JSON] String

api :: Proxy Api
api = Proxy

server :: Server Api
server = e1 :<|> e2
  where
    e1 name = return $ fromMaybe "" name
    e2 foo bar = return . show $ fromMaybe 0 foo + fromMaybe 0 bar

------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------

hasRequestPath :: ClientM any -> ByteString -> IO ()
hasRequestPath c expectedPath = do
  let anyBurl = BaseUrl Http "localhost" 6660 ""
  _ <- void (runClientM c $ ClientEnv mgr anyBurl)
    `catch` \(_ :: SomeException) -> return ()
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



cliA :: Maybe String -> ClientM String
cliB :: Maybe Int -> Maybe Int -> ClientM String
cliA :<|> cliB = client api
