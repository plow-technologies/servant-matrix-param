{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Servant.MatrixParam.ClientSpec (spec) where

import Data.ByteString     (ByteString)
import Data.IORef
import Data.Proxy
import Network.HTTP.Client (Manager, Request, defaultManagerSettings,
                            managerModifyRequest, managerResponseTimeout,
                            newManager, path)
import Network.HTTP.Types  (ok200)
import Servant.API
import Servant.Client
import System.IO.Unsafe
import Test.Hspec

import Servant.MatrixParam
import Servant.MatrixParam.Client

spec :: Spec
spec = do

  describe "Servant.API.MatrixParam" $ do

    it "correctly passes the argument to the server" $ do
      cliA (Just "alice") `hasRequestPath` "a;name=alice"
      cliB (Just 3) (Just 2) `hasRequestPath` "b;foo=3&bar=2"

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------

type Api =
       WithMatrixParams "a" '[MatrixParam "name" String] :> Get '[JSON] String
  :<|> WithMatrixParams "b" '[MatrixParam "foo" Int, MatrixParam "bar" Int]
         :> Get '[JSON] String

api :: Proxy Api
api = Proxy

------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------

hasRequestPath :: ClientM any -> ByteString -> IO ()
hasRequestPath c expectedPath = do
  _ <- runClientM c (ClientEnv mgr undefined)
  Just r <- readIORef lastRequest
  path r `shouldBe` expectedPath


lastRequest :: IORef (Maybe Network.HTTP.Client.Request)
lastRequest = unsafePerformIO $ newIORef Nothing
{-# NOINLINE lastRequest #-}


mgr :: Manager
mgr = unsafePerformIO . newManager $ defaultManagerSettings
  { managerModifyRequest = \req -> writeIORef lastRequest (Just req) >> return req
  , managerResponseTimeout = 1
  }
{-# NOINLINE mgr #-}



cliA :: Maybe String -> ClientM String
cliB :: Maybe Int -> Maybe Int -> ClientM String
cliA :<|> cliB = client api
