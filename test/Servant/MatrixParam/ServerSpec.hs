{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Servant.MatrixParam.ServerSpec where

import           Control.Monad.IO.Class
import           Data.ByteString.Lazy
import           Data.Map.Strict
import           Data.Proxy
import           Data.Text
import           Network.HTTP.Types (ok200)
import           Network.Wai
import           Network.Wai.Test
import           Servant.API
import           Servant.Server
import           Test.Hspec

import           Servant.MatrixParam
import           Servant.MatrixParam.Internal

type Api =
       WithMatrixParams "a" '[MatrixParam "name" String] :> Get '[PlainText] String
  :<|> WithMatrixParams "b" '[MatrixParam "foo" Int, MatrixParam "bar" Int]
         :> Get '[PlainText] String

api :: Proxy Api
api = Proxy

server :: Server Api
server = a :<|> b
  where
    a :: MatrixParams "a" '[MatrixParam "name" String] -> Handler String
    a (p :.: MatrixEmpty) = return $ show p

    b :: MatrixParams "b" '[MatrixParam "foo" Int, MatrixParam "bar" Int] -> Handler String
    b (foo :.: bar :.: MatrixEmpty) = return $ show (foo, bar)

testWithPath :: [Text] -> ByteString -> IO ()
testWithPath segments expected = do
  (flip runSession) (serve api server) $ do
    response <- Network.Wai.Test.request defaultRequest{
      pathInfo = segments
    }
    liftIO $ do
      simpleStatus response `shouldBe` ok200
      simpleBody response `shouldBe` expected

spec :: Spec
spec = do
  describe "Servant.API.MatrixParam" $ do
    it "allows to retrieve simple matrix parameters" $ do
      testWithPath ["a;name=bob"] "Just \"bob\""

    it "allows to omit matrix parameters" $ do
      testWithPath ["a"] "Nothing"

    it "allows multiple keys per segment" $ do
      testWithPath ["b;foo=23;bar=42"] "(Just 23,Just 42)"

    it "allows to overwrite matrix params" $ do
      testWithPath ["a;name=alice;name=bob"] "Just \"bob\""

  describe "parsePathSegment" $ do
    it "parses a path segment with a matrix param" $ do
      parsePathSegment "foo;bar=baz" `shouldBe`
        Just (MatrixSegment "foo" (Just (fromList [("bar", "baz")])))

    it "parses a bare segment" $ do
      parsePathSegment "foo" `shouldBe`
        Just (MatrixSegment "foo" mempty)

    it "parses a segment with multiple matrix params" $ do
      parsePathSegment "foo;bar=baz;huhu=baba" `shouldBe`
        Just (MatrixSegment "foo" (Just (fromList [("bar", "baz"), ("huhu", "baba")])))

    it "parses a segment with multiple values for the same matrix param" $ do
      parsePathSegment "foo;bar=baz;bar=huhu" `shouldBe`
        Just (MatrixSegment "foo" (Just (fromList [("bar", "huhu")])))

    it "parses a segment with semicolon but without params" $ do
      parsePathSegment "foo;" `shouldBe`
        Just (MatrixSegment "foo" mempty)

    it "succeeds for invalid matrix parameters strings" $ do
      parsePathSegment "foo;bar==" `shouldBe`
        Just (MatrixSegment "foo" mempty)

    it "can parse the empty string as value" $ do
      parsePathSegment "foo;bar=" `shouldBe`
        Just (MatrixSegment "foo" (Just (fromList [("bar", "")])))
