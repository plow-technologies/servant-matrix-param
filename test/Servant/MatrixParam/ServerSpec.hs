{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

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
import           Servant.MatrixParam.Server ()
import           Servant.MatrixParam.Server.Internal

type Api =
       WithMatrixParams "a" '[MatrixParam "name" String] :> Get '[PlainText] String
  :<|> WithMatrixParams "b" '[MatrixParam "foo" Int, MatrixParam "bar" Int]
         :> Get '[PlainText] String
  :<|> WithMatrixParams "c" '[MatrixParam "foo" Int, MatrixFlag "baz", MatrixParam "bar" Int]
         :> Get '[PlainText] String

api :: Proxy Api
api = Proxy

server :: Server Api
server = a :<|> b :<|> c
  where
    a :: Maybe String -> Handler String
    a p = return $ show p

    b :: Maybe Int -> Maybe Int -> Handler String
    b foo bar = return $ show (foo, bar)

    c :: Maybe Int -> Bool -> Maybe Int -> Handler String
    c foo baz bar | baz = return $ show (foo, bar)
                  | otherwise = return ""

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

    it "retrieves matrix flags" $ do
      testWithPath ["c;baz"] "(Nothing,Nothing)"
      testWithPath ["c;foo=1;baz"] "(Just 1,Nothing)"
      testWithPath ["c;bar=1;baz"] "(Nothing,Just 1)"
      testWithPath ["c;bar=1;baz;foo=2"] "(Just 2,Just 1)"
      testWithPath ["c;foo=1"] ""

    it "treats 'true' as a flag that's present" $ do
      testWithPath ["c;baz=true"] "(Nothing,Nothing)"

    it "treats '1' as a flag that's present" $ do
      testWithPath ["c;baz=1"] "(Nothing,Nothing)"

    it "allows to omit matrix parameters" $ do
      testWithPath ["a"] "Nothing"

    it "allows multiple keys per segment" $ do
      testWithPath ["b;foo=23;bar=42"] "(Just 23,Just 42)"

    it "allows to overwrite matrix params" $ do
      testWithPath ["a;name=alice;name=bob"] "Just \"bob\""


  describe "parsePathSegment" $ do
    it "parses a path segment with a matrix param" $ do
      parsePathSegment "foo;bar=baz" `shouldBe`
        Just (MatrixSegment "foo" (Just (fromList [("bar", Just "baz")])))

    it "parses a bare segment" $ do
      parsePathSegment "foo" `shouldBe`
        Just (MatrixSegment "foo" mempty)

    it "parses a segment with multiple matrix params" $ do
      parsePathSegment "foo;bar=baz;huhu=baba" `shouldBe`
        Just (MatrixSegment "foo" (Just (fromList [("bar", Just "baz"),
                                                   ("huhu", Just "baba")])))

    it "parses a segment with multiple values for the same matrix param" $ do
      parsePathSegment "foo;bar=baz;bar=huhu" `shouldBe`
        Just (MatrixSegment "foo" (Just (fromList [("bar", Just "huhu")])))

    it "parses a segment with semicolon but without params" $ do
      parsePathSegment "foo;" `shouldBe`
        Just (MatrixSegment "foo" mempty)

    it "succeeds for invalid matrix parameters strings" $ do
      parsePathSegment "foo;bar==" `shouldBe`
        Just (MatrixSegment "foo" mempty)

    it "can parse the empty string as value" $ do
      parsePathSegment "foo;bar=" `shouldBe`
        Just (MatrixSegment "foo" (Just (fromList [("bar", Just "")])))
