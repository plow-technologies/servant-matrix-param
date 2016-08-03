{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.MatrixParam.AesonSpecsSpec where

import           Data.Typeable
import           Servant.API
import           Servant.Aeson.GenericSpecs
import           Test.Hspec

import           Servant.MatrixParam
import           Servant.MatrixParam.AesonSpecs ()

spec :: Spec
spec = do
  it "has an instance for HasGenericSpecs" $ do
    usedTypes matrixParamApi `shouldBe` [boolRep]

matrixParamApi :: Proxy (MatrixParam "foo" String :> Get '[JSON] Bool)
matrixParamApi = Proxy

boolRep :: TypeRep
boolRep = typeRep (Proxy :: Proxy Bool)
