{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.MatrixParam.AesonSpecs where

import           Data.Proxy
import           Servant.API
import           Servant.Aeson.Internal

import           Servant.MatrixParam

instance HasGenericSpecs api => HasGenericSpecs (MatrixParam name a :> api) where
  collectRoundtripSpecs Proxy = collectRoundtripSpecs (Proxy :: Proxy api)
