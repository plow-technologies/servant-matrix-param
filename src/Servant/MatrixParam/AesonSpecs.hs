{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.MatrixParam.AesonSpecs where

import           Data.Proxy
import           Servant.API
import           Servant.Aeson.Internal

import           Servant.MatrixParam

instance HasGenericSpecs api => HasGenericSpecs (WithMatrixParams path params :> api) where
#if MIN_VERSION_servant_aeson_specs(0,5,0)
  collectRoundtripSpecs settings Proxy = collectRoundtripSpecs settings (Proxy :: Proxy api)
#else
  collectRoundtripSpecs Proxy = collectRoundtripSpecs (Proxy :: Proxy api)
#endif
