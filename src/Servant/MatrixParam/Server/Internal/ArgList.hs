{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Servant.MatrixParam.Server.Internal.ArgList where

import qualified Data.Map            as Map
import           Data.Proxy
import qualified Data.Text           as T
import           GHC.TypeLits        (KnownSymbol, symbolVal)
import           Servant.MatrixParam
import           Web.HttpApiData     (parseQueryParamMaybe, FromHttpApiData)

-- An HList that stores the arguments to a function
data ArgList a where
  NoArgs :: ArgList '[]
  (:.:)  :: Maybe a -> ArgList as -> ArgList (MatrixParam key a ': as)

type family Unapped (params :: [*]) end where
  Unapped '[] r = r
  Unapped (MatrixParam key a ': rest) r = Maybe a -> Unapped rest r

class (Apped (Unapped argList fn) argList ~ fn) => App fn (argList :: [*]) where
  type Apped fn argList
  -- Applies a function to an ArgList
  apply :: fn -> ArgList argList -> Apped fn argList

class ParseArgs argList where
  -- Gets the arguments from a map
  parseArgs :: Map.Map T.Text T.Text -> ArgList argList

instance (App r rest
        , Apped (Unapped rest (Maybe a -> r)) rest ~ (Maybe a -> r)
        ) => App (Maybe a -> r) (MatrixParam key a ': rest) where
  type Apped (Maybe a -> r) (MatrixParam key a ': rest) = Apped r rest
  apply fn (a :.: rest) = apply (fn a) rest

instance (FromHttpApiData a, KnownSymbol key, ParseArgs rest) => ParseArgs (MatrixParam key a ': rest) where
  parseArgs m = val :.: (parseArgs m :: ArgList rest)
    where
      key = T.pack $ symbolVal (Proxy :: Proxy key)
      val = Map.lookup key m >>= parseQueryParamMaybe

instance App r '[] where
  type Apped r '[] = r
  apply r _ = r

instance ParseArgs '[] where
  parseArgs _ = NoArgs
