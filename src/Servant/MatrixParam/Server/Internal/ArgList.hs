{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Servant.MatrixParam.Server.Internal.ArgList where

import           Control.Monad       (join)
import           Data.Char
import qualified Data.Map            as Map
import           Data.Proxy
import qualified Data.Text           as T
import           GHC.TypeLits        (KnownSymbol, symbolVal)
import           Servant.MatrixParam
import           Web.HttpApiData     (FromHttpApiData, parseQueryParamMaybe)

-- An HList that stores the arguments to a function
data ArgList a where
  NoArgs :: ArgList '[]
  -- A matrix param (always optional)
  (:.:)  :: Maybe a -> ArgList as -> ArgList (MatrixParam key a ': as)
  -- A matrix flag
  (:?:)  :: Bool -> ArgList as -> ArgList (MatrixFlag key ': as)

type family Unapped (params :: [*]) end where
  Unapped '[] r = r
  Unapped (MatrixParam key a ': rest) r = Maybe a -> Unapped rest r
  Unapped (MatrixFlag key ': rest) r = Bool -> Unapped rest r

class (Apped (Unapped argList fn) argList ~ fn) => App fn (argList :: [*]) where
  type Apped fn argList
  -- Applies a function to an ArgList
  apply :: fn -> ArgList argList -> Apped fn argList

class ParseArgs argList where
  -- Gets the arguments from a map
  parseArgs :: Map.Map T.Text (Maybe T.Text) -> ArgList argList

instance (App r rest
        , Apped (Unapped rest (Maybe a -> r)) rest ~ (Maybe a -> r)
        ) => App (Maybe a -> r) (MatrixParam key a ': rest) where
  type Apped (Maybe a -> r) (MatrixParam key a ': rest) = Apped r rest
  apply fn (a :.: rest) = apply (fn a) rest

instance (App r rest
        , Apped (Unapped rest (Bool -> r)) rest ~ (Bool -> r)
        ) => App (Bool -> r) (MatrixFlag key ': rest) where
  type Apped (Bool -> r) (MatrixFlag key ': rest) = Apped r rest
  apply fn (a :?: rest) = apply (fn a) rest

instance (FromHttpApiData a, KnownSymbol key, ParseArgs rest) => ParseArgs (MatrixParam key a ': rest) where
  parseArgs m = val :.: (parseArgs m :: ArgList rest)
    where
      key = T.pack $ symbolVal (Proxy :: Proxy key)
      val = join $ Map.lookup key m >>= fmap parseQueryParamMaybe

instance (KnownSymbol key, ParseArgs rest) => ParseArgs (MatrixFlag key ': rest) where
  parseArgs m = val :?: (parseArgs m :: ArgList rest)
    where
      key = T.pack $ symbolVal (Proxy :: Proxy key)
      val = case Map.lookup key m of
        Nothing -> False
        Just Nothing -> True
                          -- Mimicking prior behaviour
        Just (Just x) -> T.map toLower x == "true" || x == "1"

instance App r '[] where
  type Apped r '[] = r
  apply r _ = r

instance ParseArgs '[] where
  parseArgs _ = NoArgs
