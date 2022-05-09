{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.MatrixParam (
  WithMatrixParams,
  CaptureWithMatrixParams,
  MatrixParam,
  MatrixFlag,
) where

import Data.Typeable (Typeable)
import GHC.TypeLits  (Symbol)
import Data.Kind (Type)

-- | A static path followed by one or more matrix parameters.
data WithMatrixParams (path :: Symbol) (paramSpecs :: [Type])
    deriving (Typeable)

-- | A capture followed by one or more matrix parameters.
data CaptureWithMatrixParams (path :: Symbol) captureType (paramSpecs :: [Type])
    deriving (Typeable)

-- | Expresses matrix parameters for path segments in APIs, e.g.
--
-- @/books;author=<author name>@
--
-- would be represented as:
--
-- >>> import Servant
-- >>>
-- >>> type MyApi = "books" :> MatrixParam "author" String :> Get '[JSON] [String]
data MatrixParam (key :: Symbol) a
    deriving (Typeable)

-- | Lookup a potentially value-less matrix string parameter
-- with boolean semantics. If the param @sym@ is there without any value,
-- or if it's there with value "true" or "1", it's interpreted as 'True'.
-- Otherwise, it's interpreted as 'False'.
--
--
-- @/book;published@
--
-- would be represented as:
--
-- >>> import Servant
-- >>>
-- >>> type MyApi = "book" :> MatrixFlag "published" :> Get '[JSON] [String]
data MatrixFlag (key :: Symbol)
    deriving (Typeable)
