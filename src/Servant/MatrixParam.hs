{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.MatrixParam (
  WithMatrixParams,
  MatrixParam,
) where

import           Data.Typeable (Typeable)
import           GHC.TypeLits  (Symbol)

data WithMatrixParams (path :: Symbol) (paramSpecs :: [*])
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
