{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.MatrixParam (
  WithMatrixParams(..),
  MatrixParam,
) where

import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Proxy      (Proxy (..))
import qualified Data.Text       as T
import           Data.Typeable   (Typeable)
import           GHC.TypeLits    (KnownSymbol, Symbol, symbolVal)
import           Web.HttpApiData

import Servant.MatrixParam.Internal

data WithMatrixParams (path :: Symbol) (paramSpecs :: [*]) where
  MatrixEmpty :: WithMatrixParams path '[]
  (:.:) :: Maybe a -> WithMatrixParams path as -> WithMatrixParams path (MatrixParam seg a ': as)
    deriving (Typeable)

instance (KnownSymbol path, ParseMatrix (WithMatrixParams path ls))
      => FromHttpApiData (WithMatrixParams path ls) where
  parseUrlPiece t = case parsePathSegment t of
    Nothing -> Left "Could not parse path segment"
    Just v  -> if segmentPath v == expected
      then return $ parseMatrix $ getSegmentParams v
      else Left $ "Expected path to be " <> expected
      where
        expected = T.pack $ symbolVal (Proxy :: Proxy path)

class ParseMatrix a where
  parseMatrix :: M.Map T.Text T.Text -> a

instance ParseMatrix (WithMatrixParams path '[]) where
  parseMatrix _ = MatrixEmpty

instance ( ParseMatrix (WithMatrixParams path as)
         , KnownSymbol seg
         , FromHttpApiData a
         ) => ParseMatrix (WithMatrixParams path (MatrixParam seg a ': as)) where
  parseMatrix m =  (M.lookup seg m >>= parseUrlPieceMaybe)
               :.: parseMatrix m
    where
      seg = T.pack $ symbolVal (Proxy :: Proxy seg)



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
