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

module Servant.MatrixParam.Internal where

import           Data.List       (foldl')
import           Data.Map.Strict
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy      (Proxy (..))
import qualified Data.Text       as T
import           Data.Typeable   (Typeable)
import           GHC.TypeLits    (KnownSymbol, Symbol, symbolVal)
import           Web.HttpApiData

import Servant.API (Capture)


-- | Expresses matrix parameters for path segments in APIs, e.g.
--
-- @/books;author=<author name>@
--
-- would be represented as:
--
-- >>> import Servant
-- >>>
-- >>> type MyApi = WithMatrixParams "book" '[MatrixParam "author" String] :> Get '[JSON] [String]
type WithMatrixParams path params = Capture "matrixParamCapture" (MatrixParams path params)

data MatrixParams (path :: Symbol) (paramSpecs :: [*]) where
  MatrixEmpty :: MatrixParams path '[]
  (:.:) :: Maybe a -> MatrixParams path as -> MatrixParams path (MatrixParam seg a ': as)
    deriving (Typeable)
infixr 3 :.:

instance (KnownSymbol path, ParseMatrix (MatrixParams path ls))
      => FromHttpApiData (MatrixParams path ls) where
  parseUrlPiece t = case parsePathSegment t of
    Nothing -> Left "Could not parse path segment"
    Just v  -> if segmentPath v == expected
      then return $ parseMatrix $ getSegmentParams v
      else Left $ "Expected path to be " <> expected
      where
        expected = T.pack $ symbolVal (Proxy :: Proxy path)

class ParseMatrix a where
  parseMatrix :: M.Map T.Text T.Text -> a

instance ParseMatrix (MatrixParams path '[]) where
  parseMatrix _ = MatrixEmpty

instance ( ParseMatrix (MatrixParams path as)
         , KnownSymbol seg
         , FromHttpApiData a
         ) => ParseMatrix (MatrixParams path (MatrixParam seg a ': as)) where
  parseMatrix m =  (M.lookup seg m >>= parseUrlPieceMaybe)
               :.: parseMatrix m
    where
      seg = T.pack $ symbolVal (Proxy :: Proxy seg)


data MatrixParam (key :: Symbol) a
    deriving (Typeable)

-- * Matrix Segments

data MatrixSegment
  = MatrixSegment {
    segmentPath   :: T.Text,
    segmentParams :: Maybe (M.Map T.Text T.Text)
  }
  deriving (Show, Eq)

getSegmentParams :: MatrixSegment -> M.Map T.Text T.Text
getSegmentParams = fromMaybe mempty . segmentParams

parsePathSegment :: T.Text -> Maybe MatrixSegment
parsePathSegment segment = case T.splitOn ";" segment of
  [] -> return $ MatrixSegment "" mempty
  [path] -> return $ MatrixSegment path mempty
  [path, ""] -> return $ MatrixSegment path mempty
  (path : pairs) ->
    Just $ MatrixSegment path $
      fmap collectPairs $ mapM parsePair pairs

parsePair :: T.Text -> Maybe (T.Text, Maybe T.Text)
parsePair pair = case T.splitOn "=" pair of
  [key, value] -> Just (key, Just value)
  [flag] -> Just (flag, Nothing)
  _ -> Nothing

collectPairs :: forall a b . Show b => Ord a => [(a, Maybe b)] -> Map a b
collectPairs =
  Data.List.foldl'
    (\ acc (k, mv) -> maybe acc (\ v -> insert k v acc) mv)
    mempty
