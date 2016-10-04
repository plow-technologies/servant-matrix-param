{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.MatrixParam.Server.Internal (
  MatrixSegment(..),
  getSegmentParams,
  parsePathSegment,
) where

import           Data.List (foldl')
import           Data.Map.Strict
import           Data.Maybe
import           Data.Text


data MatrixSegment
  = MatrixSegment {
    segmentPath :: Text,
    segmentParams :: Maybe (Map Text Text)
  }
  deriving (Show, Eq)

getSegmentParams :: MatrixSegment -> Map Text Text
getSegmentParams = fromMaybe mempty . segmentParams

parsePathSegment :: Text -> Maybe MatrixSegment
parsePathSegment segment = case splitOn ";" segment of
  [] -> return $ MatrixSegment "" mempty
  [path] -> return $ MatrixSegment path mempty
  [path, ""] -> return $ MatrixSegment path mempty
  (path : pairs) ->
    Just $ MatrixSegment path $
      fmap collectPairs $ mapM parsePair pairs

parsePair :: Text -> Maybe (Text, Maybe Text)
parsePair pair = case splitOn "=" pair of
  [key, value] -> Just (key, Just value)
  [flag] -> Just (flag, Nothing)
  _ -> Nothing

collectPairs :: forall a b . Ord a => [(a, Maybe b)] -> Map a b
collectPairs =
  Data.List.foldl'
    (\ acc (k, mv) -> maybe acc (\ v -> insert k v acc) mv)
    mempty
