{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.MatrixParam.Server.Internal (
  MatrixSegment(..),
  getSegmentParams,
  parsePathSegment,
) where

import           Data.Map.Strict
import           Data.Maybe
import           Data.Text


data MatrixSegment
  = MatrixSegment {
    segmentPath :: Text,
    segmentParams :: Maybe (Map Text (Maybe Text))
  }
  deriving (Show, Eq)

getSegmentParams :: MatrixSegment -> Map Text (Maybe Text)
getSegmentParams = fromMaybe mempty . segmentParams

parsePathSegment :: Text -> Maybe MatrixSegment
parsePathSegment segment = case splitOn ";" segment of
  [] -> return $ MatrixSegment "" mempty
  [path] -> return $ MatrixSegment path mempty
  [path, ""] -> return $ MatrixSegment path mempty
  (path : pairs) ->
    Just $ MatrixSegment path $
      fmap fromList $ mapM parsePair pairs

parsePair :: Text -> Maybe (Text, Maybe Text)
parsePair pair = case splitOn "=" pair of
  [key, value] -> Just (key, Just value)
  [flag] -> Just (flag, Nothing)
  _ -> Nothing
