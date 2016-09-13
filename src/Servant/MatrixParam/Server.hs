{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | If you use @'MatrixParam' "author" String@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @'Maybe' 'String'@.
--
-- This lets servant worry about looking it up in the query string
-- and turning it into a value of the type you specify, enclosed
-- in 'Maybe', because it may not be there and servant would then
-- hand you 'Nothing'.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromHttpApiData' for your type.
--
-- Example:
--
-- >>> import Network.Wai
-- >>> import Servant
-- >>>
-- >>> type Api = WithMatrixParams "books" '[MatrixParam "author" String] :> Get '[JSON] [String]
-- >>> let api = Proxy :: Proxy Api
-- >>>
-- >>> -- serveBooks has type @Maybe String -> ExceptT ServantErr IO [String]@
-- >>> let serveBooks mAuthor = return $ case mAuthor of {Just "alice" -> ["Alice's Diary"]; _ -> []}
-- >>>
-- >>> let app = serve api serveBooks :: Application
--
-- You can also have multiple matrix parameters per path segment.
module Servant.MatrixParam.Server where

import           Prelude hiding (lookup)

import           Data.Map.Strict
import           Data.Proxy
import           Data.String.Conversions
import           Data.Text as T hiding (last)
import           GHC.TypeLits
import           Servant.API
import           Servant.Server
import           Servant.Server.Internal

import           Servant.MatrixParam
import           Servant.MatrixParam.Server.Internal

instance (KnownSymbol path, MatrixParamList params, HasServer api context) =>
  HasServer (WithMatrixParams path params :> api) context where

  type ServerT (WithMatrixParams path params :> api) m =
    AddMatrixParams params (ServerT api m)

  route Proxy context delayed =
    CaptureRouter $ \ first -> case parsePathSegment first of
      Just segment -> if wantedPath == segmentPath segment
        then route apiProxy context $ routeMatrixParams paramsProxy (getSegmentParams segment) delayed
        else delayedFail err404
      Nothing -> delayedFail err400
    where
      apiProxy :: Proxy api
      apiProxy = Proxy

      paramsProxy :: Proxy params
      paramsProxy = Proxy

      wantedPath :: Text
      wantedPath = cs $ symbolVal (Proxy :: Proxy path)

-- addCapture (f e) go
-- go will generate an hlist
-- f will transform function into hlist-taking one
data MList a where
  MNil :: MList '[]
  MCons :: Maybe a -> MList as -> MList (Matrix key a ': as)

class FromSegmentMap a where
  fromSegmentMap :: Map Text Text -> a

instance FromSegmentMap (HList '[]) where
  fromSegmentMap _ = HNil

instance (FromHttpApiData (WithMatrixParams as))
  => FromHttpApiData (WithMatrixParams path (a ': as)) where
  parseUrlPiece t =


class MatrixParamList (params :: [*]) where
  type AddMatrixParams params a :: *

  routeMatrixParams :: Proxy params ->
    Delayed env (AddMatrixParams params a) -> Delayed env a

instance MatrixParamList '[] where
  type AddMatrixParams '[] a = a

  routeMatrixParams Proxy _ delayed = delayed

instance (KnownSymbol key, FromHttpApiData value, MatrixParamList rest) =>
  MatrixParamList (MatrixParam key value ': rest) where

  type AddMatrixParams (MatrixParam key value ': rest) a =
    Maybe value -> AddMatrixParams rest a

  routeMatrixParams Proxy delayed =
    routeMatrixParams restProxy $
    addCapture delayed $ \txt -> case parsePathSegment txt of case lookup key params of
      Nothing -> delayedFail err400
      Just rawValue -> case parseQueryParam rawValue of
        Right value -> return $ Just value
        Left _ -> delayedFail err400
    where
      key :: Text
      key = cs $ symbolVal (Proxy :: Proxy key)

      restProxy :: Proxy rest
      restProxy = Proxy
