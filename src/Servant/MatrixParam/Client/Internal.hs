{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.MatrixParam.Client.Internal where

import           Data.Proxy
import qualified Data.Text           as T
import           GHC.TypeLits
import           Servant.API
import           Servant.Client
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8   as BS
import qualified Network.HTTP.Types.URI as HTTP
import           Servant.Client.Core
import           Servant.MatrixParam
import           Data.Monoid


instance
  ( HasClient m (WMP mat :> api)
  , KnownSymbol path
  )
  => HasClient m (WithMatrixParams path mat :> api) where

  type Client m (WithMatrixParams path mat :> api)
    = Client m (WMP mat :> api)

  clientWithRoute p Proxy req =
    clientWithRoute p (Proxy :: Proxy (WMP mat :> api))
                    req { requestPath = (requestPath req) <> "/" <> path }
    where
     path = HTTP.urlEncodeBuilder False $ BS.pack $  symbolVal (Proxy :: Proxy path)

instance
  ( HasClient m (WMP mat :> api)
  , ToHttpApiData captureType
  )
  => HasClient m (CaptureWithMatrixParams info captureType mat :> api) where

  type Client m (CaptureWithMatrixParams info captureType mat :> api)
    = captureType -> Client m (WMP mat :> api)

  clientWithRoute m Proxy req = \capture ->
    clientWithRoute m (Proxy :: Proxy (WMP mat :> api))
                    req { requestPath = ( requestPath req) <> "/" <> ((HTTP.urlEncodeBuilder False . BS.pack . T.unpack) (toUrlPiece capture) )}


-- This is just a dummy used to keep track of whether we have already processed
-- the leading path. If we are in a WMP instance, we have already dealt with
-- the leading path, and can just proceed recursively over the matrix params.
-- If not, we deal with the leading path, and call the WMP instance. WMP itself
-- should not be exported.
data WMP (x :: [*])

instance
  ( HasClient m (WMP rest :> api)
  , ToHttpApiData v
  , KnownSymbol k
  ) => HasClient m (WMP (MatrixParam k v ': rest) :> api) where

  type Client m (WMP (MatrixParam k v ': rest) :> api)
    = Maybe v -> Client m (WMP rest :> api)

  clientWithRoute p old req x = case x of
    Nothing -> clientWithRoute p nextProxy req
    Just v  -> clientWithRoute p nextProxy
      (req { requestPath = (requestPath req) <> ";" <> key <> "=" <> (HTTP.urlEncodeBuilder False . BS.pack . T.unpack) (toQueryParam v) })
    where
      nextProxy :: Proxy (WMP rest :> api)
      nextProxy = Proxy

      key :: BS.Builder
      key = HTTP.urlEncodeBuilder False $ BS.pack $ symbolVal (Proxy :: Proxy k)

instance
  ( HasClient m (WMP rest :> api)
  , KnownSymbol k
  ) => HasClient m (WMP (MatrixFlag k ': rest) :> api) where

  type Client m (WMP (MatrixFlag k ': rest) :> api)
    = Bool -> Client m (WMP rest :> api)

  clientWithRoute p Proxy req flag
    | flag = clientWithRoute p nextProxy
        req { requestPath = requestPath req <> ";" <> key }
    | otherwise = clientWithRoute p nextProxy req
    where
      nextProxy :: Proxy (WMP rest :> api)
      nextProxy = Proxy

      key :: BS.Builder
      key = HTTP.urlEncodeBuilder False $ BS.pack $ symbolVal (Proxy :: Proxy k)

instance
  ( HasClient m api
  ) => (HasClient m) (WMP '[] :> api) where

  type Client m (WMP '[] :> api) = Client m api
 
  clientWithRoute Proxy Proxy req =
    clientWithRoute (Proxy :: Proxy m) (Proxy :: Proxy api) req
