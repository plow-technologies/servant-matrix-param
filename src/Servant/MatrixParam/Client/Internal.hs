{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.MatrixParam.Client.Internal where

import           Data.Proxy
import qualified Data.Text           as T
import           GHC.TypeLits
import           Servant.API
import           Servant.Client
import           Servant.Common.Req
import           Servant.MatrixParam

instance
  ( HasClient (WMP mat :> api)
  , KnownSymbol path
  )
  => HasClient (WithMatrixParams path mat :> api) where

  type Client (WithMatrixParams path mat :> api)
    = Client (WMP mat :> api)

  clientWithRoute Proxy req =
    clientWithRoute (Proxy :: Proxy (WMP mat :> api))
                    req { reqPath = reqPath req ++ "/" ++ path }
    where
     path = symbolVal (Proxy :: Proxy path)

instance
  ( HasClient (WMP mat :> api)
  , ToHttpApiData captureType
  )
  => HasClient (CaptureWithMatrixParams info captureType mat :> api) where

  type Client (CaptureWithMatrixParams info captureType mat :> api)
    = captureType -> Client (WMP mat :> api)

  clientWithRoute Proxy req = \capture ->
    clientWithRoute (Proxy :: Proxy (WMP mat :> api))
                    req { reqPath = reqPath req ++ "/" ++ T.unpack (toUrlPiece capture) }


-- This is just a dummy used to keep track of whether we have already processed
-- the leading path. If we are in a WMP instance, we have already dealt with
-- the leading path, and can just proceed recursively over the matrix params.
-- If not, we deal with the leading path, and call the WMP instance. WMP itself
-- should not be exported.
data WMP (x :: [*])

instance
  ( HasClient (WMP rest :> api)
  , ToHttpApiData v
  , KnownSymbol k
  ) => HasClient (WMP (MatrixParam k v ': rest) :> api) where

  type Client (WMP (MatrixParam k v ': rest) :> api)
    = Maybe v -> Client (WMP rest :> api)

  clientWithRoute Proxy req x = case x of
    Nothing -> clientWithRoute nextProxy req
    Just v  -> clientWithRoute nextProxy
      req { reqPath = reqPath req ++ ";" ++ key ++ "=" ++ T.unpack (toQueryParam v) }
    where
      nextProxy :: Proxy (WMP rest :> api)
      nextProxy = Proxy

      key :: String
      key = symbolVal (Proxy :: Proxy k)

instance
  ( HasClient api
  ) => HasClient (WMP '[] :> api) where

  type Client (WMP '[] :> api) = Client api

  clientWithRoute Proxy req =
    clientWithRoute (Proxy :: Proxy api) req
