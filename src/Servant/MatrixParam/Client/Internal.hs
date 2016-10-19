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
  ( HasClient (WMP path mat :> api)
  , KnownSymbol path
  )
  => HasClient (WithMatrixParams path mat :> api) where

  type Client (WithMatrixParams path mat :> api)
    = Client (WMP path mat :> api)

  clientWithRoute Proxy req =
    clientWithRoute (Proxy :: Proxy (WMP path mat :> api))
                    req { reqPath = reqPath req ++ "/" ++ path }
    where
     path = symbolVal (Proxy :: Proxy path)

-- This is just a dummy used to keep track of whether we have already processed
-- the leading path. If we are in a WMP instance, we have already delt with the
-- leading path, and can just proceed recursive over the matrix params. If not,
-- we deal with the leading path, and call the WMP instance. WMP itself should
-- not be exported.
data WMP (p :: Symbol) (x :: [*])

instance
  ( HasClient (WMP path rest :> api)
  , ToHttpApiData v
  , KnownSymbol k
  ) => HasClient (WMP path (MatrixParam k v ': rest) :> api) where

  type Client (WMP path (MatrixParam k v ': rest) :> api)
    = Maybe v -> Client (WMP path rest :> api)

  clientWithRoute Proxy req x = case x of
    Nothing -> clientWithRoute nextProxy req
    Just v  -> clientWithRoute nextProxy
      req { reqPath = reqPath req ++ ";" ++ key ++ "=" ++ T.unpack (toQueryParam v) }
    where
      nextProxy :: Proxy (WMP path rest :> api)
      nextProxy = Proxy

      key :: String
      key = symbolVal (Proxy :: Proxy k)

instance
  ( HasClient api
  ) => HasClient (WMP path '[] :> api) where

  type Client (WMP path '[] :> api) = Client api

  clientWithRoute Proxy req =
    clientWithRoute (Proxy :: Proxy api) req
