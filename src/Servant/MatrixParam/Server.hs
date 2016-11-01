{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

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

import           Data.Proxy
import           Data.String.Conversions
import           Data.Text as T hiding (last)
import           GHC.TypeLits
import           Servant.API
import           Servant.Server
import           Servant.Server.Internal

import           Servant.MatrixParam
import           Servant.MatrixParam.Server.Internal
import           Servant.MatrixParam.Server.Internal.ArgList

instance (KnownSymbol path, HasServer api context
        , App (Unapped params (ServerT api Handler)) params
        , Apped (Unapped params (ServerT api Handler)) params ~ ServerT api Handler
        , ParseArgs params
        ) =>
  HasServer (WithMatrixParams path params :> api) context where

  type ServerT (WithMatrixParams path params :> api) m =
    Unapped params (ServerT api m)

  route Proxy context delayed =
    CaptureRouter $
        route (Proxy :: Proxy api)
              context
              (addMatrices delayed $ \ txt -> case parsePathSegment txt of
                 Nothing -> delayedFail err400
                 Just segment -> if wantedPath == segmentPath segment
                   then return $ (parseArgs $ getSegmentParams segment :: ArgList params)
                   else delayedFail err400
              )
    where
      wantedPath :: Text
      wantedPath = cs $ symbolVal (Proxy :: Proxy path)

instance (HasServer api context
        , App (Unapped params (ServerT api Handler)) params
        , FromHttpApiData captureType
        , Apped (Unapped params (ServerT api Handler)) params
          ~ (ServerT api Handler)
        , ParseArgs params
        ) =>
  HasServer (CaptureWithMatrixParams path captureType params :> api) context where

  type ServerT (CaptureWithMatrixParams path captureType params :> api) m =
    captureType -> Unapped params (ServerT api m)

  route Proxy context delayed =
    CaptureRouter $
        route (Proxy :: Proxy api)
              context
              (addCapturedMatrices delayed $ \ txt -> case parsePathSegment txt of
                 Nothing -> delayedFail err400
                 Just segment -> case parseUrlPiece (segmentPath segment) of
                   Left _ -> delayedFail err400
                   Right value -> return
                     $ (value, parseArgs $ getSegmentParams segment :: ArgList params)
              )

addMatrices :: App fn argList => Delayed env fn
           -> (captured -> DelayedIO (ArgList argList))
           -> Delayed (captured, env) (Apped fn argList)
addMatrices Delayed{..} new =
  Delayed
    { capturesD = \ (txt, env) -> (,) <$> capturesD env <*> new txt
    , serverD   = \ (x, v) a b req -> (`apply` v)  <$> serverD x a b req
    , ..
    }

addCapturedMatrices
  :: ( App restOfFn argList
     , fn ~ (captureType -> restOfFn)
     , Apped restOfFn argList ~ result)
  => Delayed env fn
  -> (captured -> DelayedIO (captureType, ArgList argList))
  -> Delayed (captured, env) result
addCapturedMatrices Delayed{..} new =
  Delayed
    { capturesD = \ (txt, env) -> (,) <$> capturesD env <*> new txt
    , serverD   = \ (x, (capture, matrixParams)) a b req ->
        let appCapture = ($ capture) <$> serverD x a b req
        in (`apply` matrixParams) <$> appCapture
    , ..
    }
