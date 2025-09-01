{-# LANGUAGE AllowAmbiguousTypes #-}
-- Allow redendudant constraints to require IOE for runWarp helpers.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Yf.Effect.Servant (
  interpretServer,
  run,
  runTLS,
  runWarpServerSettingsContext,
  runWarpServerSettings,
  runWarpTLSServerSettingsContext,
  runWarpTLSServerSettings,
  tlsSettings,
  ErrorPage,
  TLSSettings,
) where

import Control.Monad.Except qualified as T
import Data.Kind (Type)
import Effectful (
  Eff,
  Effect,
  IOE,
  Limit (..),
  Persistence (Ephemeral, Persistent),
  UnliftStrategy (..),
  liftIO,
  withEffToIO,
  (:>),
 )
import Effectful.Error.Static qualified as ErrorStatic
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS (TLSSettings, tlsSettings)
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import Servant hiding ((:>))

-- Lots of the code here sourced from
-- https://github.com/Kleidukos/servant-effectful/blob/22af09642078d5296b524495ad8213bf2ace62d2/src/Effectful/Servant.hs

-- | Transform the Eff monad into a servant Handler.
interpretServer ::
  (forall r. Eff es r -> IO r)
  -> Eff (ErrorPage : es) resp
  -> Servant.Handler resp
interpretServer toIO serveResp = do
  result <- liftIO $ toIO (ErrorStatic.runErrorNoCallStack serveResp)
  T.liftEither result

{- | IMPORTANT: Unlift strategy for how to handle doling out the
effect environment to spawned threads.
What (I think) this is saying:
1. Ephemeral: We want web server workers to have a fresh copy
2. Don't place a limit on the number of times that happens
-}
webServerUnliftStrat :: UnliftStrategy
webServerUnliftStrat =
  ConcUnlift
    Ephemeral
    Unlimited

-- | Deploy an effectful TLS server with a context.
runWarpTLSServerSettingsContext ::
  forall (api :: Type) (context :: [Type]) (es :: [Effect]).
  (HasServer api context, ServerContext context, IOE :> es) =>
  TLSSettings
  -> Warp.Settings
  -> Context context
  -> ServerT api (Eff (ErrorPage : es))
  -> Eff es ()
runWarpTLSServerSettingsContext tlsSettings settings ctx server = withEffToIO webServerUnliftStrat $ \toIO ->
  WarpTLS.runTLS
    tlsSettings
    settings
    (Servant.serveWithContextT (Proxy @api) ctx (interpretServer toIO) server)

-- | Deploy an effectful server.
runWarpTLSServerSettings ::
  forall (api :: Type) (es :: [Effect]).
  (HasServer api '[], IOE :> es) =>
  TLSSettings
  -> Warp.Settings
  -> ServerT api (Eff (ErrorPage : es))
  -> Eff es ()
runWarpTLSServerSettings tlsSettings settings =
  runWarpTLSServerSettingsContext @api tlsSettings settings EmptyContext

-- | Deploy an effectful server with a context.
runWarpServerSettingsContext ::
  forall (api :: Type) (context :: [Type]) (es :: [Effect]).
  (HasServer api context, ServerContext context, IOE :> es) =>
  Warp.Settings
  -> Context context
  -> ServerT api (Eff (ErrorPage : es))
  -> Eff es ()
runWarpServerSettingsContext settings ctx server = withEffToIO webServerUnliftStrat $ \fromIO ->
  Warp.runSettings
    settings
    (Servant.serveWithContextT (Proxy @api) ctx (interpretServer fromIO) server)

-- | Deploy an effectful server.
runWarpServerSettings ::
  forall (api :: Type) (es :: [Effect]).
  (HasServer api '[], IOE :> es) =>
  Warp.Settings
  -> ServerT api (Eff (ErrorPage : es))
  -> Eff es ()
runWarpServerSettings settings =
  runWarpServerSettingsContext @api settings EmptyContext

run ::
  forall (api :: Type) (es :: [Effect]).
  (HasServer api '[], IOE :> es) =>
  Int
  -> ServerT api (Eff (ErrorPage : es))
  -> Eff es ()
run port = runWarpServerSettings @api (Warp.defaultSettings & Warp.setPort port)

runTLS ::
  forall (api :: Type) (es :: [Effect]).
  (HasServer api '[], IOE :> es) =>
  TLSSettings
  -> Int
  -> ServerT api (Eff (ErrorPage : es))
  -> Eff es ()
runTLS tlsSettings port =
  runWarpTLSServerSettings @api
    tlsSettings
    (Warp.defaultSettings & Warp.setPort port)

type ErrorPage = ErrorStatic.Error ServerError

--------------------------------------------------------------------
-- Original interpretServer; has bugs when run as with "set up"   --
-- effects. Notably has a way to create WAI applications, though. --
--------------------------------------------------------------------

-- -- | Transform the Eff monad into a servant Handler.
-- interpretServer :: Env es -> Eff (ErrorPage : es) a -> Servant.Handler a
-- interpretServer env action = do
--   v <- liftIO $ do
--     es' <- cloneEnv env
--     unEff (ErrorStatic.runErrorNoCallStack action) es'
--   T.liftEither v

-- -- | Convert an effectful server into a wai application.
-- serveEff ::
--   forall (api :: Type) (context :: [Type]) (es :: [Effect]).
--   (HasServer api context, ServerContext context) =>
--   Env es
--   -> Context context
--   -> ServerT api (Eff (ErrorPage : es))
--   -> Application
-- serveEff env ctx = Servant.serveWithContextT (Proxy @api) ctx (interpretServer env)
