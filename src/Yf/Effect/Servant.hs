module Yf.Effect.Servant where

import Control.Monad.Except qualified as T
import Data.Kind (Type)
import Effectful (Eff)
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive (Env, cloneEnv)
import Effectful.Error.Static qualified as ErrorStatic
import Network.Wai.Handler.Warp qualified as Warp
import Servant hiding ((:>))

-- Lots of the code here sourced from
-- https://github.com/Kleidukos/servant-effectful/blob/22af09642078d5296b524495ad8213bf2ace62d2/src/Effectful/Servant.hs

-- | Transform the Eff monad into a servant Handler.
interpretServer :: Env es -> Eff (ErrorPage : es) a -> Servant.Handler a
interpretServer env action = do
  v <- liftIO $ do
    es' <- cloneEnv env
    unEff (ErrorStatic.runErrorNoCallStack action) es'
  T.liftEither v

type ErrorPage = ErrorStatic.Error ServerError
