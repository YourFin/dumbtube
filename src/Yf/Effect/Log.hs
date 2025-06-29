module Yf.Effect.Log (
  Log,

  -- Re-exports from other Log libs
  Level(..),
  Rate(..),

  -- Re-exports from DF1
  Path,
  Segment,
  ToSegment,
  Key,
  ToKey,
  Value,
  ToValue,
  Message,
  ToMessage,
  segment,
  key,
  value,
  message,

  -- Core effects
  attr_,
  push_,
  filter,
  logWithLevel,
  flush,
  askLogFn,

  -- Helpers

  attr,
  push,
  traceSensitive,
  debug,
  info,
  error,
  traceSensitive_,
  debug_,
  info_,
  error_,

  -- Runners
  runDiIO,
  runConsole,
  runConsole_,
  runNoop,
) where

import Prelude hiding (runReader, Reader, filter, error)
import Yf.Effect.Log.Level (Level(..), Rate(..))
import qualified Yf.Effect.Log.Level as Level

import Effectful (Effect, Eff, IOE, (:>), UnliftStrategy(ConcUnlift), Persistence(Persistent), Limit(Unlimited))
import qualified Effectful as Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret, localUnlift, localSeqUnlift)
import Effectful.TH (makeEffect)

-- TODO: try swapping out for static
import Effectful.Reader.Dynamic (Reader(), runReader)
import qualified Effectful.Reader.Dynamic as Reader
import Di.Core (Di, log_level)
import qualified Di.Core as Di
import qualified Di.Handle as DiHandle
import qualified Di.Df1 as DiDf1
import Df1 (Path, Segment, ToSegment, Key, ToKey, Value, ToValue, Message, ToMessage, segment, key, value, message)
import Data.Sequence (Seq)

data Log :: Effect where
  Attr_ :: Key -> Value -> m a -> Log m a
  Push_ :: Segment -> m a -> Log m a
  Filter :: (Level -> Seq Path -> Message -> Bool) -> m a -> Log m a
  AskDi :: Log m (Di Level Path Message)
  LogWithLevel :: Level -> Message -> Log m ()
  Flush :: Log m ()
  AskLogFn :: (MonadIO w) => Log m (Level -> Message -> w ())
makeEffect ''Log

attr :: (ToValue v, Log :> es) => Key -> v -> Eff es a -> Eff es a
attr key val action = attr_ key (Df1.value val) action

push :: (ToSegment s, Log :> es) => s -> Eff es a -> Eff es a
push s action = push_ (segment s) action

traceSensitive :: (ToMessage msg, Log :> es) => msg -> Eff es ()
traceSensitive msg = logWithLevel TraceSensitive (message msg)

debug :: (ToMessage msg, Log :> es) => msg -> Eff es ()
debug msg = logWithLevel Debug (message msg)

info :: (ToMessage msg, Log :> es) => msg -> Eff es ()
info msg = logWithLevel Info (message msg)

error :: (ToMessage msg, Log :> es) => Rate -> msg -> Eff es ()
error rate msg = logWithLevel (Error rate) (message msg)

traceSensitive_ :: Log :> es => Message -> Eff es ()
traceSensitive_ msg = logWithLevel TraceSensitive (message msg)

debug_ :: Log :> es => Message -> Eff es ()
debug_ msg = logWithLevel Debug (message msg)

info_ :: Log :> es => Message -> Eff es ()
info_ msg = logWithLevel Info (message msg)

error_ :: Log :> es => Rate -> Message -> Eff es ()
error_ rate msg = logWithLevel (Error rate) (message msg)

runDiIO :: (IOE :> es) => Di Level Path Message -> Eff (Log : es) a -> Eff es a
runDiIO di =
  reinterpret (runReader di) $ \env -> \case
    Attr_ kk value action -> adaptDf1 (DiDf1.attr_ kk value) action env
    Push_ segment action -> adaptDf1 (DiDf1.push segment) action env
    Filter pred action -> adaptDf1 (Di.filter pred) action env
    LogWithLevel level msg -> do
      di' <- Reader.ask @(Di Level Path Message)
      Di.log di' level msg
    Flush -> do
      di' <- Reader.ask @(Di Level Path Message)
      Di.flush di'
    AskLogFn -> do
      di' <- Reader.ask @(Di Level Path Message)
      pure (Di.log di')
  where
    adaptDf1 modifyDf1 action env = do
      unliftStrat <- Effectful.unliftStrategy
      localUnlift env unliftStrat $ \unlift ->
        Reader.local @(Di Level Path Message) modifyDf1 $ unlift action

runConsole :: (IOE :> es) => Level -> Eff (Log : es) a -> Eff es a
runConsole level action = do
  writeLog <- DiHandle.stderr lineRenderer
  Di.new writeLog (\di -> runDiIO di $ do
                      val <- filteredAction
                      flush
                      pure val)
  where
    filteredAction = filter (\msgLevel _ _ -> msgLevel >= level) action

runConsole_ :: (IOE :> es) => Eff (Log : es) a -> Eff es a
runConsole_ = runConsole (Level.Error Level.Rate)

runNoop :: Eff (Log : es) a -> Eff es a
runNoop =
  interpret $ \env -> \case
    Attr_ _ _ action -> adapt action env
    Push_ _ action -> adapt action env
    Filter _ action -> adapt action env
    LogWithLevel _ _ -> pure ()
    Flush -> pure ()
    AskLogFn -> pure (\_ _ -> pure ())
  where
    adapt action env = localSeqUnlift env (\unlift ->
      unlift action)

-- TODO: systemd journal
-- https://github.com/ocharles/libsystemd-journal

lineRenderer :: DiHandle.LineRenderer Level Path Message
lineRenderer = DiHandle.LineRendererUtf8 render
  where
    renderDf1 = case DiDf1.df1 of
      DiHandle.LineRendererUtf8 renderDf1' -> renderDf1'
    render supportsColors log =
      renderDf1 supportsColors log{
        log_level =
          log
          & log_level
          & Level.toDf1
        }
