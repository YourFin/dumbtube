{-# LANGUAGE UndecidableInstances #-}

module Yf.Effect.Abort (
    Abort,
    AbortException,
    ToAbort (..),
    Abortable (..),
    abort,
    withMsg,
    withMsg',
    on,
    runEither,
    runFallback,
    runCli,
    runCli_,
) where

import Relude hiding (on)

import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.IO qualified as Lazy
import Df1 (unMessage)
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Labeled (Labeled, labeled, runLabeled)
import Text.Show
import Yf.Effect.Log (Message, ToMessage, message)

import System.IO qualified as IO

data AbortException
    = AbortMsg Message
    | AbortEx SomeException
instance Exception AbortException
instance Show AbortException where
    showsPrec prec abortException =
        showParen (prec > app_prec) $
            ( showString "AbortException: " . case abortException of
                AbortMsg msg -> showString $ Lazy.unpack (unMessage msg)
                AbortEx ex -> showsPrec (app_prec + 1) ex
            )
      where
        app_prec = 10

class ToAbort e where
    toAbortException :: e -> AbortException

instance ToAbort Message where
    toAbortException = AbortMsg

instance {-# OVERLAPS #-} (Exception ex) => ToAbort ex where
    toAbortException = AbortEx . toException

class Abortable e where
    liftAbort :: (HasCallStack, Abort :> es) => e a -> Eff es a

instance (ToAbort ex) => Abortable (Either ex) where
    liftAbort = either abort pure

-- instance (ToAbort ex) => Abortable (Eff '[Error ex]) where
--  liftAbort =

type AbortLabel = "YF_EFFECT_ABORT"
type Abort = Labeled AbortLabel (Error AbortException)

withMsg :: (HasCallStack, Abort :> es, ToMessage m) => m -> Eff es a
withMsg msg = abort (message msg)

withMsg' :: (HasCallStack, Abort :> es) => String -> Eff es a
withMsg' msg = withMsg msg

abort :: (HasCallStack, ToAbort ex, Abort :> es) => ex -> Eff es a
abort ex =
    toAbortException ex
        & Error.throwError
        & labeled @AbortLabel @(Error AbortException)

-- \$ Error.throwError $ toAbortException ex

{- | "handle" an Error effect by aborting when it is encountered.
Intended use is with a qualified import; i.e.:
> import Yf.Effect.Abort qualified as Abort
> import Yf.Effect.Abort (Abort)
>
> hasError :: (Error MyEx :> es) => Eff es Int
> hasError = _
>
> Abort.on @MyEx hasError
-}
on :: forall ex a es. (Abort :> es, Exception ex) => Eff (Error ex : es) a -> Eff es a
on =
    Error.runErrorWith
        (\stack ex -> abort $ Bug (toException ex) stack)

runCli :: (IOE :> es) => Bool -> Eff (Abort : es) a -> Eff es a
runCli shouldShowCallStack =
    runLabeled @AbortLabel $ \unlabeledAction ->
        flip Error.runErrorWith unlabeledAction $ \callstack abortException -> do
            stderrAbort abortException
            when shouldShowCallStack $ liftIO $ do
                IO.hPutStrLn stderr ""
                IO.hPutStrLn stderr "Call Stack:"
                IO.hPutStrLn stderr $ prettyCallStack callstack
            exitFailure
  where
    stderrAbort =
        liftIO . \case
            AbortMsg msg -> Lazy.hPutStrLn stderr (unMessage msg)
            AbortEx ex -> IO.hPrint stderr ex

runCli_ :: (IOE :> es) => Eff (Abort : es) a -> Eff es a
runCli_ = runCli True

runEither :: Eff (Abort : es) a -> Eff es (Either Bug a)
runEither = runLabeled @AbortLabel $ \unlabeled ->
    Error.runError unlabeled
        <&> first (\(callstack, abortException) -> Bug (toException abortException) callstack)

runFallback :: a -> Eff (Abort : es) a -> Eff es a
runFallback val action = runEither action <&> fromRight val
