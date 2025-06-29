{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Yf.Effect.DB.Transaction
  ( Transaction
  , TransactionAbortReason(..)
  -- * Aborting the current transaction
  , abort
  , abortEx
  , abort_
  , abortOn
  -- * Creating transactions
  , create
  , Handler(..)
  ) where

import Effectful (Eff, IOE, (:>))
import Effectful qualified as Effectful
import Effectful.Error.Dynamic (throwError)
import Effectful.Error.Dynamic qualified as Error
import UnliftIO.Exception (mask, withException)

data TransactionAbortReason
    = TransactionAbortException SomeException
    | TransactionAbortString String
    deriving (Show, Generic)

type Transaction = Error.Error (Maybe TransactionAbortReason)

data Handler es handle txnResult a = Handler
    { init :: Eff es handle
    , commit :: handle -> txnResult -> Eff es a
    , rollback :: handle -> Maybe CallStack -> Maybe TransactionAbortReason -> Eff es a
    }
    deriving (Generic)

-- | Create a transaction around a given 'Eff' action,
-- given an 'init', 'commit', and 'rollback' function.
--
-- Intended for use in defining a database effect.
create :: (IOE :> es) => Handler es handle r a -> Eff (Transaction : es) r -> Eff es a
create handler actionUnderTxn = mask $ \restore -> do
    unliftStrat <- Effectful.unliftStrategy
    txnHandle <- handler.init
    eitherR <-
        restore (Error.runError actionUnderTxn)
            & flip
                (withException @_ @SomeException)
                ( \ex ->
                    toException ex
                        & TransactionAbortException
                        & Just
                        & handler.rollback txnHandle Nothing
                )
    case eitherR of
        Left (callstack, reason) -> handler.rollback txnHandle (Just callstack) reason
        Right r -> do
            handler.commit txnHandle r


-- | Abort the current transaction with a message
abort :: (Transaction :> es, HasCallStack) => String -> Eff es a
abort = throwError . Just . TransactionAbortString

-- | Abort the current transaction due to an 'Exception'
abortEx :: (Transaction :> es, Exception e, HasCallStack) => e -> Eff es a
abortEx = throwError . Just . TransactionAbortException . toException

-- | Abort the current transaction with no message
abort_ :: (Transaction :> es, HasCallStack) => Eff es a
abort_ = throwError @(Maybe TransactionAbortReason) Nothing

-- | Handle an 'Error' by aborting
abortOn :: (Transaction :> es, Exception e, HasCallStack) => Eff (Error.Error e : es) a -> Eff es a
abortOn = Error.runErrorNoCallStackWith abortEx

