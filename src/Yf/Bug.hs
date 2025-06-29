{-# LANGUAGE OverloadedStrings #-}
-- |

module Yf.Bug (
  Bug(..),
  LazyProgrammerError(..),
  mosquito,
  msg,
  msg',
)
where

import Prelude hiding (bug)


newtype LazyProgrammerError = LazyProgrammer LText
  deriving (Eq, Show, Generic, Ord)

instance Exception LazyProgrammerError

mosquito :: HasCallStack => Bug
mosquito = Bug (toException $ LazyProgrammer "bit ya") callStack

msg :: (ToLText txt, HasCallStack) => txt -> Bug
msg txt = Bug (toException $ LazyProgrammer $ toLText txt) callStack

msg' :: HasCallStack => String -> Bug
msg' txt = Bug (toException $ LazyProgrammer $ toLText txt) callStack
