module Yf.Effect.Log.Level where

import qualified Df1 as Df1

data Level
  -- Sensitive Debug info; should not be exposed outside development
  = TraceSensitive
  -- Debug info
  | Debug
  -- Info on what the application is up to
  | Info
  -- Error; should page at @Rate@
  | Error Rate
  deriving (Eq, Show)

instance Ord Level where
  compare TraceSensitive TraceSensitive = EQ
  compare Debug Debug = EQ
  compare Info Info = EQ
  compare (Error x) (Error y) = compare x y
  compare TraceSensitive _ = LT
  compare _ TraceSensitive = GT
  compare Debug _ = LT
  compare _ Debug = GT
  compare Info _ = LT
  compare _ Info = GT

-- TODO wire up automatic alarming based on rates
data Rate = Rate
  deriving (Eq, Show)

instance Ord Rate where
  compare Rate Rate = EQ

toDf1 :: Level -> Df1.Level
toDf1 TraceSensitive = Df1.Debug
toDf1 Debug = Df1.Debug
toDf1 Info = Df1.Info
toDf1 (Error _) = Df1.Error
