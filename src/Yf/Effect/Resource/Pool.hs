{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Yf.Effect.Resource.Pool where

import Prelude hiding (Reader, runReader)

import Control.Concurrent qualified
import Data.Pool qualified
import Effectful (DispatchOf, Eff, Effect, IOE, Subset, inject, liftIO, withEffToIO, (:>))
import Effectful qualified
import Effectful.Dispatch.Dynamic (
  interpret,
  localUnlift,
  send,
 )
import Effectful.Reader.Dynamic (Reader)
import Effectful.Reader.Dynamic qualified as Reader
import Effectful.Resource (Resource, allocate)
import Optics
import Text.Show hiding (show)

data Pool p :: Effect where
  Borrow :: (p -> m a) -> Pool p m a

type instance DispatchOf (Pool p) = Effectful.Dynamic

run' ::
  forall p a es.
  (IOE :> es, Resource :> es, Reader (Data.Pool.Pool p) :> es) =>
  Eff (Pool p : es) a
  -> Eff es a
run' = interpret $ \effEnv -> \case
  Borrow mkAction -> do
    pool <- Reader.ask @(Data.Pool.Pool p)
    unliftStrat <- Effectful.unliftStrategy
    (_, (handle, _localPool)) <-
      allocate
        (Data.Pool.takeResource pool)
        (\(handle, localPool) -> Data.Pool.putResource localPool handle)
    localUnlift effEnv unliftStrat $ \unlift -> unlift (mkAction handle)

withBorrowed :: (Pool p :> es) => (p -> Eff es a) -> Eff es a
withBorrowed mkAction = send $ Borrow mkAction

-- Control.Concurrent.getNumCapabilities

data PoolBuilder_0 = PoolBuilder_0
newtype PoolBuilder_1 es p = PoolBuilder_1 {create :: Eff es p}
data PoolBuilder_2 es p = PoolBuilder_2 {create :: !(Eff es p), destroy :: !(p -> Eff es ())}

data RelativeToThreadcount
  = RelExprThreadCount
  | RelExprRational Rational
  | RelExprPlus RelativeToThreadcount RelativeToThreadcount
  | RelExprMinus RelativeToThreadcount RelativeToThreadcount
  | RelExprMult RelativeToThreadcount RelativeToThreadcount
  | RelExprNegate RelativeToThreadcount
  | RelExprAbs RelativeToThreadcount
  | RelExprSignum RelativeToThreadcount
  | RelExprDiv RelativeToThreadcount RelativeToThreadcount
  | RelExprRecip RelativeToThreadcount
  deriving (Eq)
instance Num RelativeToThreadcount where
  (+) = RelExprPlus
  (-) = RelExprMinus
  (*) = RelExprMult
  negate = RelExprNegate
  abs = RelExprAbs
  signum = RelExprSignum
  fromInteger = RelExprRational . fromInteger
instance Fractional RelativeToThreadcount where
  (/) = RelExprDiv
  recip = RelExprRecip
  fromRational = RelExprRational

resolveRelativeToThreadcount :: RelativeToThreadcount -> Int -> Int
resolveRelativeToThreadcount expr threads =
  eval expr
    & ceiling
    & max 1
 where
  eval RelExprThreadCount = toEnum threads
  eval (RelExprRational n) = n
  eval (RelExprPlus a b) = eval a + eval b
  eval (RelExprMinus a b) = eval a - eval b
  eval (RelExprMult a b) = eval a * eval b
  eval (RelExprNegate a) = negate (eval a)
  eval (RelExprAbs a) = abs (eval a)
  eval (RelExprSignum a) = signum (eval a)
  eval (RelExprDiv a b) = eval a / eval b
  eval (RelExprRecip a) = recip (eval a)

instance Show RelativeToThreadcount where
  showsPrec precedence count = shows $ "RelativeToThreadcount[" <> go count precedence <> "]"
   where
    parens str = "(" <> str <> ")"
    binOp symPrec sym a b prec
      | prec > symPrec = parens $ go a symPrec <> " " <> sym <> " " <> go b symPrec
      | otherwise = go a symPrec <> " " <> sym <> " " <> go b symPrec

    go RelExprThreadCount _ = "threads"
    go (RelExprRational n) _ = show n
    go (RelExprPlus a b) prec = binOp 6 "+" a b prec
    go (RelExprMinus a b) prec = binOp 6 "-" a b prec
    go (RelExprMult a b) prec = binOp 7 "*" a b prec
    go (RelExprNegate a) prec
      | prec > 10 = parens $ "-" <> go a 10
      | otherwise = "-" <> go a 10
    go (RelExprAbs a) _ = "|" <> go a 0 <> "|"
    go (RelExprSignum a) prec
      | prec > 10 = parens $ "signfunc" <> parens (go a 0)
      | otherwise = "signfunc" <> parens (go a 0)
    go (RelExprDiv a b) prec = binOp 7 "/" a b prec
    go (RelExprRecip a) prec
      | prec > 10 = parens $ "recip" <> parens (go a 0)
      | otherwise = "recip" <> parens (go a 0)

-- validRelThreadCount :: RelThreadCount -> RelThreadCount
-- validRelThreadCount a = normalize a
-- where
--  normalizeBinOp constructor f a b = case (normalize a, normalize b) of
--    (RelExprRational x, RelExprRational y) -> constructor $ f x y
--    (a', b') -> constructor a' b'
--  normalizeUnaryOp constructor f a = case normalize a of
--    RelExprRational x -> constructor $ f x
--    a' -> constructor a
--  normalize RelExprThreadCount = RelExprThreadCount
--  normalize (RelExprRational n) = RelExprRational n
--  normalize (RelExprPlus a b) = normalizeBinOp RelExprPlus (+) a b
--  normalize (RelExprMinus a b) = normalizeBinOp RelExprMinus (-) a b
--  normalize (RelExprMult a b) = normalizeBinOp RelExprMult (*) a b
--  normalize (RelExprNegate a) = normalizeUnaryOp RelExprNegate negate a
--  normalize (RelExprAbs a) = normalizeUnaryOp RelExprAbs abs a
--  normalize (RelExprSignum a) = normalizeUnaryOp RelExprSignum signum a
--  normalize (RelExprDiv a b) = normalizeBinOp RelExprDiv (/) a b
--  normalize (RelExprRecip a) = normalizeUnaryOp RelExprRecip recip a

data PoolBuilder es p = PoolBuilder
  { create :: !(Eff es p)
  , destroy :: !(p -> Eff es ())
  , ttl :: !Double
  , max :: !RelativeToThreadcount
  , stripes :: !RelativeToThreadcount
  }
makeFieldLabelsNoPrefix ''PoolBuilder

pool :: PoolBuilder_0
pool = PoolBuilder_0

createResource :: Eff es p -> PoolBuilder_0 -> PoolBuilder_1 es p
createResource create _ = PoolBuilder_1 create

destroyResource ::
  Subset esCreate esDestroy =>
  (p -> Eff esDestroy ())
  -> PoolBuilder_1 esCreate p
  -> PoolBuilder_2 esDestroy p
destroyResource destroy PoolBuilder_1{..} = PoolBuilder_2{create = inject create, ..}

threads :: RelativeToThreadcount
threads = RelExprThreadCount

systemThreads :: RelativeToThreadcount
systemThreads = RelExprThreadCount

maxResidency :: RelativeToThreadcount -> PoolBuilder_2 es p -> PoolBuilder es p
maxResidency max PoolBuilder_2{..} = PoolBuilder{..}
 where
  stripes = threads
  ttl = 1.0 -- second

poolTtl :: Double -> PoolBuilder es p -> PoolBuilder es p
poolTtl = (#ttl .~)

stripes :: RelativeToThreadcount -> PoolBuilder es p -> PoolBuilder es p
stripes = (#stripes .~)

-- pool
--  & createResource (Eff es p)
--  & destroyResource (p -> Eff es ())
--  & maxResidency (int)
--  & gcInterval double (optional)
--  & withLogging (?)
--  & stripes (?)
--  & runner

runFromPool ::
  forall p es a.
  (IOE :> es, Resource :> es) =>
  Data.Pool.Pool p
  -> Eff (Pool p : es) a
  -> Eff es a
runFromPool pool action =
  action
    & inject @(Pool p : es) @(Pool p : Reader (Data.Pool.Pool p) : es)
    & run'
    & Reader.runReader pool

run ::
  (IOE :> es, Resource :> es, Subset poolEs es) =>
  PoolBuilder poolEs p
  -> Eff (Pool p : es) a
  -> Eff es a
run builder action = do
  let PoolBuilder{..} = builder
  capabilities <- liftIO Control.Concurrent.getNumCapabilities
  let realStripes = resolveRelativeToThreadcount stripes capabilities
  let realMax = resolveRelativeToThreadcount max capabilities
  cfg' <- withEffToIO (Effectful.ConcUnlift Effectful.Persistent Effectful.Unlimited) $ \toIO ->
    pure $
      Data.Pool.defaultPoolConfig
        (toIO $ inject create)
        (toIO . inject . destroy)
        ttl
        realMax
  let cfg = Data.Pool.setNumStripes (Just realStripes) cfg'
  (_, pp) <- allocate (Data.Pool.newPool cfg) Data.Pool.destroyAllResources
  runFromPool pp action
