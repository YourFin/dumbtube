module Yf.Effect.Sqlite (
  module ReExport,
) where

import Database.SQLite.Simple qualified as ReExport (
  ColumnIndex (..),
  Connection (..),
  Error (..),
  FormatError (..),
  FromRow (..),
  NamedParam (..),
  Only (..),
  Query (..),
  ResultError (..),
  SQLData (..),
  SQLError (..),
  ToRow (..),
  field,
  (:.) (..),
 )

import Prelude hiding (Reader, runReader)

import Yf.Effect.DB.Transaction (Transaction)
import Yf.Effect.DB.Transaction qualified as Txn
import Yf.Effect.Log (Log)
import Yf.Effect.Log qualified as Log

import Effectful (DispatchOf, Eff, Effect, IOE, withEffToIO, (:>))
import Effectful qualified as Effectful
import Effectful.Dispatch.Dynamic (interpret, localUnliftIO, reinterpret)
import Effectful.Error.Static (throwError)
import Effectful.Error.Static qualified as Effectful.Error
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static qualified as Reader
import Effectful.TH (makeEffect)

import Database.SQLite.Simple qualified as Smpl
import Database.SQLite.Simple.Internal qualified as SmplInternals

type HasError = Effectful.Error.Error Smpl.Error -- TODO: correct error type

-- runMemory (IOE :> es) :: Eff (Sqlite : es) a -> Eff es a
-- runFile (IOE :> es) :: FilePath -> Eff (Sqlite : es) a -> Eff es a

data Sqlite :: Effect where
  NextRow :: Smpl.FromRow a => Smpl.Statement -> Sqlite m (Maybe a)
  SetTrace :: Maybe (Text -> m ()) -> Sqlite m ()
  BindNamed :: Smpl.Statement -> [Smpl.NamedParam] -> Sqlite m ()
  Bind :: Smpl.ToRow params => Smpl.Statement -> params -> Sqlite m ()
  Reset :: Smpl.Statement -> Sqlite m ()

type instance DispatchOf Sqlite = Effectful.Dynamic

run' ::
  (IOE :> es, Reader Smpl.Connection :> es) =>
  Eff (Sqlite : es) a
  -> Eff es a
run' = interpret $ \effEnv -> \case
  NextRow statement ->
    liftIO $ Smpl.nextRow statement
  SetTrace mLogger -> do
    unliftStrat <- Effectful.unliftStrategy
    conn <- Reader.ask @Smpl.Connection
    localUnliftIO effEnv unliftStrat $ \unliftIO -> do
      liftIO $ Smpl.setTrace conn (fmap (\log -> \text -> unliftIO $ log text) mLogger)
  BindNamed statement params ->
    liftIO $ Smpl.bindNamed statement params
  Bind statement params ->
    liftIO $ Smpl.bind statement params
  Reset statement ->
    liftIO $ Smpl.reset statement

run ::
  (IOE :> es) =>
  Smpl.Connection
  -> Eff (Sqlite : es) a
  -> Eff es a
run conn action =
  action
    & Effectful.inject
    & run'
    & Reader.runReader conn

-- withTransaction' :: (Sqlite :> es) :: Eff (Txn : es) a -> Eff es (Either Error a)
-- catchAbort :: forall e a es. (Effectful.Error.Error e :> es) =>
