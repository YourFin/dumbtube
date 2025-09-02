module Yf.Effect.Sqlite (
  module ReExport,
  Sqlite,
  runStatic',
  runPool',
  runPool,
  pool,
  runEasy,
  runEasyMemory,
  query,
  query_,
  execute,
  execute_,
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
  Statement,
  ToRow (..),
  field,
  (:.) (..),
 )

import Prelude hiding (Reader, runReader)

import Yf.Effect.DB.Transaction (Transaction)
import Yf.Effect.DB.Transaction qualified as Txn
import Yf.Effect.Log (Log)
import Yf.Effect.Log qualified as Log
import Yf.Effect.Resource.Pool (Pool)
import Yf.Effect.Resource.Pool qualified as Pool

import Effectful (DispatchOf, Eff, Effect, IOE, withEffToIO, (:>))
import Effectful qualified
import Effectful.Dispatch.Dynamic (
  interpret,
  localUnlift,
  localUnliftIO,
  reinterpret,
  send,
 )
import Effectful.Error.Static (throwError)
import Effectful.Error.Static qualified as Effectful.Error
import Effectful.Exception (bracket)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static qualified as Reader
import Effectful.TH (makeEffect)
import Streaming hiding (run)
import Streaming.Prelude qualified as S

import Database.SQLite.Simple qualified as Smpl
import Database.SQLite.Simple.Internal qualified as SmplInternal
import Database.SQLite3 qualified as Direct

type HasError = Effectful.Error.Error Smpl.Error -- TODO: correct error type

-- runMemory (IOE :> es) :: Eff (Sqlite : es) a -> Eff es a
-- runFile (IOE :> es) :: FilePath -> Eff (Sqlite : es) a -> Eff es a

data Sqlite :: Effect where
  WithStatement :: Smpl.Query -> (Smpl.Statement -> m a) -> Sqlite m a
  NextRow :: forall r m. Smpl.FromRow r => Smpl.Statement -> Sqlite m (Maybe r)
  SetTrace :: Maybe (Text -> m ()) -> Sqlite m ()
  BindNamed :: Smpl.Statement -> [Smpl.NamedParam] -> Sqlite m ()
  Bind :: Smpl.ToRow params => Smpl.Statement -> params -> Sqlite m ()
  Reset :: Smpl.Statement -> Sqlite m ()
  Step :: Smpl.Statement -> Sqlite m Direct.StepResult

type instance DispatchOf Sqlite = Effectful.Dynamic

runStatic' ::
  (IOE :> es, Reader Smpl.Connection :> es) =>
  Eff (Sqlite : es) a
  -> Eff es a
runStatic' = interpret $ \effEnv -> \case
  WithStatement query action -> do
    conn <- Reader.ask @Smpl.Connection
    unliftStrat <- Effectful.unliftStrategy
    bracket
      (liftIO $ Smpl.openStatement conn query)
      (liftIO . Smpl.closeStatement)
      (\stmt -> localUnlift effEnv unliftStrat $ \unlift -> unlift (action stmt))
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
  Step (Smpl.Statement statement) ->
    liftIO $ Direct.step statement

-- TODO: Support tracing
runPool' ::
  (IOE :> es, Pool Smpl.Connection :> es) =>
  Eff (Sqlite : es) a
  -> Eff es a
runPool' = interpret $ \effEnv -> \case
  WithStatement query action -> Pool.withBorrowed $ \conn -> do
    unliftStrat <- Effectful.unliftStrategy
    bracket
      (liftIO $ Smpl.openStatement conn query)
      (liftIO . Smpl.closeStatement)
      (\stmt -> localUnlift effEnv unliftStrat $ \unlift -> unlift (action stmt))
  NextRow statement ->
    liftIO $ Smpl.nextRow statement
  SetTrace mLogger -> pure () -- TODO: Support tracing
  BindNamed statement params ->
    liftIO $ Smpl.bindNamed statement params
  Bind statement params ->
    liftIO $ Smpl.bind statement params
  Reset statement ->
    liftIO $ Smpl.reset statement
  Step (Smpl.Statement statement) ->
    liftIO $ Direct.step statement

pool :: (IOE :> es) => String -> Pool.PoolBuilder_2 es Smpl.Connection
pool db =
  Pool.pool
    & Pool.createResource (liftIO $ Smpl.open db)
    & Pool.destroyResource (liftIO . Smpl.close)

runConn ::
  (IOE :> es) =>
  Smpl.Connection
  -> Eff (Sqlite : es) a
  -> Eff es a
runConn conn action =
  action
    & Effectful.inject
    & runStatic'
    & Reader.runReader conn

-- Run with a connection to the database
-- located at `db`. Allows early release
-- of SQLite connection
--
-- Note: Might be worth re-implementing with
-- Text in terms of the sqlite primitives
runEasy ::
  (IOE :> es) =>
  String
  -> Eff (Sqlite : es) a
  -> Eff es a
runEasy db action =
  bracket (liftIO $ Smpl.open db) (liftIO . Smpl.close) (\conn -> runConn conn action)

runEasyMemory ::
  (IOE :> es) =>
  Eff (Sqlite : es) a
  -> Eff es a
runEasyMemory = runEasy ":memory:"

runPool ::
  (IOE :> es) =>
  Pool.PoolBuilder es Smpl.Connection
  -> Eff (Sqlite : es) a
  -> Eff es a
runPool builder action =
  action
    & Effectful.inject
    & runPool'
    & Pool.run builder

withStatement :: (Sqlite :> es) => Smpl.Query -> (Smpl.Statement -> Eff es a) -> Eff es a
withStatement q useStmt = send (WithStatement q useStmt)

---- Re-implemenatation of niceities
query ::
  (Smpl.FromRow r, Smpl.ToRow q, Sqlite :> es) =>
  Smpl.Query
  -> q
  -> (Stream (Of r) (Eff es) () -> Eff es a)
  -> Eff es a
query q params consumeStream = withStatement q $ \stmt -> do
  send (Bind stmt params)
  consumeStream (S.reread (send . NextRow) stmt)

query_ ::
  (Smpl.FromRow r, Sqlite :> es) =>
  Smpl.Query
  -> (Stream (Of r) (Eff es) () -> Eff es a)
  -> Eff es a
query_ q consumeStream = withStatement q $ \stmt -> do
  consumeStream (S.reread (send . NextRow) stmt)

execute :: (Smpl.ToRow q, Sqlite :> es) => Smpl.Query -> q -> Eff es ()
execute q params = withStatement q $ \stmt -> do
  send (Bind stmt params)
  void $ send (Step stmt)

execute_ :: Sqlite :> es => Smpl.Query -> Eff es ()
execute_ q = withStatement q $ \stmt -> do
  void $ send (Step stmt)

-- withTransaction' :: (Sqlite :> es) :: Eff (Txn : es) a -> Eff es (Either Error a)
-- catchAbort :: forall e a es. (Effectful.Error.Error e :> es) =>
