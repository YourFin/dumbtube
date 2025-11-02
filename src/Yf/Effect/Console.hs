module Yf.Effect.Console where

import Relude hiding (Reader, error, runReader)

import Data.Maybe (fromJust)
import Effectful (Eff, IOE, Subset, (:>))
import Effectful qualified as Effectful
import Effectful.Environment (Environment)
import Effectful.Environment qualified as Environment
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Optics
import Options.Applicative qualified as OA
import Yf.Effect.Log (Log)
import Yf.Effect.Log qualified as Log
import Yf.Stringy (trainCase)

data WrappedCfg a = WrappedCfg
    { cfg :: !a
    , logLevel :: !(Maybe Log.Level)
    }
    deriving (Eq, Show, Generic)

parseLogLevel :: OA.Parser (Maybe Log.Level)
parseLogLevel =
    lvlFlag' (Log.Error Log.Rate) "error"
        <|> lvlFlag Log.Info
        <|> lvlFlag Log.Debug
        <|> lvlFlag Log.TraceSensitive
        <|> OA.flag' Nothing (OA.long "no-logs" <> OA.long "quiet" <> OA.help "Silence logs")
        & many
        <&> pickLast (Just $ Log.Error Log.Rate)
  where
    lvlFlag :: Log.Level -> OA.Parser (Maybe Log.Level)
    lvlFlag level = lvlFlag' level (show level & trainCase)
    lvlFlag' level flagName =
        OA.flag'
            (Just level)
            ( OA.long flagName
                <> OA.help ("Set minimum log level to " <> flagName)
            )
    pickLast :: a -> [a] -> a
    pickLast fallback = foldl' (flip const) fallback

withWrappedOptions :: OA.Parser a -> OA.Parser (WrappedCfg a)
withWrappedOptions parser = WrappedCfg <$> parser <*> parseLogLevel

defaultInfoMod :: String -> OA.InfoMod a
defaultInfoMod shortDesc = OA.progDesc shortDesc

defaultParserPrefs :: OA.ParserPrefs
defaultParserPrefs = OA.prefs $ OA.disambiguate <> OA.columns 90

type ShortDescripion = String
cliTool ::
    (HasCallStack) =>
    ShortDescripion
    -> OA.Parser cfg
    -> Eff '[Log, Reader cfg, IOE] a
    -> IO a
cliTool shortDesc cfgParser action =
    do
        WrappedCfg{logLevel, cfg} <-
            OA.customExecParser
                defaultParserPrefs
                ( OA.info
                    (withWrappedOptions cfgParser <**> OA.helper)
                    (defaultInfoMod shortDesc)
                )

        (Log.info (showLogLevel logLevel) *> action)
            & Effectful.inject
            & maybe Log.runNoop Log.runConsole logLevel
            & Reader.runReader cfg
            & Effectful.runEff
  where
    showLogLevel :: Maybe Log.Level -> String
    showLogLevel = \case
        Just level -> "Console log level set to " <> show level
        Nothing -> "Console logs disabled"

cliTool_ ::
    forall a es.
    (HasCallStack) =>
    ShortDescripion
    -> Eff '[Log, IOE] a
    -> IO a
cliTool_ desc action =
    cliTool
        desc
        (pure ())
        (Effectful.inject @[Log, IOE] @'[Log, Reader (), IOE] action)
