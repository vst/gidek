{-# LANGUAGE OverloadedStrings #-}

-- | This module provides top-level definitions for the CLI program.
module Gidek.Cli where

import Control.Applicative ((<**>), (<|>))
import qualified Data.Text as T
import qualified Gidek.Lib.Programs as Programs
import qualified Options.Applicative as OA
import System.Exit (ExitCode, exitWith)
import qualified Zamazingo.Meta as Z.Meta


-- * Entrypoint


-- | CLI program entrypoint.
cli :: IO ()
cli =
  runCliOptions =<< OA.customExecParser pref info
  where
    pref = OA.prefs (OA.showHelpOnError <> OA.helpLongEquals <> OA.helpShowGlobals)
    pars = optCliOptions <**> infoOptVersion <**> OA.helper
    desc = OA.fullDesc <> OA.progDesc "Top Level Commands" <> infoModHeader <> infoModFooter
    info = OA.info pars desc


-- * Program


-- | CLI options definition.
data CliOptions = CliOptions
  { cliOptionsConfigFile :: !FilePath
  , cliOptionsCommand :: !(FilePath -> IO ExitCode)
  }


-- | Option parser for 'CliOptions'.
optCliOptions :: OA.Parser CliOptions
optCliOptions =
  CliOptions
    <$> OA.strOption (OA.short 'c' <> OA.long "config" <> OA.metavar "CONFIG-FILE" <> OA.help "Path to configuration file")
    <*> (commandPlan <|> commandBackup)


-- | Runs 'CliOptions'.
runCliOptions :: CliOptions -> IO ()
runCliOptions (CliOptions o s) = s o >>= exitWith


-- * Commands


-- ** plan


-- | Definition for @plan@ CLI command.
commandPlan :: OA.Parser (FilePath -> IO ExitCode)
commandPlan = OA.hsubparser (OA.command "plan" (OA.info parser infomod) <> OA.metavar "plan")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Show backup plan" <> OA.footer "This command tabulates the backup plan."
    parser = pure doPlan


-- | @plan@ CLI command program.
doPlan :: FilePath -> IO ExitCode
doPlan =
  Programs.runProgramWithConfigFile Programs.planAndPrint


-- ** backup


-- | Definition for @backup@ CLI command.
commandBackup :: OA.Parser (FilePath -> IO ExitCode)
commandBackup = OA.hsubparser (OA.command "backup" (OA.info parser infomod) <> OA.metavar "backup")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Run backups" <> OA.footer "This command runs backup procedure."
    parser = pure doBackup


-- | @backup@ CLI command program.
doBackup :: FilePath -> IO ExitCode
doBackup =
  Programs.runProgramWithConfigFile Programs.backup


-- * Helpers


-- | Version option parser.
infoOptVersion :: OA.Parser (a -> a)
infoOptVersion =
  OA.infoOption Z.Meta.versionString $
    OA.short 'v'
      <> OA.long "version"
      <> OA.help "Show application version and exit"


-- | Header 'OA.InfoMod'.
infoModHeader :: OA.InfoMod a
infoModHeader =
  OA.header (T.unpack (Z.Meta.name <> " - " <> Z.Meta.title <> " v" <> Z.Meta.versionText))


-- | Footer 'OA.InfoMod'.
infoModFooter :: OA.InfoMod a
infoModFooter =
  OA.footer "See <https://github.com/vst/gidek> for help and feedback."


-- | Tests a parser with given arguments.
runParserTest :: OA.Parser a -> [String] -> OA.ParserResult a
runParserTest parser = OA.execParserPure (OA.prefs prefs) (OA.info (parser <**> OA.helper) infomod)
  where
    prefs = OA.showHelpOnError <> OA.helpLongEquals <> OA.helpShowGlobals
    infomod = OA.fullDesc <> OA.progDesc "Test Parser" <> OA.header "testparser - especially for doctests"


-- | Tests an IO parser with given arguments.
runParserTestIO :: OA.Parser (IO a) -> [String] -> IO (Either String ())
runParserTestIO p as = case runParserTest p as of
  OA.Success _ -> pure (Right ())
  OA.Failure f -> pure (Left (show f))
  OA.CompletionInvoked _ -> pure (Right ())
