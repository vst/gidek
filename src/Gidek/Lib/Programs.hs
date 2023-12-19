{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides programs which this application is built on.
module Gidek.Lib.Programs where

import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time as Time
import qualified Gidek.Lib.Config as Config
import qualified Gidek.Lib.Git as Git
import qualified Gidek.Lib.Github as Github
import qualified Path as P
import qualified Path.IO as PIO
import System.Exit (ExitCode (..))
import System.IO (stderr)
import qualified Text.Layout.Table as Table
import qualified Zamazingo.Text as Z.Text


-- * Runners


-- | Type definition for our programs.
type Program a = ReaderT Env (ExceptT T.Text IO) a


-- | Runs the given program with the given environment and returns
-- 'ExitCode' in IO monad.
runProgram
  :: Program a
  -> Env
  -> IO ExitCode
runProgram prog env =
  runExceptT (runReaderT prog env) >>= either _onFail _onSucc
  where
    _onFail = TIO.hPutStrLn stderr >=> const (pure (ExitFailure 1))
    _onSucc = const (pure ExitSuccess)


-- | Reads the configuration from the given configuration file, builds
-- an environment, runs the given program with this environment and
-- returns 'ExitCode' in IO monad.
runProgramWithConfigFile
  :: Program a
  -> FilePath
  -> IO ExitCode
runProgramWithConfigFile prog fp = do
  env <- runExceptT (buildEnvFromConfigFile fp)
  case env of
    Left err -> TIO.hPutStrLn stderr err >> pure (ExitFailure 1)
    Right se -> runProgram prog se


-- * Environment


-- | Data definition for the environment of our 'Program'.
data Env = Env
  { envStore :: !(P.Path P.Abs P.Dir)
  , envToken :: !T.Text
  , envRepoSources :: ![Github.GithubRepoSource]
  }


-- | Attempts to build and return environment from the given
-- configuration.
buildEnvFromConfig
  :: MonadIO m
  => MonadError T.Text m
  => Config.Config
  -> m Env
buildEnvFromConfig Config.Config {..} = do
  token <- maybe (maybe errMissing readToken configTokenFile) pure configToken
  pure $ Env {envStore = configStore, envToken = token, envRepoSources = configRepos}
  where
    readToken = liftIO . TIO.readFile . P.toFilePath
    errMissing = throwError "Either \"token\" or \"token_file\" must be specified in the configuration file."


-- | Attempts to build and return environment from the given
-- configuration file.
buildEnvFromConfigFile
  :: MonadIO m
  => MonadError T.Text m
  => FilePath
  -> m Env
buildEnvFromConfigFile fp =
  PIO.resolveFile' fp >>= Config.readConfigFile >>= buildEnvFromConfig


-- * Plan


-- | Type definition for a plan.
type Plan = (Github.GithubRepo, P.Path P.Abs P.Dir, Bool)


-- | Attempts to plan what to expect if the application runs backups
-- for a given configuration.
plan :: Program [Plan]
plan = do
  token <- asks envToken
  sources <- asks envRepoSources
  Github.listRepositories token sources >>= mapM planRepository


-- | Attempts to plan what to expect if the application runs backups
-- for a given configuration and a single repository.
planRepository :: Github.GithubRepo -> Program Plan
planRepository repo = do
  dir <- getRepositoryDirectory repo
  exists <- PIO.doesDirExist dir
  pure (repo, dir, exists)


-- | Attempts to plan what to expect if the application runs backups
-- for a given configuration and prettyprints plans in a table format.
planAndPrint :: Program ()
planAndPrint =
  plan >>= printPlans


-- | Prettyprints plans in a table format
printPlans
  :: MonadIO m
  => [Plan]
  -> m ()
printPlans = do
  liftIO . putStrLn . table
  where
    tableHeader =
      [ "ID" :: String
      , "Name"
      , "New?"
      , "Path"
      ]
    tableCols =
      [ Table.column Table.expand Table.left Table.noAlign Table.noCutMark -- ID
      , Table.column Table.expand Table.left Table.noAlign Table.noCutMark -- Name
      , Table.column Table.expand Table.center Table.noAlign Table.noCutMark -- New?
      , Table.column Table.expand Table.left Table.noAlign Table.noCutMark -- Path
      ]
    tableRows = fmap toRow
    toRow (Github.GithubRepo {..}, path, exists) =
      Table.rowG
        [ T.unpack githubRepoId
        , [i|#{githubRepoOwner}/#{githubRepoName}|]
        , if exists then "exists" else "new"
        , P.toFilePath path
        ]
    table ps = Table.tableString (Table.columnHeaderTableS tableCols Table.unicodeS (Table.titlesH tableHeader) (tableRows ps))


-- * Backup


-- | Performs backup procedure for all repositories of interest as per
-- given configuration.
backup :: Program ()
backup = do
  token <- asks envToken
  sources <- asks envRepoSources
  repos <- Github.listRepositories token sources
  _log "Starting backup process..."
  mapM_ backupRepository repos
  _log "Finished backup process."


-- | Runs backup procedure on a single repository.
backupRepository :: Github.GithubRepo -> Program ()
backupRepository repo@Github.GithubRepo {..} = do
  token <- asks envToken
  _log [i|Cloning #{githubRepoOwner}/#{githubRepoName}|]
  dir <- getRepositoryBackupDirectory repo
  metadataFile <- getRepositoryMetadataFile repo
  PIO.ensureDir (P.parent metadataFile)
  liftIO (BL.writeFile (P.toFilePath metadataFile) (Aeson.encode repo))
  Git.backup (_buildUri token githubRepoUrl) dir


-- | Builds URI by injecting GitHub token into the HTTPS uri.
_buildUri
  :: T.Text
  -> T.Text
  -> T.Text
_buildUri tok url =
  "https://" <> tok <> "@" <> T.drop (T.length "https://") url


-- * Logging


-- | Prints a log message.
_log
  :: MonadIO m
  => T.Text
  -> m ()
_log msg = do
  now <- liftIO Time.getCurrentTime
  liftIO (TIO.putStrLn [i|[#{Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%z" now}] #{msg}|])


-- * Directories


-- | Returns the repository directory for a given repository.
getRepositoryDirectory :: Github.GithubRepo -> Program (P.Path P.Abs P.Dir)
getRepositoryDirectory Github.GithubRepo {..} = do
  store <- asks envStore
  (\x -> store P.</> $(P.mkRelDir "backups") P.</> x) <$> reldir
  where
    reldir = either (throwError . ("Invalid repository directory path segment: " <>) . Z.Text.tshow) pure (P.parseRelDir (T.unpack githubRepoId))


-- | Returns the repository backup directory for a given repository.
getRepositoryBackupDirectory :: Github.GithubRepo -> Program (P.Path P.Abs P.Dir)
getRepositoryBackupDirectory repo =
  (P.</> $(P.mkRelDir "data")) <$> getRepositoryDirectory repo


-- | Returns the repository metadata file path for a given repository.
getRepositoryMetadataFile :: Github.GithubRepo -> Program (P.Path P.Abs P.File)
getRepositoryMetadataFile repo =
  (P.</> $(P.mkRelFile "metadata.json")) <$> getRepositoryDirectory repo
