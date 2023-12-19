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


-- | Runs an action and returns 'ExitCode' in IO monad.
runProgram
  :: ExceptT T.Text IO a
  -> IO ExitCode
runProgram act =
  runExceptT act >>= either _onFail _onSucc
  where
    _onFail = TIO.hPutStrLn stderr >=> const (pure (ExitFailure 1))
    _onSucc = const (pure ExitSuccess)


-- | Reads the configuration file, runs an action with this
-- configuration file and returns 'ExitCode' in IO monad.
runProgramWithConfigFile
  :: (Config.Config -> ExceptT T.Text IO a)
  -> FilePath
  -> IO ExitCode
runProgramWithConfigFile act =
  runProgram . withConfigFromFile act


-- | Reads the configuration file, runs the action with this
-- configuration file and returns its result.
withConfigFromFile
  :: MonadIO m
  => MonadError T.Text m
  => (Config.Config -> m a)
  -> FilePath
  -> m a
withConfigFromFile act fp = do
  PIO.resolveFile' fp >>= Config.readConfigFile >>= act


-- * Plan


-- | Type definition for a plan.
type Plan = (Github.GithubRepo, P.Path P.Abs P.Dir, Bool)


-- | Attempts to plan what to expect if the application runs backups
-- for a given configuration.
plan
  :: MonadIO m
  => MonadError T.Text m
  => Config.Config
  -> m [Plan]
plan cfg@Config.Config {..} =
  Github.listRepositories configToken configRepos >>= mapM (planRepository cfg)


-- | Attempts to plan what to expect if the application runs backups
-- for a given configuration and a single repository.
planRepository
  :: MonadIO m
  => MonadError T.Text m
  => Config.Config
  -> Github.GithubRepo
  -> m Plan
planRepository cfg repo = do
  dir <- getRepositoryDirectory cfg repo
  exists <- PIO.doesDirExist dir
  pure (repo, dir, exists)


-- | Attempts to plan what to expect if the application runs backups
-- for a given configuration and prettyprints plans in a table format.
planAndPrint
  :: MonadIO m
  => MonadError T.Text m
  => Config.Config
  -> m ()
planAndPrint =
  plan >=> printPlans


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
backup
  :: MonadIO m
  => MonadError T.Text m
  => Config.Config
  -> m ()
backup cfg@Config.Config {..} = do
  repos <- Github.listRepositories configToken configRepos
  _log "Starting backup process..."
  mapM_ (backupRepository cfg) repos
  _log "Finished backup process."


-- | Runs backup procedure on a single repository.
backupRepository
  :: MonadIO m
  => MonadError T.Text m
  => Config.Config
  -> Github.GithubRepo
  -> m ()
backupRepository cfg@Config.Config {..} repo@Github.GithubRepo {..} = do
  _log [i|Cloning #{githubRepoOwner}/#{githubRepoName}|]
  dir <- getRepositoryBackupDirectory cfg repo
  metadataFile <- getRepositoryMetadataFile cfg repo
  PIO.ensureDir (P.parent metadataFile)
  liftIO (BL.writeFile (P.toFilePath metadataFile) (Aeson.encode repo))
  Git.backup (_buildUri configToken githubRepoUrl) dir


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


-- | Returns the logs directory for a given configuration.
getLogsDirectory
  :: Config.Config
  -> P.Path P.Abs P.Dir
getLogsDirectory Config.Config {..} =
  configStore P.</> $(P.mkRelDir "logs")


-- | Returns the repository directory for a given configuration and a
-- given repository.
getRepositoryDirectory
  :: MonadError T.Text m
  => Config.Config
  -> Github.GithubRepo
  -> m (P.Path P.Abs P.Dir)
getRepositoryDirectory Config.Config {..} Github.GithubRepo {..} =
  (\x -> configStore P.</> $(P.mkRelDir "backups") P.</> x) <$> reldir
  where
    reldir = either (throwError . ("Invalid repository directory path segment: " <>) . Z.Text.tshow) pure (P.parseRelDir (T.unpack githubRepoId))


-- | Returns the repository backup directory for a given configuration
-- and a given repository.
getRepositoryBackupDirectory
  :: MonadError T.Text m
  => Config.Config
  -> Github.GithubRepo
  -> m (P.Path P.Abs P.Dir)
getRepositoryBackupDirectory cfg repo =
  (P.</> $(P.mkRelDir "data")) <$> getRepositoryDirectory cfg repo


-- | Returns the repository metadata file path for a given
-- configuration and a given repository.
getRepositoryMetadataFile
  :: MonadError T.Text m
  => Config.Config
  -> Github.GithubRepo
  -> m (P.Path P.Abs P.File)
getRepositoryMetadataFile cfg repo =
  (P.</> $(P.mkRelFile "metadata.json")) <$> getRepositoryDirectory cfg repo
