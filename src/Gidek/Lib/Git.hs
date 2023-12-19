{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides auxiliary Git definitions.
module Gidek.Lib.Git where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Path as P
import qualified Path.IO as PIO
import System.Exit (ExitCode (..))
import qualified System.Process.Typed as TP


-- | Backs up a Git repository.
backup
  :: MonadIO m
  => MonadError T.Text m
  => T.Text
  -> P.Path P.Abs P.Dir
  -> m ()
backup uri dir = do
  exists <- PIO.doesDirExist dir
  if exists
    then update uri dir
    else clone uri dir


-- | Runs @git clone --mirror@ to backup a repository.
clone
  :: MonadIO m
  => MonadError T.Text m
  => T.Text
  -> P.Path P.Abs P.Dir
  -> m ()
clone uri dir = do
  (ec, _, stderr) <- TP.readProcess proc
  case ec of
    ExitFailure _ -> throwError ("Git error: " <> TL.toStrict (TLE.decodeUtf8 stderr))
    ExitSuccess -> pure ()
  where
    args = ["clone", "--mirror", T.unpack uri, P.toFilePath dir]
    proc = TP.proc "git" args


-- | Runs @git remote update@ to update a repository backup.
update
  :: MonadIO m
  => MonadError T.Text m
  => T.Text
  -> P.Path P.Abs P.Dir
  -> m ()
update uri dir = do
  updateOrigin uri dir
  (ec, _, stderr) <- TP.readProcess proc
  case ec of
    ExitFailure _ -> throwError ("Git error: " <> TL.toStrict (TLE.decodeUtf8 stderr))
    ExitSuccess -> pure ()
  where
    args = ["remote", "update"]
    proc = TP.setWorkingDir (P.toFilePath dir) (TP.proc "git" args)


-- | Updates origin URL.
updateOrigin
  :: MonadIO m
  => MonadError T.Text m
  => T.Text
  -> P.Path P.Abs P.Dir
  -> m ()
updateOrigin uri dir = do
  (ec, _, stderr) <- TP.readProcess proc
  case ec of
    ExitFailure _ -> throwError ("Git error: " <> TL.toStrict (TLE.decodeUtf8 stderr))
    ExitSuccess -> pure ()
  where
    args = ["remote", "set-url", "origin", T.unpack uri]
    proc = TP.setWorkingDir (P.toFilePath dir) (TP.proc "git" args)
