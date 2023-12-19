{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides definitions for application configuration.
module Gidek.Lib.Config where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Gidek.Lib.Github as Github
import qualified Path as P
import qualified Zamazingo.Aeson as Z.Aeson
import qualified Zamazingo.Text as Z.Text


-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTemplateHaskell
-- >>> :set -XTypeApplications
-- >>> import qualified Path as P
-- >>> import qualified Path.IO as PIO
-- >>> import Control.Monad.Except (runExceptT)


-- | Application configuration definition.
data Config = Config
  { configStore :: !(P.Path P.Abs P.Dir)
  , configToken :: !(Maybe T.Text)
  , configTokenFile :: !(Maybe (P.Path P.Abs P.File))
  , configRepos :: ![Github.GithubRepoSource]
  }
  deriving (Eq, Generic, Show)


-- | 'Aeson.FromJSON' instance for 'Config'.
--
-- >>> Aeson.decode @Config "{\"repos\":[{\"name\":\"vst/gidek\",\"type\":\"single\"},{\"name\":\"vst\",\"type\":\"user\"},{\"name\":\"fourmolu\",\"type\":\"organization\"}],\"store\":\"/tmp/gidek/store/\",\"token\":\"gho_hebelehubele\",\"token_file\":\"/var/run/secrets/github_token\"}"
-- Just (Config {configStore = "/tmp/gidek/store/", configToken = Just "gho_hebelehubele", configTokenFile = Just "/var/run/secrets/github_token", configRepos = [GithubRepoSourceSingle "vst/gidek",GithubRepoSourceUser "vst",GithubRepoSourceOrganization "fourmolu"]})
instance Aeson.FromJSON Config where
  parseJSON = Aeson.genericParseJSON _aesonOptionsConfig


-- | 'Aeson.FromJSON' instance for 'Config'.
--
-- >>> let store = $(P.mkAbsDir "/tmp/gidek/store")
-- >>> let token = Just "gho_hebelehubele"
-- >>> let tokenFile = Just $(P.mkAbsFile "/var/run/secrets/github_token")
-- >>> let repos = [Github.GithubRepoSourceSingle "vst/gidek", Github.GithubRepoSourceUser "vst", Github.GithubRepoSourceOrganization "fourmolu"]
-- >>> let config = Config {configStore=store, configToken=token, configTokenFile=tokenFile, configRepos=repos}
-- >>> Aeson.encode config
-- "{\"repos\":[{\"name\":\"vst/gidek\",\"type\":\"single\"},{\"name\":\"vst\",\"type\":\"user\"},{\"name\":\"fourmolu\",\"type\":\"organization\"}],\"store\":\"/tmp/gidek/store/\",\"token\":\"gho_hebelehubele\",\"token_file\":\"/var/run/secrets/github_token\"}"
-- >>> Z.Aeson.testRoundtrip config
-- True
instance Aeson.ToJSON Config where
  toJSON = Aeson.genericToJSON _aesonOptionsConfig


-- | "Aeson" options for decoding and encoding 'Config' values.
_aesonOptionsConfig :: Aeson.Options
_aesonOptionsConfig =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Z.Aeson.aesonStripToSnake "config"
    }


-- | Attempts to read 'Config' from the YAML file at the given
-- absolute path.
--
-- >>> path <- (P.</> $(P.mkRelFile "config.tmpl.yaml")) <$> PIO.getCurrentDir
-- >>> runExceptT (readConfigFile path)
-- Right (Config {configStore = "/tmp/gidek/store/", configToken = Just "gho_hebelehubele", configTokenFile = Just "/var/run/secrets/github_token", configRepos = [GithubRepoSourceSingle "vst/gidek",GithubRepoSourceUser "vst",GithubRepoSourceOrganization "fourmolu"]})
readConfigFile
  :: MonadIO m
  => MonadError T.Text m
  => P.Path P.Abs P.File
  -> m Config
readConfigFile p = do
  either onErr pure =<< liftIO (Yaml.decodeFileEither (P.toFilePath p))
  where
    onErr = throwError . ("Error while reading configuration file: " <>) . Z.Text.tshow
