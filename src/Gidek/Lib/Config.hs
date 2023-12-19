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
  , configToken :: !T.Text
  , configRepos :: ![Repos]
  }
  deriving (Eq, Generic, Show)


-- | 'Aeson.FromJSON' instance for 'Config'.
--
-- >>> Aeson.decode @Config "{\"repos\":[],\"store\":\"/tmp/gidek/store/\",\"token\":\"gho_hebelehubele\"}"
-- Just (Config {configStore = "/tmp/gidek/store/", configToken = "gho_hebelehubele", configRepos = []})
-- >>> Aeson.decode @Config "{\"repos\":[{\"name\":\"vst/gidek\",\"type\":\"repo\"},{\"name\":\"vst\",\"type\":\"user\"},{\"name\":\"fourmolu\",\"type\":\"organization\"}],\"store\":\"/tmp/gidek/store/\",\"token\":\"gho_hebelehubele\"}"
-- Just (Config {configStore = "/tmp/gidek/store/", configToken = "gho_hebelehubele", configRepos = [ReposRepo "vst/gidek",ReposUser "vst",ReposOrganization "fourmolu"]})
-- >>> Z.Aeson.testRoundtrip (Config $(P.mkAbsDir "/tmp/gidek/store") "gho_hebelehubele" [])
-- True
-- >>> Z.Aeson.testRoundtrip (Config $(P.mkAbsDir "/tmp/gidek/store") "gho_hebelehubele" [ReposRepo "vst/gidek", ReposUser "vst", ReposOrganization "fourmolu"])
-- True
instance Aeson.FromJSON Config where
  parseJSON = Aeson.genericParseJSON _aesonOptionsConfig


-- | 'Aeson.FromJSON' instance for 'Config'.
--
-- >>> Aeson.encode (Config $(P.mkAbsDir "/tmp/gidek/store") "gho_hebelehubele" [])
-- "{\"repos\":[],\"store\":\"/tmp/gidek/store/\",\"token\":\"gho_hebelehubele\"}"
-- >>> Aeson.encode (Config $(P.mkAbsDir "/tmp/gidek/store") "gho_hebelehubele" [ReposRepo "vst/gidek", ReposUser "vst", ReposOrganization "fourmolu"])
-- "{\"repos\":[{\"name\":\"vst/gidek\",\"type\":\"repo\"},{\"name\":\"vst\",\"type\":\"user\"},{\"name\":\"fourmolu\",\"type\":\"organization\"}],\"store\":\"/tmp/gidek/store/\",\"token\":\"gho_hebelehubele\"}"
instance Aeson.ToJSON Config where
  toJSON = Aeson.genericToJSON _aesonOptionsConfig


-- | "Aeson" options for decoding and encoding 'Repos' values.
_aesonOptionsConfig :: Aeson.Options
_aesonOptionsConfig =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Z.Aeson.aesonStripToSnake "config"
    }


-- | A 'Repos' is one of (1) a single repository specification, (2)
-- all repositories owned by a user, or (3) all repositories owned by
-- an organization.
data Repos
  = ReposRepo !T.Text
  | ReposUser !T.Text
  | ReposOrganization !T.Text
  deriving (Eq, Generic, Show)


-- | 'Aeson.FromJSON' instance for 'Repos'.
--
-- >>> Aeson.decode @Repos "{\"name\":\"vst/gidek\",\"type\":\"repo\"}"
-- Just (ReposRepo "vst/gidek")
-- >>> Aeson.decode @Repos "{\"name\":\"vst\",\"type\":\"user\"}"
-- Just (ReposUser "vst")
-- >>> Aeson.decode @Repos "{\"name\":\"fourmolu\",\"type\":\"organization\"}"
-- Just (ReposOrganization "fourmolu")
-- >>> Z.Aeson.testRoundtrip (ReposRepo "vst/gidek")
-- True
-- >>> Z.Aeson.testRoundtrip (ReposUser "vst")
-- True
-- >>> Z.Aeson.testRoundtrip (ReposOrganization "vst")
-- True
instance Aeson.FromJSON Repos where
  parseJSON = Aeson.genericParseJSON _aesonOptionsRepo


-- | 'Aeson.FromJSON' instance for 'Repos'.
--
-- >>> Aeson.encode (ReposRepo "vst/gidek")
-- "{\"name\":\"vst/gidek\",\"type\":\"repo\"}"
-- >>> Aeson.encode (ReposUser "vst")
-- "{\"name\":\"vst\",\"type\":\"user\"}"
-- >>> Aeson.encode (ReposOrganization "fourmolu")
-- "{\"name\":\"fourmolu\",\"type\":\"organization\"}"
instance Aeson.ToJSON Repos where
  toJSON = Aeson.genericToJSON _aesonOptionsRepo


-- | "Aeson" options for decoding and encoding 'Repos' values.
_aesonOptionsRepo :: Aeson.Options
_aesonOptionsRepo =
  Aeson.defaultOptions
    { Aeson.constructorTagModifier = Z.Aeson.aesonStripToSnake "Repos"
    , Aeson.sumEncoding = Aeson.TaggedObject {Aeson.tagFieldName = "type", Aeson.contentsFieldName = "name"}
    }


-- | Attempts to read 'Config' from the YAML file at the given
-- absolute path.
--
-- >>> path <- (P.</> $(P.mkRelFile "config.tmpl.yaml")) <$> PIO.getCurrentDir
-- >>> runExceptT (readConfigFile path)
-- Right (Config {configStore = "/tmp/gidek/store/", configToken = "gho_hebelehubele", configRepos = [ReposRepo "vst/gidek",ReposUser "vst",ReposOrganization "fourmolu"]})
readConfigFile
  :: MonadIO m
  => MonadError T.Text m
  => P.Path P.Abs P.File
  -> m Config
readConfigFile p = do
  either onErr pure =<< liftIO (Yaml.decodeFileEither (P.toFilePath p))
  where
    onErr = throwError . ("Error while reading configuration file: " <>) . Z.Text.tshow
