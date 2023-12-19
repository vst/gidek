{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides helpers for working with the GitHub API.
module Gidek.Lib.Github where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Combinators.Decode as ACD
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
import qualified System.Process.Typed as TP
import qualified Zamazingo.Aeson as Z.Aeson


-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications


-- | GitHub repository source specification.
--
-- It can be one of (1) a single repository specification, (2) all
-- repositories owned by a user, or (3) all repositories owned by an
-- organization.
data GithubRepoSource
  = GithubRepoSourceSingle !T.Text
  | GithubRepoSourceUser !T.Text
  | GithubRepoSourceOrganization !T.Text
  deriving (Eq, Generic, Show)


-- | 'Aeson.FromJSON' instance for 'GithubRepoSource'.
--
-- >>> Aeson.decode @GithubRepoSource "{\"name\":\"vst/gidek\",\"type\":\"single\"}"
-- Just (GithubRepoSourceSingle "vst/gidek")
-- >>> Aeson.decode @GithubRepoSource "{\"name\":\"vst\",\"type\":\"user\"}"
-- Just (GithubRepoSourceUser "vst")
-- >>> Aeson.decode @GithubRepoSource "{\"name\":\"fourmolu\",\"type\":\"organization\"}"
-- Just (GithubRepoSourceOrganization "fourmolu")
-- >>> Z.Aeson.testRoundtrip (GithubRepoSourceSingle "vst/gidek")
-- True
-- >>> Z.Aeson.testRoundtrip (GithubRepoSourceUser "vst")
-- True
-- >>> Z.Aeson.testRoundtrip (GithubRepoSourceOrganization "vst")
-- True
instance Aeson.FromJSON GithubRepoSource where
  parseJSON = Aeson.genericParseJSON _aesonOptionsGithubRepoSource


-- | 'Aeson.FromJSON' instance for 'GithubRepoSource'.
--
-- >>> Aeson.encode (GithubRepoSourceSingle "vst/gidek")
-- "{\"name\":\"vst/gidek\",\"type\":\"single\"}"
-- >>> Aeson.encode (GithubRepoSourceUser "vst")
-- "{\"name\":\"vst\",\"type\":\"user\"}"
-- >>> Aeson.encode (GithubRepoSourceOrganization "fourmolu")
-- "{\"name\":\"fourmolu\",\"type\":\"organization\"}"
instance Aeson.ToJSON GithubRepoSource where
  toJSON = Aeson.genericToJSON _aesonOptionsGithubRepoSource


-- | "Aeson" options for decoding and encoding 'GithubRepoSource' values.
_aesonOptionsGithubRepoSource :: Aeson.Options
_aesonOptionsGithubRepoSource =
  Aeson.defaultOptions
    { Aeson.constructorTagModifier = Z.Aeson.aesonStripToSnake "GithubRepoSource"
    , Aeson.sumEncoding = Aeson.TaggedObject {Aeson.tagFieldName = "type", Aeson.contentsFieldName = "name"}
    }


-- | Simple GitHub repository data definition.
data GithubRepo = GithubRepo
  { githubRepoId :: !T.Text
  , githubRepoUrl :: !T.Text
  , githubRepoName :: !T.Text
  , githubRepoOwner :: !T.Text
  }
  deriving (Eq, Generic, Show)


instance Aeson.FromJSON GithubRepo where
  parseJSON = Aeson.genericParseJSON _aesonOptionsGithubRepo


instance Aeson.ToJSON GithubRepo where
  toJSON = Aeson.genericToJSON _aesonOptionsGithubRepo


-- | "Aeson" options for decoding and encoding 'GithubRepo' values.
_aesonOptionsGithubRepo :: Aeson.Options
_aesonOptionsGithubRepo =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Z.Aeson.aesonStripToSnake "githubRepo"
    }


-- | Attempts to list all repositories of interest for a list of
-- GitHub repositories sources.
listRepositories
  :: MonadIO m
  => MonadError T.Text m
  => T.Text
  -> [GithubRepoSource]
  -> m [GithubRepo]
listRepositories tok sources =
  List.sortOn sortKey . List.nub . concat <$> mapM (listRepositoriesForSource tok) sources
  where
    sortKey GithubRepo {..} = (githubRepoOwner, githubRepoName)


-- | Attempts to list all repositories of interest for a given GitHub
-- repositories source.
listRepositoriesForSource
  :: MonadIO m
  => MonadError T.Text m
  => T.Text
  -> GithubRepoSource
  -> m [GithubRepo]
listRepositoriesForSource tok source =
  case source of
    GithubRepoSourceSingle handle -> (: []) <$> getRepository tok handle
    GithubRepoSourceUser user -> listUserRepositories tok user
    GithubRepoSourceOrganization organization -> listOrganizationRepositories tok organization


-- | Lists repositories for a given GitHub token and a given user
-- handle.
listUserRepositories
  :: MonadIO m
  => MonadError T.Text m
  => T.Text
  -> T.Text
  -> m [GithubRepo]
listUserRepositories tok user = do
  res <- runGithubQuery tok True _gqlListUserRepositories [("user", user)]
  either _onFail (pure . concat) (ACD.eitherDecode _parser res)
  where
    _onFail = throwError . ("Error while decoding response from GitHub API: " <>) . T.pack
    _parser = ACD.list (ACD.at ["data", "user", "repositories", "nodes"] (ACD.list _decoderSimpleGithubRepo))


-- | GraphQL query for listing user repositories.
_gqlListUserRepositories :: T.Text
_gqlListUserRepositories =
  [i|
query ($user: String!, $endCursor: String) {
  user(login: $user) {
    repositories(ownerAffiliations: OWNER, first: 100, after: $endCursor) {
      nodes {
        id
        url
        name
        owner {
          login
        }
      }
      pageInfo {
        hasNextPage
        endCursor
      }
    }
  }
}
|]


-- | Lists repositories for a given GitHub token and a given
-- organization handle.
listOrganizationRepositories
  :: MonadIO m
  => MonadError T.Text m
  => T.Text
  -> T.Text
  -> m [GithubRepo]
listOrganizationRepositories tok organization = do
  res <- runGithubQuery tok True _gqlListOrganizationRepositories [("organization", organization)]
  either _onFail (pure . concat) (ACD.eitherDecode _parser res)
  where
    _onFail = throwError . ("Error while decoding response from GitHub API: " <>) . T.pack
    _parser = ACD.list (ACD.at ["data", "organization", "repositories", "nodes"] (ACD.list _decoderSimpleGithubRepo))


-- | GraphQL query for listing organization repositories.
_gqlListOrganizationRepositories :: T.Text
_gqlListOrganizationRepositories =
  [i|
query ($organization: String!, $endCursor: String) {
  organization(login: $organization) {
    repositories(ownerAffiliations: OWNER, first: 100, after: $endCursor) {
      nodes {
        id
        url
        name
        owner {
          login
        }
      }
      pageInfo {
        hasNextPage
        endCursor
      }
    }
  }
}
|]


-- | Fetches and returns the GitHub repository for a given GitHub
-- token and the GitHub repository handle
-- (@[repository-owner]/[repository-name]@).
getRepository
  :: MonadIO m
  => MonadError T.Text m
  => T.Text
  -> T.Text
  -> m GithubRepo
getRepository tok handle = do
  res <- runGithubQuery tok False _gqlGetRepository [("owner", owner), ("name", name)]
  either _onFail pure (ACD.eitherDecode _parser res)
  where
    (owner, name) = decomposeRepoHandle handle
    _onFail = throwError . ("Error while decoding response from GitHub API: " <>) . T.pack
    _parser = ACD.at ["data", "repository"] _decoderSimpleGithubRepo


-- | GraphQL query for fetching a repository.
_gqlGetRepository :: T.Text
_gqlGetRepository =
  [i|
query($owner: String!, $name: String!) {
  repository(owner: $owner, name: $name) {
    id
    url
    name
    owner {
      login
    }
  }
}
|]


-- | JSON decoder for simple GitHub repository json payloads.
_decoderSimpleGithubRepo :: ACD.Decoder GithubRepo
_decoderSimpleGithubRepo =
  GithubRepo
    <$> ACD.key "id" ACD.text
    <*> ACD.key "url" ACD.text
    <*> ACD.key "name" ACD.text
    <*> ACD.at ["owner", "login"] ACD.text


-- | Returns the owner and name for a given GitHub repository handle.
--
-- >>> decomposeRepoHandle "vst/gidek"
-- ("vst","gidek")
decomposeRepoHandle :: T.Text -> (T.Text, T.Text)
decomposeRepoHandle r = T.dropWhile (== '/') <$> T.breakOn "/" r


-- | Attempts to run GitHub GraphQL query and returns the successful
-- result as a raw JSON string.
--
-- The author is too lazy to use GitHub API directly, so instead uses
-- @gh api graphql@ subcommand.
runGithubQuery
  :: MonadIO m
  => MonadError T.Text m
  => T.Text
  -> Bool
  -> T.Text
  -> [(T.Text, T.Text)]
  -> m BL.ByteString
runGithubQuery tok paginate query vars = do
  (ec, stdout, stderr) <- TP.readProcess proc
  case ec of
    ExitFailure _ -> throwError ("GitHub API Error: " <> TL.toStrict (TLE.decodeUtf8 stderr))
    ExitSuccess -> if paginate then runSlurp stdout else pure stdout
  where
    args = ["api", "graphql", "-f", [i|query=#{T.unpack query}|]] <> concatMap (\(k, v) -> ["-F", [i|#{k}=#{v}|]]) vars <> (["--paginate" | paginate])
    proc = TP.setEnv [("GH_TOKEN", T.unpack tok)] $ TP.proc "gh" args


-- | Author is proud to announce the winner of 42nd International
-- Competition of Ugly Hacks: Attempt to "slurp" given JSON documents
-- into an array of JSON values using an external process (@jq@).
runSlurp
  :: MonadIO m
  => MonadError T.Text m
  => BL.ByteString
  -> m BL.ByteString
runSlurp json = do
  (ec, stdout, stderr) <- TP.readProcess proc
  case ec of
    ExitFailure _ -> throwError ("JQ: " <> TL.toStrict (TLE.decodeUtf8 stderr))
    ExitSuccess -> pure stdout
  where
    proc = TP.setStdin (TP.byteStringInput json) (TP.proc "jq" ["--slurp"])
