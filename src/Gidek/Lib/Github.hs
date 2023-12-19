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
import qualified Gidek.Lib.Config as Config
import System.Exit (ExitCode (..))
import qualified System.Process.Typed as TP
import qualified Zamazingo.Aeson as Z.Aeson


-- $setup
--
-- >>> :set -XOverloadedStrings


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


-- | Attempts to list all repositories of interest for a given
-- configuration value.
listRepositories
  :: MonadIO m
  => MonadError T.Text m
  => Config.Config
  -> m [GithubRepo]
listRepositories Config.Config {..} =
  List.sortOn sortKey . List.nub <$> listRepositoriesAux configToken configRepos
  where
    sortKey GithubRepo {..} = (githubRepoOwner, githubRepoName)


-- | Attempts to list all repositories of interest for a given
-- list of repositories specification.
listRepositoriesAux
  :: MonadIO m
  => MonadError T.Text m
  => T.Text
  -> [Config.Repos]
  -> m [GithubRepo]
listRepositoriesAux tok reposs =
  concat <$> mapM (listRepositoriesAuxOne tok) reposs


-- | Attempts to list all repositories of interest for a given
-- repositories specification.
listRepositoriesAuxOne
  :: MonadIO m
  => MonadError T.Text m
  => T.Text
  -> Config.Repos
  -> m [GithubRepo]
listRepositoriesAuxOne tok repos =
  case repos of
    Config.ReposRepo handle -> (: []) <$> getRepository tok handle
    Config.ReposUser user -> listUserRepositories tok user
    Config.ReposOrganization organization -> listOrganizationRepositories tok organization


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
