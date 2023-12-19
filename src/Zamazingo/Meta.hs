{-# LANGUAGE OverloadedStrings #-}

-- | This module provides project metadata information definitions.
module Zamazingo.Meta where

import qualified Data.Text as T
import Data.Version (Version, showVersion)
import qualified Paths_gidek as Paths


-- | Application name.
--
-- >>> name
-- "gidek"
name :: T.Text
name = "gidek"


-- | Application title.
--
-- >>> title
-- "Backup Git(Hub) Repositories"
title :: T.Text
title = "Backup Git(Hub) Repositories"


-- | Application version.
--
-- > version
-- Version {versionBranch = [0,0,0], versionTags = []}
version :: Version
version = Paths.version


-- | Application version as a 'String' value.
--
-- > versionString
-- "0.0.0"
versionString :: String
versionString = showVersion version


-- | Application version as a 'T.Text' value.
--
-- > versionText
-- "0.0.0"
versionText :: T.Text
versionText = T.pack versionString
