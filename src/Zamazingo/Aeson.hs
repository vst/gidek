-- | This module provides auxiliary definitions for working with
-- "Aeson".
module Zamazingo.Aeson where

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import Data.Maybe (fromMaybe)


-- | Strips the prefix string from the given string
aesonStripPrefix :: String -> String -> String
aesonStripPrefix prefix label = fromMaybe label (List.stripPrefix prefix label)


-- | Identifier symbol (assumed to be camel-case) to snake-case converter.
aesonToSnake :: String -> String
aesonToSnake = Aeson.camelTo2 '_'


-- | Strips the prefix string from the given string and then converts the
-- resulting string to snake-case string.
aesonStripToSnake :: String -> String -> String
aesonStripToSnake prefix label = aesonToSnake (aesonStripPrefix prefix label)


-- | Tests whether encoding and decoding a value yields the same
-- value.
--
-- >>> :set -XTypeApplications
-- >>> testRoundtrip @Int 42
-- True
-- >>> testRoundtrip @(Maybe Int) Nothing
-- True
-- >>> testRoundtrip @(Maybe Int) (Just 42)
-- True
testRoundtrip
  :: Aeson.FromJSON a
  => Aeson.ToJSON a
  => Eq a
  => a
  -> Bool
testRoundtrip v =
  Aeson.decode (Aeson.encode v) == Just v
