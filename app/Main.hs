module Main where

import qualified Gidek.Cli as Cli
import System.Exit (exitWith)


main :: IO ()
main = Cli.cli >>= exitWith
