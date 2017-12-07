module Main where

import Text.Reparser
import System.Exit
import Data.Maybe

main :: IO ()
main = do
  body <- getContents
  case (reparse body) of
    (Just error) -> putStrLn (show error) >> exitWith (ExitFailure 1)
    _            -> exitWith ExitSuccess

