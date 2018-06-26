{-# LANGUAGE OverloadedStrings #-}
module Parsing
    ( parserMain
    , parserVersion
    , parserTODO
    , parserFIXME
    , parserNOTE
    , parserREVIEW
    , parserOPTIMIZE
    , parserBUG
    ) where


import Paths_pakell (version)
----------------
import Prelude hiding (FilePath)
import Data.Version (showVersion)
import Data.List (isInfixOf)
import Turtle


-- Main routine
-- ----------------------------------------------
parserMain :: Parser (IO ())
parserMain = pure mainSubroutine

mainSubroutine :: IO ()
mainSubroutine = echo "TODO: should call parse dir and show keywords"
-- ----------------------------------------------


-- Keywords command
-- ----------------------------------------------
-- ---------
parserTODO :: Parser (IO ())
parserTODO = subcommand "todo" "Find TODO notes" $ pure $ mainSubroutine
-- ---------

-- ---------
parserFIXME :: Parser (IO ())
parserFIXME = subcommand "fixme" "Find FIME notes" $ pure $ mainSubroutine
-- ---------

-- ---------
parserNOTE :: Parser (IO ())
parserNOTE = subcommand "note" "Find NOTE notes" $ pure $ mainSubroutine
-- ---------

-- ---------
parserREVIEW :: Parser (IO ())
parserREVIEW = subcommand "review" "Find REVIEW notes" $ pure $ mainSubroutine
-- ---------

-- ---------
parserOPTIMIZE :: Parser (IO ())
parserOPTIMIZE = subcommand "optimize" "Find OPTIMIZE notes" $ pure $ mainSubroutine
-- ---------

-- ---------
parserBUG :: Parser (IO ())
parserBUG = fmap print
                 (subcommand "bug" "Find BUG notes"
                      (argPath "path" "path of file or directory"))

findBUG :: Line -> IO ()
findBUG p = print p
-- ---------
-- ----------------------------------------------

-- Version
-- ----------------------------------------------
parserVersion :: Parser (IO ())
parserVersion = subcommand "version" "Show version" $ pure $ verboseVersion

verboseVersion :: IO ()
verboseVersion = do
  echo "Version information:"
  putStrLn $ showVersion version
-- ----------------------------------------------

-- Read lines of given filename
readLines :: String -> IO [String]
readLines = fmap lines . readFile

-- Find and print lines
findAndPrint :: String -> [String] -> IO ()
findAndPrint word lines' = do
  let xs =  filter (isInfixOf word) lines'
  print xs
