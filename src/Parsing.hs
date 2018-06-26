{-# LANGUAGE OverloadedStrings #-}
module Parsing
    ( parserMain
    , parserVersion
    , parserTODO
    , parserFIXME
    , parserNOTE
    , parserTEMP
    , parserREVIEW
    , parserOPTIMIZE
    , parserBUG
    ) where


import Paths_pakell (version)
----------------
import Prelude hiding (FilePath)
import qualified Data.Text
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
parserTODO = fmap (find' "TODO")
                 (subcommand "todo" "Find TODO notes"
                      (argPath "path" "path of file or directory"))
-- ---------

-- ---------
parserFIXME :: Parser (IO ())
parserFIXME = fmap (find' "FIXME")
                 (subcommand "fixme" "Find FIXME notes"
                      (argPath "path" "path of file or directory"))
-- ---------

-- ---------
parserNOTE :: Parser (IO ())
parserNOTE = fmap (find' "NOTE")
                 (subcommand "note" "Find NOTE notes"
                      (argPath "path" "path of file or directory"))
-- ---------

-- ---------
parserTEMP :: Parser (IO ())
parserTEMP = fmap (find' "TEMP")
                 (subcommand "temp" "Find TEMP notes"
                      (argPath "path" "path of file or directory"))
-- ---------

-- ---------
parserREVIEW :: Parser (IO ())
parserREVIEW = fmap (find' "REVIEW")
                 (subcommand "review" "Find REVIEW notes"
                      (argPath "path" "path of file or directory"))
-- ---------

-- ---------
parserOPTIMIZE :: Parser (IO ())
parserOPTIMIZE = fmap (find' "OPTIMIZE")
                      (subcommand "optimize" "Find OPTIMIZE notes"
                            (argPath "path" "path of file or directory"))
-- ---------

-- ---------
parserBUG :: Parser (IO ())
parserBUG = fmap (find' "BUG")
                 (subcommand "bug" "Find BUG notes"
                      (argPath "path" "path of file or directory"))
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

-- find'
find' :: String -> FilePath -> IO ()
find' word p = do
  -- getting a list of all file name
  lines' <- readLines $ filePathToString p
  -- getting a list of tuple, fst element is the number of line
  -- and snd element is the line
  let numberAndLines = zip [1..] lines'
  -- take the "word" parameter and try to find it in lines.
  -- check the function "numberAndLines" to understand what is happening
  findAndPrint word numberAndLines


-- Read lines of given filename
readLines :: String -> IO [String]
readLines = fmap lines . readFile


-- Find and print lines
findAndPrint :: String -> [(Integer, String)] -> IO ()
findAndPrint word numberAndLines = do
  let nl =  filter (isInfixOf word) (fmap snd numberAndLines)
  print nl


-- convert FilePath to String
filePathToString :: FilePath -> String
filePathToString = Data.Text.unpack . format fp
