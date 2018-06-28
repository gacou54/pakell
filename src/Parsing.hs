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
import Data.String.Utils (replace)
import Data.Text (strip, unpack)
import Data.Version (showVersion)
import Data.List (isInfixOf)
import Turtle
----------------
import Utils (filePathToString, niceString, shellToList)


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
parserTODO = fmap (find' " TODO")
                 (subcommand "todo" "Find TODO notes"
                      (argPath "path" "path of file or directory"))
-- ---------

-- ---------
parserFIXME :: Parser (IO ())
parserFIXME = fmap (find' " FIXME")
                 (subcommand "fixme" "Find FIXME notes"
                      (argPath "path" "path of file or directory"))
-- ---------

-- ---------
parserNOTE :: Parser (IO ())
parserNOTE = fmap (find' " NOTE")
                 (subcommand "note" "Find NOTE notes"
                      (argPath "path" "path of file or directory"))
-- ---------

-- ---------
parserTEMP :: Parser (IO ())
parserTEMP = fmap (find' " TEMP")
                 (subcommand "temp" "Find TEMP notes"
                      (argPath "path" "path of file or directory"))
-- ---------

-- ---------
parserREVIEW :: Parser (IO ())
parserREVIEW = fmap (find' " REVIEW")
                 (subcommand "review" "Find REVIEW notes"
                      (argPath "path" "path of file or directory"))
-- ---------

-- ---------
parserOPTIMIZE :: Parser (IO ())
parserOPTIMIZE = fmap (find' " OPTIMIZE")
                      (subcommand "optimize" "Find OPTIMIZE notes"
                            (argPath "path" "path of file or directory"))
-- ---------

-- ---------
parserBUG :: Parser (IO ())
parserBUG = fmap (find' " BUG")
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


-- This is the main "find" function.
-- It takes a String and a FilePath. The function looks
-- in the file to find occurence of the given String.
-- If there is, print them in a nice way
-- -----------------------------------------------------
find' :: String -> FilePath -> IO ()
find' word p = do
  -- find out path status
  statusPath <-  stat p
  findWithStatus statusPath word p


-- Find word
-- It depends on the status file (directory or file)
-- -----------------------------------------------------
findWithStatus :: FileStatus -> String -> FilePath -> IO ()
findWithStatus statusPath word p
  | isRegularFile statusPath = do

      -- This part is the one for a single file
      -- ---------------------------------------
      -- getting a list of all line in file
      lines' <- readLines $ filePathToString p

      printer p word lines'

  | isDirectory statusPath = do

      -- This part is the one for a directory
      -- ---------------------------------------
      -- getting a list of all file in directory
      files <- shellToList $ lstree p

      -- getting a list of all line in file
      let filesString = [filePathToString f | f <- files]
      print filesString
      -- getting list of list of line (a list for each file)
      -- let lines' = map readLines filesString
      -- let lines' = [readLines f | f <- filesString]

      -- let toto = head lines'

      -- map (printer p word) lines'

-- -----------------------------------------------------


-- Printer
-- -----------------------------------------------------
printer :: FilePath -> String -> [String] -> IO ()
printer p word lines' = do
  -- getting a list of tuple, fst element is the number of line
  -- and snd element is the line
  let numberAndLines = zip [1..] lines'

  -- take the "word" parameter and try to find it in lines.
  -- check the function "numberAndLines" to understand what is happening

  putStrLn $ "\n\x1b[38;5;45m" ++ filePathToString p ++ "\x1b[0m"
  putStrLn "-------------------"

  -- getting a nice string of all lines with the <keyword>
  let s = niceString $ findWord word numberAndLines

  -- word with ascii format for color and bold text
  let asciiWord = "\x1b[1m\x1b[38;5;82m" ++ word ++ "\x1b[0m"

  -- replace print the result
  putStrLn $ replace word asciiWord s
-- -----------------------------------------------------


-- Read lines of given filename
-- -----------------------------------------------------
readLines :: String -> IO [String]
readLines = fmap lines . readFile
-- -----------------------------------------------------


-- Find word in lines
-- -----------------------------------------------------
findWord :: String -> [(Integer, String)] -> [(Integer, String)]
findWord _ [] = []
findWord word [nl]
  | isInfixOf word (snd nl) = [nl]
  | otherwise               = []
findWord word (nl:numAndLines)
  | isInfixOf word (snd nl) = [nl] ++ (findWord word numAndLines)
  | otherwise               = findWord word numAndLines
-- -----------------------------------------------------

