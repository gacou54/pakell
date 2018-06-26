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

  putStrLn $ "\n\x1b[38;5;45m" ++ filePathToString p ++ "\x1b[0m"
  putStrLn "-------------------"

  -- getting a nice string of all lines with the <keyword>
  let s = niceString $ findWord word numberAndLines

  -- word with ascii format for color and bold text
  let asciiWord = "\x1b[1m\x1b[38;5;82m" ++ word ++ "\x1b[0m"

  -- replace print the result
  putStrLn $ replace word asciiWord s


-- Read lines of given filename
readLines :: String -> IO [String]
readLines = fmap lines . readFile


-- Find word in lines
findWord :: String -> [(Integer, String)] -> [(Integer, String)]
findWord word [nl]
  | isInfixOf word (snd nl) = [nl]
  | otherwise               = []
findWord word (nl:numAndLines)
  | isInfixOf word (snd nl) = [nl] ++ (findWord word numAndLines)
  | otherwise               = findWord word numAndLines


-- Puts the informations into a nicely formated string
niceString :: [(Integer, String)] -> String
niceString [nl]             = "  \x1b[38;5;3m"         ++
                              (show $ fst nl)          ++
                              " :  "                   ++
                              "\x1b[0m"                ++
                              trimWhiteSpace (snd nl)  ++ "\n"
niceString (nl:numAndLines) = "  \x1b[38;5;3m"         ++
                              (show $ fst nl)          ++
                              " :  "                   ++
                              "\x1b[0m"                ++
                              trimWhiteSpace (snd nl)  ++ "\n" ++
                              (niceString numAndLines)


-- Triming (strip) whitespace
-- -----------------------------------------------------
trimWhiteSpace :: String -> String
trimWhiteSpace [] = []
trimWhiteSpace [x]
  | x == ' '  = []
  | otherwise = [x]
trimWhiteSpace xs = trimWhiteSpaceSuffix $ trimWhiteSpacePrefix xs


trimWhiteSpacePrefix :: String -> String
trimWhiteSpacePrefix [] = []
trimWhiteSpacePrefix [x]
  | x == ' '  = []
  | otherwise = [x]
trimWhiteSpacePrefix xs
  | head xs == ' '  = trimWhiteSpacePrefix $ tail xs
  | otherwise       = xs


trimWhiteSpaceSuffix :: String -> String
trimWhiteSpaceSuffix [] = []
trimWhiteSpaceSuffix [x]
  | x == ' '        = []
  | otherwise       = [x]
trimWhiteSpaceSuffix xs
  | last xs == ' '  = trimWhiteSpaceSuffix xs
  | otherwise       = xs
-- -----------------------------------------------------


-- Convert FilePath to String
-- -----------------------------------------------------
filePathToString :: FilePath -> String
filePathToString = unpack . format fp
-- -----------------------------------------------------

-- Replace function
-- I found this in Data.String.Utils source code
-- -----------------------------------------------------
-- replace :: Eq a => [a] -> [a] -> [a] -> [a]
-- replace old new l = join new . split old $ l
-- -----------------------------------------------------

