{-# LANGUAGE OverloadedStrings #-}
module Parsing
    ( parserMain
    , parserVersion
    , parserTODO
    , parserLook
    , parserAdd
    , parserRemove
    , parserList
    , parserClear
    ) where


import Paths_pakell (version)
----------------
import Prelude hiding (FilePath)
import Data.String.Utils (replace)
import Data.Text (strip, pack, unpack)
import Data.Version (showVersion)
import Data.List (isInfixOf)
import Turtle
----------------
import Utils ( filePathToString
             , listToString
             , niceString
             , shellToList )


-- Main routine
-- ----------------------------------------------
parserMain :: Parser (IO ())
parserMain = pure mainSubroutine

mainSubroutine :: IO ()
mainSubroutine = echo "TODO: should call parse dir and show keywords"
-- ----------------------------------------------


-- Commands
-- ----------------------------------------------
-- ---------
parserTODO :: Parser (IO ())
parserTODO = fmap (find' " TODO")
                 (subcommand "todo" "Find TODO notes"
                      (argPath "path" "path of file or directory"))
-- ---------

-- ---------
parserLook :: Parser (IO ())
parserLook = fmap look
                  (subcommand "look" "Look for keywords notes"
                      (argPath "PATH" "path of file or directory"))
-- ---------

-- ---------
parserAdd :: Parser (IO ())
parserAdd = fmap add
                 (subcommand "add" "Add a keyword"
                      (argText "WORD" "Keyword"))
-- ---------
--
-- ---------
parserRemove :: Parser (IO ())
parserRemove = fmap remove
                 (subcommand "remove" "Remove a keyword"
                      (argText "WORD" "Keyword"))
-- ---------

-- ---------
parserList :: Parser (IO ())
parserList = (subcommand "list" "List all stored keywords" (pure listWords))
-- ---------
--
-- ---------
parserClear :: Parser (IO ())
parserClear = (subcommand "clear" "Clear all stored keywords" (pure clear))
-- ---------

-- ----------------------------------------------


-- add function
-- Add a keyword into config file
--
-- Parameters
-- -----------
-- Text : keyword to add in config file
-- -----------------------------------------
add :: Text -> IO ()
add word = do
  -- look if word is in config
  let wordString = unpack word
  inConfig <- checkIfInConfig $ wordString

  case inConfig of
    True  -> putStrLn "Already a keyword"
    False -> do
      pakellConfig <- getConfigPath
      current      <- readFile $ fromString pakellConfig

      -- current' (head current == "")
      --   True  -> tail current
      --   False -> current

      -- delete old config file
      rm $ fromString pakellConfig

      -- create a new one
      writeFile (fromString pakellConfig) (current++wordString++"\n")

--
-- remove function
-- Remove a keyword to remove from config file
--
-- Parameters
-- -----------
-- Text : keyword to remove from config file
-- -----------------------------------------
remove :: Text -> IO ()
remove word = do
  -- look if word is in config
  let wordString = unpack word
  inConfig <- checkIfInConfig wordString

  case inConfig of
    False -> putStrLn "Not a stored keyword"
    True  -> do
      pakellConfig <- getConfigPath
      -- list of String
      currents     <- readLines pakellConfig

      -- get new list of keyword
      let news      = filter (\s -> s /= wordString) currents

      -- delete old config file
      rm $ fromString pakellConfig

      -- create a new one
      writeFile (fromString pakellConfig) (listToString news)



listWords :: IO ()
listWords = do
  -- getting path
  pakellConfig <- getConfigPath
  -- getting file content
  content <- readFile pakellConfig
  -- print it
  case content of
    [] -> putStrLn ""
    _  -> putStrLn $ init content


clear :: IO()
clear = do
  -- get config path
  pakellConfig <- getConfigPath
  writeFile (fromString pakellConfig) ""

-- ----------------------------------------------
getConfigPath :: IO String
getConfigPath = do
  homePath <- home
  let pakellConfig = encodeString homePath ++ "/.config/pakell.conf"
  return pakellConfig


getKeyWords :: IO [String]
getKeyWords = do
  -- getting config path
  pakellConfig <- getConfigPath

  -- read each keayword (one per line)
  keywords <- readLines pakellConfig
  return keywords


checkIfInConfig :: String -> IO Bool
checkIfInConfig word = do
  -- getting each keyword
  keywords <- getKeyWords

  let x = filter (\s -> s == word) keywords
  case x of
    [] -> return False
    _  -> return True
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

-- look function
-- look into specified path if one of the config keyword
-- is there
--
-- Parameters
-- -----------
-- FilePath : File path of target file or directory
-- -----------------------------------------------------
look :: FilePath -> IO ()
look p = do
  keywords <- getKeyWords

  status <- stat p
  if (isRegularFile status)
    then do
      putStrLn $ "\n\x1b[38;5;45m" ++ filePathToString p ++ "\x1b[0m"
      putStrLn "-------------------"

      findWrapper keywords p

    else if (isDirectory status)
      then do
        files <- shellToList $ lstree p

        putStrLn $ "\n\x1b[38;5;45m" ++ filePathToString (head files) ++ "\x1b[0m"
        putStrLn "-------------------"

        mapM_ (findWrapper keywords) files

    else do
      putStrLn "Not a file nor a directory"


findWrapper :: [String] -> FilePath -> IO ()
findWrapper [x] p    = do
  find' x p
findWrapper (x:xs) p = do
  find' x p
  findWrapper xs p


-- This is the main "find" function.
-- It takes a String and a FilePath. The function looks
-- in the file to find occurence of the given String.
-- If there is, print them in a nice way
-- -----------------------------------------------------
find' :: String -> FilePath -> IO ()
find' word p = do
  lines' <- readLines $ filePathToString p
  printer p word lines'
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

