{-# LANGUAGE OverloadedStrings #-}
module Parsing
    ( parserMain
    , parserLook
    , parserLookfor
    , parserAdd
    , parserRemove
    , parserList
    , parserClear
    , parserVersion
    , parserL
    , parserLf
    , parserLs
    ) where


import Paths_pakell (version)
----------------
import Prelude hiding (FilePath)
import qualified GHC.IO
import System.IO hiding (FilePath)
import Data.Maybe (fromJust)
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
mainSubroutine = do
  currenPath <- pwd
  keywords   <- getKeyWords
  look (Just keywords) currenPath
-- ----------------------------------------------


-- Commands
-- ----------------------------------------------
-- ---------
parserLookfor :: Parser (IO ())
parserLookfor = fmap lookfor
                  (subcommand "lookfor" "Look for specified word" argLookfor)

argLookfor :: Parser (Maybe FilePath, Text)
argLookfor = (,) <$> optional (optPath "path" 'p' "Path to look in")
                 <*> (argText "Keyword" "Look for the keyword")
-- ---------
--
-- ---------
parserLf :: Parser (IO ())
parserLf = fmap lookfor
                  (subcommand "lf" "Alias for look command" argLookfor)
-- ---------

-- ---------
parserLook :: Parser (IO ())
parserLook = fmap (look Nothing )
                  (subcommand "look" "Look for keywords notes"
                      (argPath "PATH" "path of file or directory"))
-- ---------

-- ---------
parserL :: Parser (IO ())
parserL = fmap (look Nothing)
                (subcommand "l" "Alias for look command"
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

-- ---------
parserLs :: Parser (IO ())
parserLs = (subcommand "ls" "Alias for list command" (pure listWords))
-- ---------

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
--
-- lookfor function
-- look for a specified keyword
-- -----------------------------------------------------
lookfor :: (Maybe FilePath, Text) -> IO ()
lookfor (Nothing, text) = do -- case with no specified path
  -- getting string from text
  let s = unpack text

  -- getitng current path
  currenPath <- pwd

  -- look
  look (Just [s]) currenPath

lookfor (Just p, text) = do
  -- getting string from text
  let s = unpack text

  -- look
  look (Just [s]) p
-- -----------------------------------------------------


-- look function
-- look into specified path if one of the config keyword
-- is there
--
-- Parameters
-- -----------
-- FilePath : File path of target file or directory
-- -----------------------------------------------------
look :: Maybe [String] -> FilePath -> IO ()
look Nothing p = do
  keywords <- getKeyWords

  status <- stat p
  if (isRegularFile status)
    then do
      -- FIXME : some file cannot be read,
      -- I tried something but not seems to work

      -- getting encoding
      let s    = filePathToString p
      h        <- openFile s ReadMode
      encoding <- hGetEncoding h

      case encoding of
        Just utf8 -> find' keywords p
        _         -> print ()

    else if (isDirectory status)
      then do
        paths <- shellToList $ ls p

        -- getting all status
        filesStatus <- mapM stat paths

        -- is files
        let isFiles = map isRegularFile filesStatus

        -- is directories
        let isDirs  = map isDirectory filesStatus

        -- only files should be marked as True here
        mapM_ (find'  keywords) $ keep isFiles paths

        -- FIXME : add an option to recursivly parse directories
        -- (just uncomment below)
        -- only directories should be markes as True here
        -- mapM_ look $ keep isDirs paths

    else do
      putStrLn "Not a file nor a directory"

look word p = do
  -- getting word
  let keyword = fromJust word

  -- getting status
  status <- stat p

  if (isRegularFile status)
    then do
      -- FIXME : some file cannot be read,
      -- I tried something but not seems to work

      -- getting encoding
      let s    = filePathToString p
      h        <- openFile s ReadMode
      encoding <- hGetEncoding h

      case encoding of
        Just utf8 -> find' keyword p
        _         -> return ()

    else if (isDirectory status)
      then do
        paths <- shellToList $ ls p

        -- getting all status
        filesStatus <- mapM stat paths

        -- is files
        let isFiles = map isRegularFile filesStatus

        -- is directories
        let isDirs  = map isDirectory filesStatus

        -- only files should be marked as True here
        mapM_ (find' keyword) $ keep isFiles paths

        -- FIXME : add an option to recursivly parse directories
        -- (just uncomment below)
        -- only directories should be markes as True here
        -- mapM_ look $ keep isDirs paths

    else do
      putStrLn "Not a file nor a directory"



-- keep or not element in list with an associated list of boolean
-- -----------------------------------------------------
keep :: [Bool] -> [a] -> [a]
keep [] [x] = error $ "Error: list and associated list of boolean " ++
                      "are not the same length"
keep [b] [] = error $ "Error: list and associated list of boolean " ++
                      "are not the same length"
keep [b] [x]
  | b         = [x]
  | otherwise = []
keep (b:bs) (x:xs)
  | b         = [x] ++ keep bs xs
  | otherwise = keep bs xs
-- -----------------------------------------------------


-- This is the main "find" function.
-- It takes a String and a FilePath. The function looks
-- in the file to find occurence of the given String.
-- If there is, print them in a nice way
-- -----------------------------------------------------
find' :: [String] -> FilePath -> IO ()
find' [] p = return ()
find' keywords p = do

  lines' <- readLines $ filePathToString p
  printer keywords p lines'
-- -----------------------------------------------------


-- Printer
-- -----------------------------------------------------
printer :: [String] -> FilePath -> [String] -> IO ()
printer words p lines' = do
  -- getting a list of tuple, fst element is the number of line
  -- and snd element is the line
  let numberAndLines = zip [1..] lines'

  -- now we have to confirm that lines are valid.
  -- we create a boolean list that indicates that
  -- line is valid or not
  boolList <- mapM (okLine words) numberAndLines

  -- case where boolList is an empty list
  when (foldl1 (||) boolList) ( putStrLn $ "\n\x1b[38;5;45m"  ++
                                         filePathToString p ++
                                         "\x1b[0m" )
  when (foldl1 (||) boolList) ( putStrLn "-------------------" )

  -- getting a nice string of all lines with the <keyword>
  let s = niceString $ keepLines boolList numberAndLines

   -- list of wor with ascii format for color and bold text
  let asciiWords = map asciiIt words

  when (foldl1 (||) boolList) (
    -- replace print the result
    putStrLn $ replaceCombi s words asciiWords )
-- -----------------------------------------------------


-- Read lines of given filename
-- -----------------------------------------------------
readLines :: String -> IO [String]
readLines = fmap lines . readFile
-- -----------------------------------------------------

-- Check if keywords are in line
-- -----------------------------------------------------
okLine :: [String] -> (Integer, String) -> IO Bool
okLine [] _     = return False
okLine words nl = return $ foldl1 (||) $ fmap (flip (isInfixOf) (snd nl)) words
-- -----------------------------------------------------

-- Add or not to an the list of lines that will be print
-- -----------------------------------------------------
keepLines :: [Bool] -> [(Integer, String)] -> [(Integer, String)]
keepLines [b] [nl]
  | b == True = [nl]
  | otherwise = []
keepLines (b:bs) (nl:nls)
  | b == True = [nl] ++ keepLines bs nls
  | otherwise = keepLines bs nls
-- -----------------------------------------------------

-- ascii it, change the display format
-- -----------------------------------------------------
asciiIt :: String -> String
asciiIt []   = []
asciiIt word = "\x1b[1m\x1b[38;5;82m" ++ word ++ "\x1b[0m"
-- -----------------------------------------------------

-- replace with combinaison
-- -----------------------------------------------------
replaceCombi :: String -> [String] -> [String] -> String
replaceCombi bigString [w] [a] = replace w a bigString
replaceCombi bigString (w:ws) (a:as) = replaceCombi (replace w a bigString) ws as
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

