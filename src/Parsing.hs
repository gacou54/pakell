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
import Control.Exception
import System.IO.Error
import Turtle
----------------
import Utils ( filePathToString
             , listToString
             , niceString
             , shellToList )


-- Main routine
-- ----------------------------------------------
parserMain :: Parser (IO ())
parserMain = fmap mainSubroutine argRecursiveMain


argRecursiveMain :: Parser (Bool, Bool)
argRecursiveMain = (,) <$> switch "recursive" 'r' "Look recursivly in directories"
                       <*> switch "hidden" 'd' "Also parse hidden file/directory"


mainSubroutine :: (Bool, Bool) -> IO ()
mainSubroutine (r, h) = do
  currenPath <- pwd                -- current path
  look Nothing (r, h, currenPath)  -- look
-- ----------------------------------------------


-- Commands
-- ----------------------------------------------
-- ---------
parserLookfor :: Parser (IO ())
parserLookfor = fmap lookfor
                  (subcommand "lookfor" "Look for specified word" argLookfor)

parserLf :: Parser (IO ())
parserLf = fmap lookfor
                  (subcommand "lf" "Alias for look command" argLookfor)

argLookfor :: Parser (Maybe FilePath, Bool, Bool, Text)
argLookfor = (,,,) <$> optional (optPath "path" 'p' "Path to look in")
                   <*> switch "recursive" 'r' "Look recursivly in directories"
                   <*> switch "hidden" 'd' "Also parse hidden file/directory"
                   <*> (argText "Keyword" "Look for the keyword")
-- ---------

-- ---------
parserLook :: Parser (IO ())
parserLook = fmap (look Nothing)
                  (subcommand "look" "Look for keywords notes" argRecursive)

parserL :: Parser (IO ())
parserL = fmap (look Nothing)
                (subcommand "l" "Alias for look command" argRecursive)

argRecursive :: Parser (Bool, Bool, FilePath)
argRecursive = (,,) <$> switch "recursive" 'r' "Look recursivly in directories"
                    <*> switch "hidden" 'd' "Also parse hidden file/directory"
                    <*> (argPath "PATH" "path of file or directory")
-- ---------

-- ---------
parserAdd :: Parser (IO ())
parserAdd = fmap add
                 (subcommand "add" "Add a keyword"
                      (argText "WORD" "Keyword"))
-- ---------

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

-- ---------
parserVersion :: Parser (IO ())
parserVersion = subcommand "version" "Show version" $ pure $ verboseVersion
-- ---------
-- ----------------------------------------------


-- look for a specified keyword
-- -----------------------------------------------------
lookfor :: (Maybe FilePath, Bool, Bool, Text) -> IO ()
lookfor (Nothing, r, h, text) = do              -- case with no specified path
  currenPath <- pwd                             -- getitng current path
  look (Just [unpack text]) (r, h, currenPath)  -- look

lookfor (Just p, r, h, text) = do               -- case with specified path
  look (Just [unpack text]) (r, h, p)           -- look
-- -----------------------------------------------------


-- look into specified path if one of the config keyword
-- is there
--
-- Parameters
-- -----------
-- FilePath : File path of target file or directory
--
-- look function have 4 case
-- -----------------------------------------------------

look :: Maybe [String] -> (Bool, Bool, FilePath) -> IO ()
look Nothing (r, h, p) = do  -- case with no specified word
  keywords <- getKeyWords    -- getting keywords
  status   <- stat p         -- getting status

  if (isRegularFile status)
    then do
      find' keywords p  -- find keyword

    else if (isDirectory status)
      then do
        paths       <- shellToList $ ls p  -- getting path
        filesStatus <- mapM stat paths     -- getting all status

        -- is files
        let isFiles = map isRegularFile filesStatus

        -- is directories
        let isDirs  = map isDirectory filesStatus

        -- REVIEW: there should be a better way to do that
        -- must pass the "-d | --hidden" argument
        -- pass on hidden path (/.<path>)
        let hidden    = map (not h &&) $
                        map (\p -> isInfixOf "/." p) $ map filePathToString paths
        let isOkFiles = map (\(h, f) -> not h && f)  $ zip hidden isFiles
        let isOkDirs  = map (\(h, d) -> not h && d)  $ zip hidden isDirs

        -- only files should be marked as True here
        mapM_ (find' keywords) $ keep isOkFiles paths

        -- only directories should be marked as True here
        when r $ mapM_ (\x -> look Nothing (r, h, x)) $ keep isOkDirs paths

    else return ()

look word (r, h, p) = do        -- case specified word
  let keyword =  fromJust word  -- getting word
  status      <- stat p         -- getting status

  if (isRegularFile status)
    then do
      find' keyword p  -- find keyword

    else if (isDirectory status)
      then do
        paths       <- shellToList $ ls p  -- getting paths
        filesStatus <- mapM stat paths     -- getting all status

        -- is files
        let isFiles = map isRegularFile filesStatus

        -- is directories
        let isDirs  = map isDirectory filesStatus

        -- REVIEW: there should be a better way to do that
        -- pass on hidden path (.<path>)
        let hidden    = map (not h &&) $
                        map (\p -> isInfixOf "/." p) $ map filePathToString paths
        let isOkFiles = map (\(h, f) -> not h && f)  $ zip hidden isFiles
        let isOkDirs  = map (\(h, d) -> not h && d)  $ zip hidden isDirs

        -- only files should be marked as True here
        mapM_ (find' keyword) $ keep isOkFiles paths

        -- only directories should be marked as True here
        when r $ mapM_ (\x -> look word (r, h, x)) $ keep isOkDirs paths

    else return ()
-- -----------------------------------------------------


-- This is the main "find" function.
-- The function looks in the file to find occurence of the given String.
-- If there is, print them in a nice way
-- -----------------------------------------------------
find' :: [String] -> FilePath -> IO ()
find' []       p = return ()
find' keywords p = do
  -- FIXME : some file cannot be read,
  -- I tried something but not seems to work


  h <- openFile (filePathToString p) ReadMode  -- getting lines
  hSetEncoding h latin1  -- set encoding
  fileString  <- hGetContents h

  -- FIXME: This is very bad
  let lines' = lines fileString

  when ((lines' /= []) && foldl1 (&&) (map (\x -> length x < 400) lines'))
    (printer keywords p $ lines fileString)  -- print formated lines

  hClose h

-- -----------------------------------------------------


-- Add a keyword into config file
-- -----------------------------------------
add :: Text -> IO ()
add word = do
  -- look if word is in config
  -- -------------------------
  let wordString =  unpack word                 -- get String version
  inConfig       <- checkIfInConfig wordString  -- check in config
  -- -------------------------

  case inConfig of
    True  -> putStrLn "Already a keyword"
    False -> do
      pakellConfig <- getConfigPath                       -- getting path
      current      <- readFile $ fromString pakellConfig  -- getting config

      rm $ fromString pakellConfig  -- delete old config file

      -- create a new config file with the added word
      writeFile (fromString pakellConfig) (current++wordString++"\n")
-- -----------------------------------------


-- Remove a keyword to remove from config file
-- -----------------------------------------
remove :: Text -> IO ()
remove word = do
  let wordString =  unpack word  -- look if word is in config
  inConfig       <- checkIfInConfig wordString

  case inConfig of
    False -> putStrLn "Not a stored keyword"
    True  -> do
      pakellConfig <- getConfigPath           -- getting config path
      currents     <- readLines pakellConfig  -- list of String

      -- get new list of keyword
      let news     =  filter (\s -> s /= wordString) currents

      rm $ fromString pakellConfig  -- delete old config file

      -- create a new config file without removed word
      writeFile (fromString pakellConfig) (listToString news)
-- ----------------------------------------------


-- List all keywords in config file
-- ----------------------------------------------
listWords :: IO ()
listWords = do
  pakellConfig <- getConfigPath     -- getting path
  content <- readFile pakellConfig  -- getting file content

  -- print it
  case content of
    [] -> return ()
    _  -> putStrLn $ init content
-- ----------------------------------------------


-- Erase all keywords from config file
-- ----------------------------------------------
clear :: IO ()
clear = do
  pakellConfig <- getConfigPath           -- get config path
  writeFile (fromString pakellConfig) ""  -- write an empty file
-- ----------------------------------------------


-- Get config file path
-- ----------------------------------------------
getConfigPath :: IO String
getConfigPath = do
  homePath <- home  -- getitng home path

  -- returning pakell config path
  return $ encodeString homePath ++ "/.config/pakell.conf"
-- ----------------------------------------------


-- Get keywords in config file
-- ----------------------------------------------
getKeyWords :: IO [String]
getKeyWords = do
  pakellConfig <- getConfigPath       -- getting config path
  keywords <- readLines pakellConfig  -- read each keayword (one per line)
  return keywords                     -- returning keywords
-- ----------------------------------------------


-- Check if keyword is in config
-- ----------------------------------------------
checkIfInConfig :: String -> IO Bool
checkIfInConfig word = do
  keywords <- getKeyWords  -- getting each keyword
  return $ foldl1 (||) $ map (\s -> s == word) keywords
-- ----------------------------------------------


-- Version
-- ----------------------------------------------
verboseVersion :: IO ()
verboseVersion = do
  echo "Version information:"
  putStrLn $ showVersion version
-- ----------------------------------------------


-- Keep or not element in list with an associated list of boolean
-- -----------------------------------------------------
keep :: [Bool] -> [a] -> [a]
keep [] []  = []
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

   -- list of word with ascii format for color and bold text
  let asciiWords = map asciiIt words

  when (foldl1 (||) boolList) (
    putStrLn $ replaceCombi s words asciiWords ) -- replace print the result
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


-- Ascii it, change the display format
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

