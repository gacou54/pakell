{-# LANGUAGE OverloadedStrings #-}
module Parsing
    ( parserMain
    , parserVersion
    , parserTODO
    , parserTODO'
    , parserFIXME
    , parserNOTE
    , parserREVIEW
    , parserOPTIMIZE
    , parserBUG
    ) where


import Paths_pakell (version)
----------------
import Data.Version (showVersion)
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
parserTODO = subcommand "todo" "Find TODO notes" $ pure $ wordParser $ "TODO"

parserTODO' :: Parser (IO ())
parserTODO' = subcommand "TODO" "Find TODO notes" $ pure $ mainSubroutine
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
parserBUG = subcommand "bug" "Find BUG notes" $ pure $ mainSubroutine
-- ---------
-- ----------------------------------------------

-- Version
-- ----------------------------------------------
parserVersion :: Parser (IO ())
parserVersion = subcommand "version" "Show version" $ pure $ verboseVersion

verboseVersion :: IO ()
verboseVersion = do
  echo "Version information :"
  putStrLn $ showVersion version
-- ----------------------------------------------


wordParser :: String -> IO ()
wordParser s = do
  echo "TODO"

