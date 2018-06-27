{-# LANGUAGE OverloadedStrings #-}
module Utils
    ( filePathToString
    , niceString
    , shellToList
    ) where


import Prelude hiding (FilePath)
import qualified Control.Foldl as Foldl
import Data.Text (strip, unpack)
import Turtle


-- Convert FilePath to String
-- -----------------------------------------------------
filePathToString :: FilePath -> String
filePathToString = unpack . format fp
-- -----------------------------------------------------

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

-- Puts the informations into a nicely formated string
-- -----------------------------------------------------
niceString :: [(Integer, String)] -> String
niceString []               = []
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
-- -----------------------------------------------------

-- To list (form Shell)
-- -----------------------------------------------------
shellToList :: Shell a -> IO [a]
shellToList s = fold s Foldl.list
-- -----------------------------------------------------
