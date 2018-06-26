{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle
----------------
import Parsing ( parserMain
               , parserVersion
               , parserTODO
               , parserFIXME
               , parserNOTE
               , parserREVIEW
               , parserOPTIMIZE
               , parserBUG )


main :: IO ()
main = do
  cmd <- options "Options for pakell parsing tool" parser
  cmd


parser :: Parser (IO ())
parser = parserMain <|>
  parserVersion     <|>
  parserTODO        <|>
  parserFIXME       <|>
  parserNOTE        <|>
  parserREVIEW      <|>
  parserOPTIMIZE    <|>
  parserBUG

