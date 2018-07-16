{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle
----------------
import Utils   ( filePathToString )
import Parsing ( parserMain
               , parserLook
               , parserLookfor
               , parserAdd
               , parserRemove
               , parserList
               , parserClear
               , parserVersion
               , parserL
               , parserLf
               , parserLs )


main :: IO ()
main = do
  -- create config file if not exist
  -- ---------------------------------------------------------------------
  -- FilePath type, home dir (/home/<user>)
  homePath <- home

  -- String of config path
  let configPath   = encodeString homePath ++ "/.config"
  let pakellConfig = encodeString homePath ++ "/.config/pakell.conf"

  -- getting boolean : True if file exists; False if not exists
  configPathExist <- testdir  $ fromString configPath
  configExist     <- testfile $ fromString pakellConfig

  -- if .config does not exist, create it
  when (not configPathExist) $ mkdir $ fromString configPath

  -- if pakell.conf does not exist, create it
  when (not configExist) $ touch $ fromString pakellConfig
  -- ---------------------------------------------------------------------

  -- command
  cmd <- options "Options for pakell parsing tool" parser
  cmd


parser :: Parser (IO ())
parser = parserMain <|>
  parserLook        <|>
  parserLookfor     <|>
  parserAdd         <|>
  parserRemove      <|>
  parserList        <|>
  parserClear       <|>
  parserVersion     <|>
  parserL           <|>
  parserLf          <|>
  parserLs

