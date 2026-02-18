module Main where

import Text.Pandoc.Codecs (jsonSchemaForOpt)
import qualified Data.ByteString.Lazy.Char8 as LBC8

main :: IO ()
main =  do
  LBC8.putStrLn jsonSchemaForOpt
