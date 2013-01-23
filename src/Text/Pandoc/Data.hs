{-# LANGUAGE TemplateHaskell #-}
module Text.Pandoc.Data (dataFiles) where
import Data.FileEmbed
import qualified Data.ByteString as B

dataFiles :: [(FilePath, B.ByteString)]
dataFiles = $(embedDir "data")
