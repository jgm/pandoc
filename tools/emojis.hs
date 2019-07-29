-- Script to generate the list of emojis in T.P.Emoji.hs.
-- to run:
-- curl https://raw.githubusercontent.com/github/gemoji/master/db/emoji.json -o emoji.json
-- stack script --resolver lts-13.17 --package aeson --package bytestring --package text --package containers tools/emojis.hs < emoji.json

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Data.Map as M

data Emoji = Emoji Text [Text]
  deriving Show

instance FromJSON Emoji where
    parseJSON = withObject "Emoji" $ \v -> Emoji
        <$> v .: "emoji"
        <*> v .: "aliases"

main :: IO ()
main = do
  bs <- B.getContents
  case eitherDecode bs of
    Left e -> error e
    Right (emoji :: [Emoji]) -> do
      let emojis = M.fromList $
           [(alias, txt) | Emoji txt aliases <- emoji, alias <- aliases]
      putStrLn $ prettify $ dropWhile (/='[') $ show emojis

prettify :: String -> String
prettify [] = ""
prettify ('[':xs) = '\n':' ':' ':'[':prettify xs
prettify (']':xs) = '\n':' ':' ':']':prettify xs
prettify (',':'(':xs) = '\n':' ':' ':',':'(':prettify xs
prettify (x:xs) = x:prettify xs
