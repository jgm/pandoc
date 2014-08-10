import System.FilePath
import Text.Parsec
import Data.Char
import System.Environment
import Control.Applicative hiding (many)
import Data.List

main :: IO ()
main = (head <$> getArgs) >>= parseUnicodeMapping


parseUnicodeMapping :: FilePath -> IO ()
parseUnicodeMapping fname = do
  fin <- readFile fname
  let mapname = dropExtension . takeFileName $ fname
  let res = runParse fin
  let header = "-- Generated from " ++ fname ++ "\n" ++
                mapname ++ " :: [(Char, Char)]\n" ++ mapname ++" =\n  [ "
  let footer = "]"
  writeFile (replaceExtension fname ".hs")
    (header ++ (concat $ intersperse "\n  , " (map show res)) ++ footer)

type Unicode = Char

runParse :: String -> [(Char, Unicode)]
runParse s=  either (error . show) id (parse parseMap "" s)

anyline = manyTill anyChar newline

getHexChar :: Parsec String () Char
getHexChar = do
  [(c,_)] <- readLitChar . ("\\x" ++) <$> many1 hexDigit
  return c

parseMap :: Parsec String () [(Char, Unicode)]
parseMap = do
  skipMany (char '#' >> anyline)
  many (flip (,) <$> getHexChar <* tab <*> getHexChar <* anyline)


