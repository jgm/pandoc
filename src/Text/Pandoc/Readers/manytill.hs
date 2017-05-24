import Text.Parsec.String (Parser)
import Text.Parsec (parse, ParseError)
import Text.Parsec.Combinator (many1, manyTill)
import Text.Parsec.Char (anyChar, string, noneOf)
import qualified Text.Pandoc.Builder as B (str)
import Text.Pandoc.Builder (Inlines)
import Text.Parsec.Prim (try)

simpleParse :: Parser a -> String -> Either ParseError a
simpleParse p = parse p ""

header' :: Parser String
header' = manyTill anyChar (string "===")

header'' :: Parser [Inlines]
header'' = manyTill (B.str <$> (many1 anyChar)) (string "===")

header''' :: Parser [Inlines]
header''' = manyTill (B.str <$> (many1 (noneOf "="))) (string "===")

headerWithTry' :: Parser String
headerWithTry' = manyTill anyChar (try (string "==="))

headerWithTry'' :: Parser [Inlines]
headerWithTry'' = manyTill (B.str <$> (many1 anyChar)) (try (string "==="))

headerWithTry''' :: Parser [Inlines]
headerWithTry''' = manyTill (B.str <$> (many1 (noneOf "="))) (try (string "==="))
