{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Readers.Docx.EqField (eqToTeX) where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec hiding (lower, upper)
import Text.Parsec.Text (Parser)

-- ========================================
-- AST Definition
-- ========================================

data EqExpr
  = Array ArrayOpts [EqExpr]
  | Bracket BracketOpts EqExpr
  | Displace DisplaceOpts
  | Fraction EqExpr EqExpr
  | Integral IntegralOpts (Maybe EqExpr) (Maybe EqExpr) (Maybe EqExpr)
  | List [EqExpr]
  | Overstrike OverstrikeOpts [EqExpr]
  | Radical (Maybe EqExpr) EqExpr
  | Script ScriptOpts EqExpr
  | Box BoxOpts EqExpr
  | Text Text
  | Composite [EqExpr] -- For combining multiple expressions
  deriving (Show, Eq)

data ArrayOpts = ArrayOpts
  { arrayAlign :: Alignment,
    arrayCols :: Int,
    arrayVSpace :: Maybe Int,
    arrayHSpace :: Maybe Int
  }
  deriving (Show, Eq)

data Alignment = AlignLeft | AlignCenter | AlignRight
  deriving (Show, Eq)

data BracketOpts = BracketOpts
  { leftBracket :: Text,
    rightBracket :: Text
  }
  deriving (Show, Eq)

data DisplaceOpts = DisplaceOpts
  { displaceForward :: Maybe Int,
    displaceBackward :: Maybe Int,
    displaceLine :: Bool
  }
  deriving (Show, Eq)

data IntegralOpts = IntegralOpts
  { integralType :: IntegralType,
    integralInline :: Bool,
    integralCustom :: Maybe Text
  }
  deriving (Show, Eq)

data IntegralType = IntType | SumType | ProductType | CustomType Text
  deriving (Show, Eq)

data OverstrikeOpts = OverstrikeOpts
  { overstrikeAlign :: OverstrikeAlignment
  }
  deriving (Show, Eq)

data OverstrikeAlignment = OLeft | OCenter | ORight
  deriving (Show, Eq)

data ScriptOpts = ScriptOpts
  { scriptType :: ScriptType,
    scriptOffset :: Maybe Int
  }
  deriving (Show, Eq)

data ScriptType = Superscript | Subscript | Both
  deriving (Show, Eq)

data BoxOpts = BoxOpts
  { boxTop :: Bool,
    boxBottom :: Bool,
    boxLeft :: Bool,
    boxRight :: Bool
  }
  deriving (Show, Eq)

-- ========================================
-- Parser
-- ========================================

-- Main parser entry point
parseEqField :: Text -> Either ParseError EqExpr
parseEqField input = parse eqFieldParser "" (preprocessInput input)

-- Preprocess input to handle special cases
preprocessInput :: Text -> Text
preprocessInput = id -- For now, just return as-is

-- Main Eq field parser
eqFieldParser :: Parser EqExpr
eqFieldParser = do
  spaces
  optional (try (string "Eq" >> spaces))
  optional (char '{' >> spaces)
  expr <- eqExpression
  optional (spaces >> char '}')
  spaces
  eof
  return expr

-- Parse main Eq expression
eqExpression :: Parser EqExpr
eqExpression = try compositeExpr <|> singleExpr
  where
    compositeExpr = do
      exprs <- many1 (try singleExpr)
      return $ case exprs of
        [e] -> e
        es -> Composite es

-- Parse single expression
singleExpr :: Parser EqExpr
singleExpr =
  choice
    [ try arrayExpr,
      try bracketExpr,
      try fractionExpr,
      try integralExpr,
      try listExpr,
      try overstrikeExpr,
      try radicalExpr,
      try scriptExpr,
      try boxExpr,
      try textExpr
    ]

-- Parse array expression
arrayExpr :: Parser EqExpr
arrayExpr = do
  char '\\' >> char 'a'
  opts <- parseArrayOpts
  spaces
  elements <- parseParenList
  return $ Array opts elements

parseArrayOpts :: Parser ArrayOpts
parseArrayOpts = do
  options <- many (try parseArrayOption)
  let alignOpt = lookup "align" options
  let align = case alignOpt of
        Just "al" -> AlignLeft
        Just "ac" -> AlignCenter
        Just "ar" -> AlignRight
        _ -> AlignLeft
  let cols = fromMaybe 1 $ lookup "co" options >>= readMaybeText
  let vspace = lookup "vs" options >>= readMaybeText
  let hspace = lookup "hs" options >>= readMaybeText
  return $ ArrayOpts align cols vspace hspace
  where
    parseArrayOption :: Parser (Text, Text)
    parseArrayOption = do
      spaces
      char '\\'
      choice
        [ do
            opt <- string "al" <|> string "ac" <|> string "ar"
            return ("align", T.pack opt),
          do
            string "co"
            digits <- many1 digit
            return ("co", T.pack digits),
          do
            string "vs"
            digits <- many1 digit
            return ("vs", T.pack digits),
          do
            string "hs"
            digits <- many1 digit
            return ("hs", T.pack digits)
        ]

-- Parse bracket expression
bracketExpr :: Parser EqExpr
bracketExpr = do
  char '\\' >> char 'b'
  opts <- parseBracketOpts
  spaces
  content <- parseParenContent
  return $ Bracket opts content

parseBracketOpts :: Parser BracketOpts
parseBracketOpts = do
  options <- many (try parseBracketOption)
  let (left, right) = determineBrackets options
  return $ BracketOpts left right
  where
    parseBracketOption :: Parser (Text, Char)
    parseBracketOption = do
      spaces
      char '\\'
      choice
        [ do
            _ <- string "lc"
            _ <- char '\\'
            c <- anyChar
            return ("lc", c),
          do
            _ <- string "rc"
            _ <- char '\\'
            c <- anyChar
            return ("rc", c),
          do
            _ <- string "bc"
            _ <- char '\\'
            c <- anyChar
            return ("bc", c)
        ]

    determineBrackets opts =
      let lcOpt = lookup "lc" opts
          rcOpt = lookup "rc" opts
          bcOpt = lookup "bc" opts
       in case (bcOpt, lcOpt, rcOpt) of
            (Just c, _, _) -> (bracketToTeX c, bracketToTeX c)
            (_, Just lc, Just rc) -> (bracketToTeX lc, bracketToTeX rc)
            (_, Just lc, _) -> (bracketToTeX lc, ".")
            (_, _, Just rc) -> (".", bracketToTeX rc)
            _ -> ("(", ")")
    
    bracketToTeX '　' = "."
    bracketToTeX ' ' = "."
    bracketToTeX c = charToTeX c

    charToTeX '{' = "\\{"
    charToTeX '}' = "\\}"
    charToTeX '[' = "["
    charToTeX ']' = "]"
    charToTeX '(' = "("
    charToTeX ')' = ")"
    charToTeX '<' = "\\langle"
    charToTeX '>' = "\\rangle"
    charToTeX c = T.singleton c

-- Parse fraction expression
fractionExpr :: Parser EqExpr
fractionExpr = do
  char '\\' >> char 'f'
  spaces
  char '('
  spaces
  num <- parseArgument
  spaces
  separator
  spaces
  den <- parseArgument
  spaces
  char ')'
  return $ Fraction num den

-- Parse integral expression
integralExpr :: Parser EqExpr
integralExpr = do
  char '\\' >> char 'i'
  opts <- parseIntegralOpts
  spaces
  args <- parseParenList
  let (lower, upper, integrand) = case args of
        [l, u, i] -> (Just l, Just u, Just i)
        [l, u] -> (Just l, Just u, Nothing)
        _ -> (Nothing, Nothing, Nothing)
  return $ Integral opts lower upper integrand

parseIntegralOpts :: Parser IntegralOpts
parseIntegralOpts = do
  options <- many (try parseIntegralOption)
  let itype =
        if "su" `elem` options
          then SumType
          else
            if "pr" `elem` options
              then ProductType
              else IntType
  let inline = "in" `elem` options
  return $ IntegralOpts itype inline Nothing
  where
    parseIntegralOption =
      spaces
        >> char '\\'
        >> (string "su" <|> string "pr" <|> string "in")

-- Parse list expression
listExpr :: Parser EqExpr
listExpr = do
  char '\\' >> char 'l'
  spaces
  elements <- parseParenList
  return $ List elements

-- Parse overstrike expression
overstrikeExpr :: Parser EqExpr
overstrikeExpr = do
  char '\\' >> char 'o'
  opts <- parseOverstrikeOpts
  spaces
  elements <- parseParenList
  return $ Overstrike opts elements

parseOverstrikeOpts :: Parser OverstrikeOpts
parseOverstrikeOpts = do
  options <- many (try parseOverstrikeOption)
  let align =
        if "al" `elem` options
          then OLeft
          else
            if "ar" `elem` options
              then ORight
              else OCenter
  return $ OverstrikeOpts align
  where
    parseOverstrikeOption =
      spaces
        >> char '\\'
        >> (string "al" <|> string "ac" <|> string "ar")

-- Parse radical expression
radicalExpr :: Parser EqExpr
radicalExpr = do
  char '\\' >> char 'r'
  spaces
  char '('
  spaces
  first <- parseArgument
  second <- optionMaybe (try $ separator >> spaces >> parseArgument)
  spaces
  char ')'
  case second of
    Nothing -> return $ Radical Nothing first
    Just s -> return $ Radical (Just first) s

-- Parse script expression
scriptExpr :: Parser EqExpr
scriptExpr = do
  char '\\' >> char 's'
  opts <- parseScriptOpts
  spaces
  content <- parseParenContentOrSingle
  return $ Script opts content

parseScriptOpts :: Parser ScriptOpts
parseScriptOpts = do
  options <- many (try parseScriptOption)
  let stype =
        if any (isUpOption . fst) options
          then Superscript
          else
            if any (isDownOption . fst) options
              then Subscript
              else Both
  let offset =
        listToMaybe
          [ read (T.unpack n) | (opt, n) <- options, isUpOption opt || isDownOption opt, not (T.null n)
          ]
  return $ ScriptOpts stype offset
  where
    parseScriptOption =
      spaces
        >> char '\\'
        >> ( (,)
               <$> (string "up" <|> string "do" <|> string "ai" <|> string "di")
               <*> (T.pack <$> many digit)
           )
    isUpOption = (`elem` ["up", "ai"])
    isDownOption = (`elem` ["do", "di"])
    listToMaybe [] = Nothing
    listToMaybe (x : _) = Just x

-- Parse box expression
boxExpr :: Parser EqExpr
boxExpr = do
  char '\\' >> char 'x'
  opts <- parseBoxOpts
  spaces
  content <- parseParenContent
  return $ Box opts content

parseBoxOpts :: Parser BoxOpts
parseBoxOpts = do
  options <- many (try parseBoxOption)
  let top = "to" `elem` options
  let bottom = "bo" `elem` options
  let left = "le" `elem` options
  let right = "ri" `elem` options
  let allSides = null options
  return $
    if allSides
      then BoxOpts True True True True
      else BoxOpts top bottom left right
  where
    parseBoxOption =
      spaces
        >> char '\\'
        >> (string "to" <|> string "bo" <|> string "le" <|> string "ri")

-- Parse text (fallback)
textExpr :: Parser EqExpr
textExpr = Text . T.pack <$> many1 (noneOf "\\(),;")

-- Helper parsers
parseParenContent :: Parser EqExpr
parseParenContent = do
  char '('
  spaces
  content <- eqExpression
  spaces
  char ')'
  return content

parseParenContentOrSingle :: Parser EqExpr
parseParenContentOrSingle = try parseParenContent <|> parseSingleArg
  where
    parseSingleArg = do
      char '('
      spaces
      arg <- parseArgument
      spaces
      char ')'
      return arg

parseParenList :: Parser [EqExpr]
parseParenList = do
  char '('
  spaces
  args <- parseArgument `sepBy` separator
  spaces
  char ')'
  return args

parseArgument :: Parser EqExpr
parseArgument = try eqExpression <|> textArgument
  where
    textArgument = Text . T.pack <$> many1 (noneOf "(),;\\")

separator :: Parser ()
separator = void (oneOf ",;，") >> spaces

-- ========================================
-- TeX Generator
-- ========================================

toTeX :: EqExpr -> Text
toTeX (Array opts elements) = generateArray opts elements
toTeX (Bracket opts content) = generateBracket opts content
toTeX (Fraction num den) = "\\frac{" <> toTeX num <> "}{" <> toTeX den <> "}"
toTeX (Integral opts lower upper integrand) = generateIntegral opts lower upper integrand
toTeX (List elements) = T.intercalate "," (map toTeX elements)
toTeX (Overstrike opts elements) = generateOverstrike opts elements
toTeX (Radical Nothing expr) = "\\sqrt{" <> toTeX expr <> "}"
toTeX (Radical (Just n) expr) = "\\sqrt[" <> toTeX n <> "]{" <> toTeX expr <> "}"
toTeX (Script opts content) = generateScript opts content
toTeX (Box opts content) = generateBox opts content
toTeX (Text s) = processText s
toTeX (Composite exprs) = T.concat (map toTeX exprs)
toTeX _ = ""

generateArray :: ArrayOpts -> [EqExpr] -> Text
generateArray opts elements =
  let alignChar = case arrayAlign opts of
        AlignLeft -> 'l'
        AlignCenter -> 'c'
        AlignRight -> 'r'
      cols = arrayCols opts
      colSpec = T.replicate cols (T.singleton alignChar)
      rows = chunksOf cols elements
      rowsStr = map (T.intercalate " & " . map toTeX) rows
      content = T.intercalate " \\\\\n" rowsStr
   in "\\begin{array}{" <> colSpec <> "}\n" <> content <> "\n\\end{array}"

generateBracket :: BracketOpts -> EqExpr -> Text
generateBracket opts content =
  "\\left" <> leftBracket opts <> toTeX content <> "\\right" <> rightBracket opts

generateIntegral :: IntegralOpts -> Maybe EqExpr -> Maybe EqExpr -> Maybe EqExpr -> Text
generateIntegral opts lower upper integrand =
  let symbol = case integralType opts of
        IntType -> "\\int"
        SumType -> "\\sum"
        ProductType -> "\\prod"
        CustomType s -> s
      lowerStr = maybe "" (\l -> "_{" <> toTeX l <> "}") lower
      upperStr = maybe "" (\u -> "^{" <> toTeX u <> "}") upper
      integrandStr = maybe "" (\i -> " " <> toTeX i) integrand
   in symbol <> lowerStr <> upperStr <> integrandStr

generateOverstrike :: OverstrikeOpts -> [EqExpr] -> Text
generateOverstrike _ [base, Text "→"] = "\\vec{" <> toTeX base <> "}"
generateOverstrike _ [base, Text "-"] = "\\overline{" <> toTeX base <> "}"
generateOverstrike _ [base, Text "^"] = "\\hat{" <> toTeX base <> "}"
generateOverstrike _ (base : rest) =
  foldl (\acc e -> "\\overset{" <> toTeX e <> "}{" <> acc <> "}") (toTeX base) rest
generateOverstrike _ [] = ""

generateScript :: ScriptOpts -> EqExpr -> Text
generateScript opts content =
  case scriptType opts of
    Superscript -> "^{" <> toTeX content <> "}"
    Subscript -> "_{" <> toTeX content <> "}"
    Both -> toTeX content -- Handle stacked scripts differently

generateBox :: BoxOpts -> EqExpr -> Text
generateBox opts content
  | boxTop opts && boxBottom opts && boxLeft opts && boxRight opts =
      "\\boxed{" <> toTeX content <> "}"
  | boxTop opts = "\\overline{" <> toTeX content <> "}"
  | boxBottom opts = "\\underline{" <> toTeX content <> "}"
  | otherwise = toTeX content

processText :: Text -> Text
processText t
  | T.null t = ""
  | otherwise = escapeChar (T.head t) <> processText (T.tail t)

escapeChar :: Char -> Text
escapeChar '{' = "\\{"
escapeChar '}' = "\\}"
escapeChar '＝' = "="
escapeChar '＜' = "<"
escapeChar '＞' = ">"
escapeChar '≥' = "\\ge"
escapeChar '≤' = "\\le"
escapeChar '≠' = "\\ne"
escapeChar '≈' = "\\approx"
escapeChar '～' = "\\sim"
escapeChar '≡' = "\\equiv"
escapeChar '∝' = "\\propto"
escapeChar '，' = ","
escapeChar '＋' = "+"
escapeChar '－' = "+"
escapeChar '×' = "\\times"
escapeChar '÷' = "\\div"
escapeChar '⋅' = "\\cdot"
escapeChar '…' = "\\cdots"
escapeChar '（' = "("
escapeChar '）' = ")"
escapeChar 'α' = "\\alpha"
escapeChar 'β' = "\\beta"
escapeChar 'Γ' = "\\Gamma"
escapeChar 'γ' = "\\gamma"
escapeChar 'Δ' = "\\Delta"
escapeChar 'δ' = "\\delta"
escapeChar 'ϵ' = "\\epsilon"
escapeChar 'ε' = "\\varepsilon"
escapeChar 'ζ' = "\\zeta"
escapeChar 'η' = "\\eta"
escapeChar 'Θ' = "\\Theta"
escapeChar 'θ' = "\\theta"
escapeChar 'ϑ' = "\\vartheta"
escapeChar 'ι' = "\\iota"
escapeChar 'κ' = "\\kappa"
escapeChar 'Λ' = "\\Lambda"
escapeChar 'λ' = "\\lambda"
escapeChar 'μ' = "\\mu"
escapeChar 'ν' = "\\nu"
escapeChar 'Ξ' = "\\Xi"
escapeChar 'ξ' = "\\xi"
escapeChar 'Π' = "\\Pi"
escapeChar 'π' = "\\pi"
escapeChar 'ϖ' = "\\varpi"
escapeChar 'ϱ' = "\\varrho"
escapeChar 'ρ' = "\\rho"
escapeChar 'Σ' = "\\Sigma"
escapeChar 'σ' = "\\sigma"
escapeChar 'ς' = "\\varsigma"
escapeChar 'τ' = "\\tau"
escapeChar 'Υ' = "\\Upsilon"
escapeChar 'υ' = "\\upsilon"
escapeChar 'Φ' = "\\Phi"
escapeChar 'ϕ' = "\\phi"
escapeChar 'φ' = "\\varphi"
escapeChar 'χ' = "\\chi"
escapeChar 'Ψ' = "\\Psi"
escapeChar 'ψ' = "\\psi"
escapeChar 'Ω' = "\\Omega"
escapeChar 'ω' = "\\omega"
escapeChar c = T.singleton c

-- Helper functions
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

readMaybeText :: Read a => Text -> Maybe a
readMaybeText s = case reads (T.unpack s) of
  [(x, "")] -> Just x
  _ -> Nothing

-- ========================================
-- Main conversion function
-- ========================================

eqToTeX :: Text -> Either Text Text
eqToTeX input =
  case parseEqField input of
    Left err -> Left $ T.pack $ show err
    Right expr -> Right $ toTeX expr
