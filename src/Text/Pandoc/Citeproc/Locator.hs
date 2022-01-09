{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Citeproc.Locator
  ( parseLocator
  , toLocatorMap
  , LocatorInfo(..)
  , LocatorMap(..) )
where
import Citeproc.Types
import Text.Pandoc.Citeproc.Util (splitStrWhen)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (foldl')
import Text.Parsec
import Text.Pandoc.Definition
import Text.Pandoc.Parsing (romanNumeral)
import Text.Pandoc.Shared (stringify)
import Control.Monad (mzero)
import qualified Data.Map as M
import Data.Char (isSpace, isPunctuation, isDigit)


data LocatorInfo =
  LocatorInfo{ locatorRaw :: Text
             , locatorLabel :: Text
             , locatorLoc :: Text
             }
  deriving (Show)

parseLocator :: LocatorMap -> [Inline] -> (Maybe LocatorInfo, [Inline])
parseLocator locmap inp =
  case parse (pLocatorWords locmap) "suffix" $ splitInp inp of
       Right r -> r
       Left _  -> (Nothing, maybeAddComma inp)

splitInp :: [Inline] -> [Inline]
splitInp = splitStrWhen (\c -> isSpace c || (isPunctuation c && c /= ':'))

--
-- Locator parsing
--

type LocatorParser = Parsec [Inline] ()

pLocatorWords :: LocatorMap
              -> LocatorParser (Maybe LocatorInfo, [Inline])
pLocatorWords locMap = do
  optional $ pMatchChar "," (== ',')
  optional pSpace
  info <- pLocatorDelimited locMap <|> pLocatorIntegrated locMap
  s <- getInput -- rest is suffix
  return $
    if T.null (locatorLabel info) && T.null (locatorLoc info)
       then (Nothing, maybeAddComma s)
       else (Just info, s)

maybeAddComma :: [Inline] -> [Inline]
maybeAddComma [] = []
maybeAddComma ils@(Space : _) = ils
maybeAddComma ils@(Str t : _)
  | Just (c, _) <- T.uncons t
  , isPunctuation c = ils
maybeAddComma ils = Str "," : Space : ils

pLocatorDelimited :: LocatorMap -> LocatorParser LocatorInfo
pLocatorDelimited locMap = try $ do
  _ <- pMatchChar "{" (== '{')
  skipMany pSpace -- gobble pre-spaces so label doesn't try to include them
  (rawlab, la, _) <- pLocatorLabelDelimited locMap
  -- we only care about balancing {} and [] (because of the outer [] scope);
  -- the rest can be anything
  let inner = do { t <- anyToken; return (True, stringify t) }
  gs <- many (pBalancedBraces [('{','}'), ('[',']')] inner)
  _ <- pMatchChar "}" (== '}')
  let lo = T.concat $ map snd gs
  return $ LocatorInfo{ locatorLoc = lo,
                        locatorLabel = la,
                        locatorRaw = rawlab <> "{" <> lo <> "}" }

pLocatorLabelDelimited :: LocatorMap -> LocatorParser (Text, Text, Bool)
pLocatorLabelDelimited locMap
  = pLocatorLabel' locMap lim <|> return ("", "page", True)
    where
        lim = stringify <$> anyToken

pLocatorIntegrated :: LocatorMap -> LocatorParser LocatorInfo
pLocatorIntegrated locMap = try $ do
  (rawlab, la, wasImplicit) <- pLocatorLabelIntegrated locMap
  -- if we got the label implicitly, we have presupposed the first one is
  -- going to have a digit, so guarantee that. You _can_ have p. (a)
  -- because you specified it.
  let modifier = if wasImplicit
                    then requireDigits
                    else requireRomansOrDigits
  g <- try $ pLocatorWordIntegrated (not wasImplicit) >>= modifier
  gs <- many (try $ pLocatorWordIntegrated False >>= modifier)
  let lo = T.concat (g:gs)
  return $ LocatorInfo{ locatorLabel = la,
                        locatorLoc = lo,
                        locatorRaw = rawlab <> lo }

pLocatorLabelIntegrated :: LocatorMap -> LocatorParser (Text, Text, Bool)
pLocatorLabelIntegrated locMap
  = pLocatorLabel' locMap lim <|>
     (lookAhead digital >> return ("", "page", True))
    where
      lim = try $ pLocatorWordIntegrated True >>= requireRomansOrDigits
      digital = try $ pLocatorWordIntegrated True >>= requireDigits

pLocatorLabel' :: LocatorMap -> LocatorParser Text
               -> LocatorParser (Text, Text, Bool)
pLocatorLabel' locMap lim = go ""
    where
      -- grow the match string until we hit the end
      -- trying to find the largest match for a label
      go acc = try $ do
          -- advance at least one token each time
          -- the pathological case is "p.3"
          t <- anyToken
          ts <- manyTill anyToken (try $ lookAhead lim)
          let s = acc <> stringify (t:ts)
          case M.lookup (T.toCaseFold $ T.strip s) (unLocatorMap locMap) of
            -- try to find a longer one, or return this one
            Just l -> go s <|> return (s, l, False)
            Nothing -> go s

-- hard requirement for a locator to have some real digits in it
requireDigits :: (Bool, Text) -> LocatorParser Text
requireDigits (_, s) = if not (T.any isDigit s)
                          then Prelude.fail "requireDigits"
                          else return s

-- soft requirement for a sequence with some roman or arabic parts
-- (a)(iv) -- because iv is roman
-- 1(a)  -- because 1 is an actual digit
-- NOT: a, (a)-(b), hello, (some text in brackets)
requireRomansOrDigits :: (Bool, Text) -> LocatorParser Text
requireRomansOrDigits (d, s) = if not d
                                  then Prelude.fail "requireRomansOrDigits"
                                  else return s

pLocatorWordIntegrated :: Bool -> LocatorParser (Bool, Text)
pLocatorWordIntegrated isFirst = try $ do
  punct <- if isFirst
              then return ""
              else (stringify <$> pLocatorSep) <|> return ""
  sp <- option "" (pSpace >> return " ")
  (dig, s) <- pBalancedBraces [('(',')'), ('[',']'), ('{','}')] pPageSeq
  return (dig, punct <> sp <> s)

-- we want to capture:  123, 123A, C22, XVII, 33-44, 22-33; 22-11
--                      34(1), 34A(A), 34(1)(i)(i), (1)(a)
--                      [17], [17]-[18], '591 [84]'
--                      (because CSL cannot pull out individual pages/sections
--                      to wrap in braces on a per-style basis)
pBalancedBraces :: [(Char, Char)]
                -> LocatorParser (Bool, Text)
                -> LocatorParser (Bool, Text)
pBalancedBraces braces p = try $ do
  ss <- many1 surround
  return $ anyWereDigitLike ss
  where
      except = notFollowedBy pBraces >> p
      -- outer and inner
      surround = foldl' (\a (open, close) -> sur open close except <|> a)
                       except
                       braces

      isc c = stringify <$> pMatchChar [c] (== c)

      sur c c' m = try $ do
          (d, mid) <- between (isc c) (isc c') (option (False, "") m)
          return (d, T.cons c . flip T.snoc c' $  mid)

      flattened = concatMap (\(o, c) -> [o, c]) braces
      pBraces = pMatchChar "braces" (`elem` flattened)


-- YES 1, 1.2, 1.2.3
-- NO  1., 1.2. a.6
-- can't use sepBy because we want to leave trailing .s
pPageSeq :: LocatorParser (Bool, Text)
pPageSeq = oneDotTwo <|> withPeriod
  where
      oneDotTwo = do
          u <- pPageUnit
          us <- many withPeriod
          return $ anyWereDigitLike (u:us)
      withPeriod = try $ do
          -- .2
          p <- pMatchChar "." (== '.')
          u <- try pPageUnit
          return (fst u, stringify p <> snd u)

anyWereDigitLike :: [(Bool, Text)] -> (Bool, Text)
anyWereDigitLike as = (any fst as, T.concat $ map snd as)

pPageUnit :: LocatorParser (Bool, Text)
pPageUnit = roman <|> plainUnit
  where
      -- roman is a 'digit'
      roman = (True,) <$> pRoman
      plainUnit = do
          ts <- many1 (notFollowedBy pSpace >>
                       notFollowedBy pLocatorPunct >>
                       notFollowedBy pMath >>
                       anyToken)
          let s = stringify ts
          -- otherwise look for actual digits or -s
          return (T.any isDigit s, s)

pRoman :: LocatorParser Text
pRoman = try $ do
  tok <- anyToken
  case tok of
       Str t -> case parse (romanNumeral True *> eof)
                   "roman numeral" (T.toUpper t) of
                      Left _    -> mzero
                      Right ()  -> return t
       _      -> mzero

pLocatorPunct :: LocatorParser Inline
pLocatorPunct = pMatchChar "punctuation" isLocatorPunct

pLocatorSep :: LocatorParser Inline
pLocatorSep = pMatchChar "locator separator" isLocatorSep

pMatchChar :: String -> (Char -> Bool) -> LocatorParser Inline
pMatchChar msg f = satisfyTok f' <?> msg
    where
        f' (Str (T.unpack -> [c])) = f c
        f' _                       = False

pSpace :: LocatorParser Inline
pSpace = satisfyTok (\t -> isSpacey t || t == Str "\160") <?> "space"

pMath :: LocatorParser Inline
pMath = satisfyTok isMath
 where
  isMath (Math{}) = True
  isMath _ = False

satisfyTok :: (Inline -> Bool) -> LocatorParser Inline
satisfyTok f = tokenPrim show (\sp _ _ -> sp) (\tok -> if f tok
                                                          then Just tok
                                                          else Nothing)

isSpacey :: Inline -> Bool
isSpacey Space     = True
isSpacey SoftBreak = True
isSpacey _         = False

isLocatorPunct :: Char -> Bool
isLocatorPunct '-' = False -- page range
isLocatorPunct '–' = False -- page range, en dash
isLocatorPunct ':' = False -- vol:page-range hack
isLocatorPunct c   = isPunctuation c -- includes [{()}]

isLocatorSep :: Char -> Bool
isLocatorSep ',' = True
isLocatorSep ';' = True
isLocatorSep _   = False

--
-- Locator Map
--

newtype LocatorMap = LocatorMap { unLocatorMap :: M.Map Text Text }
  deriving (Show)

toLocatorMap :: Locale -> LocatorMap
toLocatorMap locale =
  LocatorMap $ foldr go mempty locatorTerms
 where
  go tname locmap =
    case M.lookup tname (localeTerms locale) of
      Nothing -> locmap
      Just ts -> foldr (\x -> M.insert (T.toCaseFold $ snd x) tname) locmap ts
-- we store keys in "case-folded" (lowercase) form, so that both
-- "Chap." and "chap." will match, for example.

locatorTerms :: [Text]
locatorTerms =
  [ "book"
  , "chapter"
  , "column"
  , "figure"
  , "folio"
  , "issue"
  , "line"
  , "note"
  , "opus"
  , "page"
  , "number-of-pages"
  , "paragraph"
  , "part"
  , "section"
  , "sub verbo"
  , "verse"
  , "volume" ]
