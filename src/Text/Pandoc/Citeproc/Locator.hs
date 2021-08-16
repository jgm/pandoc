{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Citeproc.Locator
  ( parseLocator )
where
import Citeproc.Types
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

parseLocator :: Locale -> [Inline] -> (Maybe (Text, Text), [Inline])
parseLocator locale inp =
  case parse (pLocatorWords (toLocatorMap locale)) "suffix" $ splitInp inp of
       Right r -> r
       Left _  -> (Nothing, maybeAddComma inp)

splitInp :: [Inline] -> [Inline]
splitInp = splitStrWhen (\c -> isSpace c || (isPunctuation c && c /= ':'))

--
-- Locator parsing
--

type LocatorParser = Parsec [Inline] ()

pLocatorWords :: LocatorMap
              -> LocatorParser (Maybe (Text, Text), [Inline])
pLocatorWords locMap = do
  optional $ pMatchChar "," (== ',')
  optional pSpace
  (la, lo) <- pLocatorDelimited locMap <|> pLocatorIntegrated locMap
  s <- getInput -- rest is suffix
  -- need to trim, otherwise "p. 9" and "9" will have 'different' locators later on
  -- i.e. the first one will be " 9"
  return $
    if T.null la && T.null lo
       then (Nothing, maybeAddComma s)
       else (Just (la, T.strip lo), s)

maybeAddComma :: [Inline] -> [Inline]
maybeAddComma [] = []
maybeAddComma ils@(Space : _) = ils
maybeAddComma ils@(Str t : _)
  | Just (c, _) <- T.uncons t
  , isPunctuation c = ils
maybeAddComma ils = Str "," : Space : ils

pLocatorDelimited :: LocatorMap -> LocatorParser (Text, Text)
pLocatorDelimited locMap = try $ do
  _ <- pMatchChar "{" (== '{')
  skipMany pSpace -- gobble pre-spaces so label doesn't try to include them
  (la, _) <- pLocatorLabelDelimited locMap
  -- we only care about balancing {} and [] (because of the outer [] scope);
  -- the rest can be anything
  let inner = do { t <- anyToken; return (True, stringify t) }
  gs <- many (pBalancedBraces [('{','}'), ('[',']')] inner)
  _ <- pMatchChar "}" (== '}')
  let lo = T.concat $ map snd gs
  return (la, lo)

pLocatorLabelDelimited :: LocatorMap -> LocatorParser (Text, Bool)
pLocatorLabelDelimited locMap
  = pLocatorLabel' locMap lim <|> return ("page", True)
    where
        lim = stringify <$> anyToken

pLocatorIntegrated :: LocatorMap -> LocatorParser (Text, Text)
pLocatorIntegrated locMap = try $ do
  (la, wasImplicit) <- pLocatorLabelIntegrated locMap
  -- if we got the label implicitly, we have presupposed the first one is
  -- going to have a digit, so guarantee that. You _can_ have p. (a)
  -- because you specified it.
  let modifier = if wasImplicit
                    then requireDigits
                    else requireRomansOrDigits
  g <- try $ pLocatorWordIntegrated (not wasImplicit) >>= modifier
  gs <- many (try $ pLocatorWordIntegrated False >>= modifier)
  let lo = T.concat (g:gs)
  return (la, lo)

pLocatorLabelIntegrated :: LocatorMap -> LocatorParser (Text, Bool)
pLocatorLabelIntegrated locMap
  = pLocatorLabel' locMap lim <|> (lookAhead digital >> return ("page", True))
    where
      lim = try $ pLocatorWordIntegrated True >>= requireRomansOrDigits
      digital = try $ pLocatorWordIntegrated True >>= requireDigits

pLocatorLabel' :: LocatorMap -> LocatorParser Text
               -> LocatorParser (Text, Bool)
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
          case M.lookup (T.toCaseFold $ T.strip s) locMap of
            -- try to find a longer one, or return this one
            Just l -> go s <|> return (l, False)
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

splitStrWhen :: (Char -> Bool) -> [Inline] -> [Inline]
splitStrWhen _ [] = []
splitStrWhen p (Str xs : ys) = go (T.unpack xs) ++ splitStrWhen p ys
  where
   go [] = []
   go s = case break p s of
             ([],[])     -> []
             (zs,[])     -> [Str $ T.pack zs]
             ([],w:ws) -> Str (T.singleton w) : go ws
             (zs,w:ws) -> Str (T.pack zs) : Str (T.singleton w) : go ws
splitStrWhen p (x : ys) = x : splitStrWhen p ys

--
-- Locator Map
--

type LocatorMap = M.Map Text Text

toLocatorMap :: Locale -> LocatorMap
toLocatorMap locale =
  foldr go mempty locatorTerms
 where
  go tname locmap =
    case M.lookup tname (localeTerms locale) of
      Nothing -> locmap
      Just ts -> foldr (\x -> M.insert (snd x) tname) locmap ts

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
