{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.LaTeX.Citation
  ( citationCommands
  , cites
  )
where

import Text.Pandoc.Class
import Text.Pandoc.Readers.LaTeX.Parsing
import Text.Pandoc.Builder as B
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import Control.Applicative ((<|>), optional, many)
import Control.Monad (mzero)
import Control.Monad.Trans (lift)
import Control.Monad.Except (throwError)
import Text.Pandoc.Error (PandocError(PandocParsecError))
import Text.Pandoc.Parsing hiding (blankline, many, mathDisplay, mathInline,
                            optional, space, spaces, withRaw, (<|>))
import Text.Pandoc.Readers.LaTeX.Types (Tok)

citationCommands :: PandocMonad m => LP m Inlines -> M.Map Text (LP m Inlines)
citationCommands inline =
  let citation = citationWith inline
      tok = spaces *> grouped inline
      multipleCites = multiCites inline
   in M.fromList
  [ ("cite", citation "cite" NormalCitation False)
  , ("Cite", citation "Cite" NormalCitation False)
  , ("citep", citation "citep" NormalCitation False)
  , ("citep*", citation "citep*" NormalCitation False)
  , ("citeal", citation "citeal" NormalCitation False)
  , ("citealp", citation "citealp" NormalCitation False)
  , ("citealp*", citation "citealp*" NormalCitation False)
  , ("autocite", citation "autocite" NormalCitation False)
  , ("smartcite", citation "smartcite" NormalCitation False)
  , ("footcite", inNote <$> citation "footcite" NormalCitation False)
  , ("parencite", citation "parencite" NormalCitation False)
  , ("supercite", citation "supercite" NormalCitation False)
  , ("footcitetext", inNote <$> citation "footcitetext" NormalCitation False)
  , ("citeyearpar", citation "citeyearpar" SuppressAuthor False)
  , ("citeyear", citation "citeyear" SuppressAuthor False)
  , ("autocite*", citation "autocite*" SuppressAuthor False)
  , ("cite*", citation "cite*" SuppressAuthor False)
  , ("parencite*", citation "parencite*" SuppressAuthor False)
  , ("textcite", multipleCites "textcite" AuthorInText True)
  , ("citet", citation "citet" AuthorInText False)
  , ("citet*", citation "citet*" AuthorInText False)
  , ("citealt", citation "citealt" AuthorInText False)
  , ("citealt*", citation "citealt*" AuthorInText False)
  , ("textcites", citation "textcites" AuthorInText True)
  , ("cites", citation "cites" NormalCitation True)
  , ("autocites", citation "autocites" NormalCitation True)
  , ("footcites", inNote <$> citation "footcites" NormalCitation True)
  , ("parencites", citation "parencites" NormalCitation True)
  , ("supercites", citation "supercites" NormalCitation True)
  , ("footcitetexts", inNote <$> citation "footcitetexts" NormalCitation True)
  , ("Autocite", citation "Autocite" NormalCitation False)
  , ("Smartcite", citation "Smartcite" NormalCitation False)
  , ("Footcite", inNote <$> citation "Footcite" NormalCitation False)
  , ("Parencite", citation "Parencite" NormalCitation False)
  , ("Supercite", citation "Supercite" NormalCitation False)
  , ("Footcitetext", inNote <$> citation "Footcitetext" NormalCitation False)
  , ("Citeyearpar", citation "Citeyearpar" SuppressAuthor False)
  , ("Citeyear", citation "Citeyear" SuppressAuthor False)
  , ("Autocite*", citation "Autocite*" SuppressAuthor False)
  , ("Cite*", citation "Cite*" SuppressAuthor False)
  , ("Parencite*", citation "Parencite*" SuppressAuthor False)
  , ("Textcite", citation "Textcite" AuthorInText False)
  , ("Textcites", citation "Textcites" AuthorInText True)
  , ("Cites", citation "Cites" NormalCitation True)
  , ("Autocites", citation "Autocites" NormalCitation True)
  , ("Footcites", inNote <$> citation "Footcites" NormalCitation True)
  , ("Parencites", citation "Parencites" NormalCitation True)
  , ("Supercites", citation "Supercites" NormalCitation True)
  , ("Footcitetexts", inNote <$> citation "Footcitetexts" NormalCitation True)
  , ("citetext", complexNatbibCitation inline NormalCitation)
  , ("citeauthor", (try (tok *> sp *> controlSeq "citetext") *>
                        complexNatbibCitation inline AuthorInText)
                   <|> citation "citeauthor" AuthorInText False)
  , ("nocite", mempty <$ (citation "nocite" NormalCitation False >>=
                          addMeta "nocite"))
  ]

-- citations

addPrefix :: (Text, [Inline]) -> [(Citation, Text)] -> [(Citation, Text)]
addPrefix (_, p) ((k, raw):ks) = (k {citationPrefix = p ++ citationPrefix k}, raw) : ks
addPrefix _ _      = []

addSuffix :: (Text, [Inline]) -> [(Citation, Text)] -> [(Citation, Text)]
addSuffix (foo, s) ks@(_:_) =
  let (k, raw) = last ks
      stuff = if foo == "" then "" else "[" <> foo <> "]"
  in  init ks ++ [(k {citationSuffix = citationSuffix k ++ s}, stuff <> raw)]
addSuffix _ _ = []

simpleCiteArgs :: forall m . PandocMonad m => LP m Inlines -> LP m [(Citation, Text)]
simpleCiteArgs inline = try $ do
  first  <- optionMaybe $ fmap toList <$> opt
  second <- optionMaybe $ fmap toList <$> opt
  keys <- try $ bgroup *> manyTill citationLabel egroup
  let (pre, suf) = case (first  , second ) of
        (Just s , Nothing) -> (mempty, s )
        (Just s , Just t ) -> (s , t )
        _                  -> (mempty, mempty)
      conv k = (Citation { citationId      = k
                        , citationPrefix  = []
                        , citationSuffix  = []
                        , citationMode    = NormalCitation
                        , citationHash    = 0
                        , citationNoteNum = 0
                        }, "{" <> k <> "}")
  return $ addPrefix pre $ addSuffix suf $ map conv keys
 where
  opt :: PandocMonad m => LP m (Text, Inlines)
  opt = do
    toks <- try (sp *> bracketedToks <* sp)
    -- now parse the toks as inlines
    st <- getState
    parsed <- lift $
      runParserT (mconcat <$> many inline) st "bracketed option"
       (TokStream False toks)
    case parsed of
      Right result -> return (untokenize toks, result)
      Left e       -> throwError $ PandocParsecError (toSources toks) e



citationLabel :: PandocMonad m => LP m Text
citationLabel  = do
  sp
  untokenize <$>
    (many1 (satisfyTok isWordTok <|> symbolIn bibtexKeyChar)
          <* sp
          <* optional (symbol ',')
          <* sp)
  where bibtexKeyChar = ".:;?!`'()/*@_+=-&[]" :: [Char]

cites :: PandocMonad m
      => LP m Inlines -> CitationMode -> Bool -> LP m [(Citation, Text)]
cites inline mode multi = try $ do
  let paropt = parenWrapped inline
  cits <- if multi
             then do
               multiprenote <- optionMaybe $ toList <$> paropt
               multipostnote <- optionMaybe $ toList <$> paropt
               let (pre, suf) = case (multiprenote, multipostnote) of
                     (Just s , Nothing) -> (mempty, s)
                     (Nothing , Just t) -> (mempty, t)
                     (Just s , Just t ) -> (s, t)
                     _                  -> (mempty, mempty)
               tempCits <- many1 $ simpleCiteArgs inline
               case tempCits of
                 (k:ks) -> case ks of
                             (_:_) -> return $ (addMprenote pre k : init ks) ++
                                                 [addMpostnote suf (last ks)]
                             _ -> return [addMprenote pre (addMpostnote suf k)]
                 _ -> return [[]]
             else count 1 $ simpleCiteArgs inline
  let cs = concat cits
  return $ case mode of
        AuthorInText -> case cs of
                             ((c, raw):rest) -> (c {citationMode = mode}, raw) : rest
                             []       -> []
        _            -> map (\(a, raw) -> (a {citationMode = mode}, raw)) cs
  where mprenote (k:ks) = (k:ks) ++ [Space]
        mprenote _ = mempty
        mpostnote (k:ks) = [Str ",", Space] ++ (k:ks)
        mpostnote _ = mempty
        addMprenote mpn (k:ks) =
          let mpnfinal = case citationPrefix (fst k) of
                           (_:_) -> mprenote mpn
                           _ -> mpn
          in addPrefix (mempty, mpnfinal) (k:ks)
        addMprenote _ _ = []
        addMpostnote = addSuffix . (\t -> (mempty, mpostnote t))

citationWith :: PandocMonad m
             => LP m Inlines -> Text -> CitationMode -> Bool -> LP m Inlines
citationWith inline name mode multi = do
  (c, raw) <- withRaw $ cites inline mode multi
  return $ cite (map fst c) (rawLatex name raw)

multiCites :: PandocMonad m
           => LP m Inlines -> Text -> CitationMode -> Bool -> LP m Inlines
multiCites inline name mode multi = do
  (cites, _) <- withRaw $ cites inline mode multi
  return . mconcat . commaSeparated $ toCite <$> cites
  where
    toCite (citation, raw) =
      cite [citation {citationMode = AuthorInText}] (rawInline "latex" $ "\\" <> name <> raw)
    commaSeparated =
      L.intersperse (str "," <> space)

rawLatex :: Text -> [Tok] -> Inlines
rawLatex name raw =
  (rawInline "latex" $ "\\" <> name <> untokenize raw)

handleCitationPart :: Inlines -> [(Citation, Text)]
handleCitationPart ils =
  let isCite Cite{} = True
      isCite _      = False
      (pref, rest) = break isCite (toList ils)
  in case rest of
          (Cite cs _:suff) -> addPrefix (mempty, pref) $ addSuffix (mempty, suff) (map (\x -> (x, mempty)) cs)
          _                -> []

complexNatbibCitation :: PandocMonad m
                      => LP m Inlines -> CitationMode -> LP m Inlines
complexNatbibCitation inline mode = try $ do
  (cs, raw) <-
    withRaw $ concat <$> do
      bgroup
      items <- mconcat <$>
                many1 (notFollowedBy (symbol ';') >> inline)
                  `sepBy1` symbol ';'
      egroup
      return $ map handleCitationPart items
  case cs of
       []       -> mzero
       ((c, r):cits) -> return $ cite (c{ citationMode = mode }:(map fst cits))
                      (rawLatex "citetext" raw)

inNote :: Inlines -> Inlines
inNote ils =
  note $ para $ ils <> str "."
