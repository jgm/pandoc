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
import qualified Data.Map as M
import Data.Text (Text)
import Control.Applicative ((<|>), optional, many)
import Control.Monad (mzero)
import Control.Monad.Trans (lift)
import Control.Monad.Except (throwError)
import Text.Pandoc.Error (PandocError(PandocParsecError))
import Text.Pandoc.Parsing hiding (blankline, many, mathDisplay, mathInline,
                            optional, space, spaces, withRaw, (<|>))

citationCommands :: PandocMonad m => LP m Inlines -> M.Map Text (LP m Inlines)
citationCommands inline =
  let citation = citationWith inline
      tok = spaces *> grouped inline
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
  , ("textcite", citation "textcite" AuthorInText False)
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

addPrefix :: [Inline] -> [Citation] -> [Citation]
addPrefix p (k:ks) = k {citationPrefix = p ++ citationPrefix k} : ks
addPrefix _ _      = []

addSuffix :: [Inline] -> [Citation] -> [Citation]
addSuffix s ks@(_:_) =
  let k = last ks
  in  init ks ++ [k {citationSuffix = citationSuffix k ++ s}]
addSuffix _ _ = []

simpleCiteArgs :: forall m . PandocMonad m => LP m Inlines -> LP m [Citation]
simpleCiteArgs inline = try $ do
  first  <- optionMaybe $ toList <$> opt
  second <- optionMaybe $ toList <$> opt
  keys <- try $ bgroup *> manyTill citationLabel egroup
  let (pre, suf) = case (first  , second ) of
        (Just s , Nothing) -> (mempty, s )
        (Just s , Just t ) -> (s , t )
        _                  -> (mempty, mempty)
      conv k = Citation { citationId      = k
                        , citationPrefix  = []
                        , citationSuffix  = []
                        , citationMode    = NormalCitation
                        , citationHash    = 0
                        , citationNoteNum = 0
                        }
  return $ addPrefix pre $ addSuffix suf $ map conv keys
 where
  opt :: PandocMonad m => LP m Inlines
  opt = do
    toks <- try (sp *> bracketedToks <* sp)
    -- now parse the toks as inlines
    st <- getState
    parsed <- lift $
      runParserT (mconcat <$> many inline) st "bracketed option" toks
    case parsed of
      Right result -> return result
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
      => LP m Inlines -> CitationMode -> Bool -> LP m [Citation]
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
                             (c:rest) -> c {citationMode = mode} : rest
                             []       -> []
        _            -> map (\a -> a {citationMode = mode}) cs
  where mprenote (k:ks) = (k:ks) ++ [Space]
        mprenote _ = mempty
        mpostnote (k:ks) = [Str ",", Space] ++ (k:ks)
        mpostnote _ = mempty
        addMprenote mpn (k:ks) =
          let mpnfinal = case citationPrefix k of
                           (_:_) -> mprenote mpn
                           _ -> mpn
          in addPrefix mpnfinal (k:ks)
        addMprenote _ _ = []
        addMpostnote = addSuffix . mpostnote

citationWith :: PandocMonad m
             => LP m Inlines -> Text -> CitationMode -> Bool -> LP m Inlines
citationWith inline name mode multi = do
  (c,raw) <- withRaw $ cites inline mode multi
  return $ cite c (rawInline "latex" $ "\\" <> name <> untokenize raw)

handleCitationPart :: Inlines -> [Citation]
handleCitationPart ils =
  let isCite Cite{} = True
      isCite _      = False
      (pref, rest) = break isCite (toList ils)
  in case rest of
          (Cite cs _:suff) -> addPrefix pref $ addSuffix suff cs
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
       (c:cits) -> return $ cite (c{ citationMode = mode }:cits)
                      (rawInline "latex" $ "\\citetext" <> untokenize raw)

inNote :: Inlines -> Inlines
inNote ils =
  note $ para $ ils <> str "."

