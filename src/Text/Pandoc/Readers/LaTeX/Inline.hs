{-# LANGUAGE OverloadedStrings     #-}
{- |
   Module      : Text.Pandoc.Readers.LaTeX.Inline
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable
-}
module Text.Pandoc.Readers.LaTeX.Inline
  ( acronymCommands
  , verbCommands
  , charCommands
  , nameCommands
  , refCommands
  , rawInlineOr
  , listingsLanguage
  )
where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Builder
import Text.Pandoc.Readers.LaTeX.Types (Tok (..), TokType (..))
import Control.Applicative (optional)
import Control.Monad (guard, mzero, mplus)
import Text.Pandoc.Class.PandocMonad (PandocMonad (..), translateTerm)
import Text.Pandoc.Readers.LaTeX.Parsing
import Text.Pandoc.Extensions (extensionEnabled, Extension(..))
import Text.Pandoc.Parsing (getOption, updateState, getState, notFollowedBy,
                            manyTill, getInput, setInput, incSourceColumn,
                            option)
import Text.Pandoc.Highlighting (fromListingsLanguage,)
import Data.Maybe (maybeToList)
import Text.Pandoc.Options (ReaderOptions(..))
import qualified Text.Pandoc.Translations as Translations

rawInlineOr :: PandocMonad m => Text -> LP m Inlines -> LP m Inlines
rawInlineOr name' fallback = do
  parseRaw <- extensionEnabled Ext_raw_tex <$> getOption readerExtensions
  if parseRaw
     then rawInline "latex" <$> getRawCommand name' ("\\" <> name')
     else fallback

dolabel :: PandocMonad m => LP m Inlines
dolabel = do
  v <- braced
  let refstr = untokenize v
  updateState $ \st ->
    st{ sLastLabel = Just refstr }
  return $ spanWith (refstr,[],[("label", refstr)])
    $ inBrackets $ str $ untokenize v

doref :: PandocMonad m => Text -> LP m Inlines
doref cls = do
  v <- braced
  let refstr = untokenize v
  return $ linkWith ("",[],[ ("reference-type", cls)
                           , ("reference", refstr)])
                    ("#" <> refstr)
                    ""
                    (inBrackets $ str refstr)

inBrackets :: Inlines -> Inlines
inBrackets x = str "[" <> x <> str "]"

doTerm :: PandocMonad m => Translations.Term -> LP m Inlines
doTerm term = str <$> translateTerm term

lit :: Text -> LP m Inlines
lit = pure . str

doverb :: PandocMonad m => LP m Inlines
doverb = do
  Tok _ Symbol t <- anySymbol
  marker <- case T.uncons t of
              Just (c, ts) | T.null ts -> return c
              _            -> mzero
  withVerbatimMode $
    code . untokenize <$>
      manyTill (notFollowedBy newlineTok >> verbTok marker) (symbol marker)

verbTok :: PandocMonad m => Char -> LP m Tok
verbTok stopchar = do
  t@(Tok pos toktype txt) <- anyTok
  case T.findIndex (== stopchar) txt of
       Nothing -> return t
       Just i  -> do
         let (t1, t2) = T.splitAt i txt
         inp <- getInput
         setInput $ Tok (incSourceColumn pos i) Symbol (T.singleton stopchar)
                  : totoks (incSourceColumn pos (i + 1)) (T.drop 1 t2) ++ inp
         return $ Tok pos toktype t1

listingsLanguage :: [(Text, Text)] -> Maybe Text
listingsLanguage opts =
  case lookup "language" opts of
    Nothing  -> Nothing
    Just l   -> fromListingsLanguage l `mplus` Just l

dolstinline :: PandocMonad m => LP m Inlines
dolstinline = do
  options <- option [] keyvals
  let classes = maybeToList $ listingsLanguage options
  doinlinecode classes

domintinline :: PandocMonad m => LP m Inlines
domintinline = do
  skipopts
  cls <- untokenize <$> braced
  doinlinecode [cls]

doinlinecode :: PandocMonad m => [Text] -> LP m Inlines
doinlinecode classes = do
  Tok _ Symbol t <- anySymbol
  marker <- case T.uncons t of
              Just (c, ts) | T.null ts -> return c
              _            -> mzero
  let stopchar = if marker == '{' then '}' else marker
  withVerbatimMode $
    codeWith ("",classes,[]) . T.map nlToSpace . untokenize <$>
      manyTill (verbTok stopchar) (symbol stopchar)

nlToSpace :: Char -> Char
nlToSpace '\n' = ' '
nlToSpace x    = x



verbCommands :: PandocMonad m => M.Map Text (LP m Inlines)
verbCommands = M.fromList
  [ ("verb", doverb)
  , ("lstinline", dolstinline)
  , ("mintinline", domintinline)
  , ("Verb", doverb)
  ]



charCommands :: PandocMonad m => M.Map Text (LP m Inlines)
charCommands = M.fromList
  [ ("ldots", lit "…")
  , ("vdots", lit "\8942")
  , ("dots", lit "…")
  , ("mdots", lit "…")
  , ("sim", lit "~")
  , ("sep", lit ",")
  , ("P", lit "¶")
  , ("S", lit "§")
  , ("$", lit "$")
  , ("%", lit "%")
  , ("&", lit "&")
  , ("#", lit "#")
  , ("_", lit "_")
  , ("{", lit "{")
  , ("}", lit "}")
  , ("qed", lit "\a0\x25FB")
  , ("/", pure mempty) -- italic correction
  , ("\\", linebreak <$ (do inTableCell <- sInTableCell <$> getState
                            guard $ not inTableCell
                            optional rawopt
                            spaces))
  , (",", lit "\8198")
  , ("@", pure mempty)
  , (" ", lit "\160")
  , ("ps", pure $ str "PS." <> space)
  , ("TeX", lit "TeX")
  , ("LaTeX", lit "LaTeX")
  , ("bar", lit "|")
  , ("textless", lit "<")
  , ("textgreater", lit ">")
  , ("textbackslash", lit "\\")
  , ("backslash", lit "\\")
  , ("slash", lit "/")
  -- fontawesome
  , ("faCheck", lit "\10003")
  , ("faClose", lit "\10007")
  -- hyphenat
  , ("bshyp", lit "\\\173")
  , ("fshyp", lit "/\173")
  , ("dothyp", lit ".\173")
  , ("colonhyp", lit ":\173")
  , ("hyp", lit "-")
  ]

nameCommands :: PandocMonad m => M.Map Text (LP m Inlines)
nameCommands = M.fromList
  [ ("figurename", doTerm Translations.Figure)
  , ("prefacename", doTerm Translations.Preface)
  , ("refname", doTerm Translations.References)
  , ("bibname", doTerm Translations.Bibliography)
  , ("chaptername", doTerm Translations.Chapter)
  , ("partname", doTerm Translations.Part)
  , ("contentsname", doTerm Translations.Contents)
  , ("listfigurename", doTerm Translations.ListOfFigures)
  , ("listtablename", doTerm Translations.ListOfTables)
  , ("indexname", doTerm Translations.Index)
  , ("abstractname", doTerm Translations.Abstract)
  , ("tablename", doTerm Translations.Table)
  , ("enclname", doTerm Translations.Encl)
  , ("ccname", doTerm Translations.Cc)
  , ("headtoname", doTerm Translations.To)
  , ("pagename", doTerm Translations.Page)
  , ("seename", doTerm Translations.See)
  , ("seealsoname", doTerm Translations.SeeAlso)
  , ("proofname", doTerm Translations.Proof)
  , ("glossaryname", doTerm Translations.Glossary)
  , ("lstlistingname", doTerm Translations.Listing)
  ]

refCommands :: PandocMonad m => M.Map Text (LP m Inlines)
refCommands = M.fromList
  [ ("label", rawInlineOr "label" dolabel)
  , ("ref", rawInlineOr "ref" $ doref "ref")
  , ("cref", rawInlineOr "cref" $ doref "ref")       -- from cleveref.sty
  , ("vref", rawInlineOr "vref" $ doref "ref+page")  -- from varioref.sty
  , ("eqref", rawInlineOr "eqref" $ doref "eqref")   -- from amsmath.sty
  ]

acronymCommands :: PandocMonad m => M.Map Text (LP m Inlines)
acronymCommands = M.fromList
  -- glossaries package
  [ ("gls", doAcronym "short")
  , ("Gls", doAcronym "short")
  , ("glsdesc", doAcronym "long")
  , ("Glsdesc", doAcronym "long")
  , ("GLSdesc", doAcronym "long")
  , ("acrlong", doAcronym "long")
  , ("Acrlong", doAcronym "long")
  , ("acrfull", doAcronym "full")
  , ("Acrfull", doAcronym "full")
  , ("acrshort", doAcronym "abbrv")
  , ("Acrshort", doAcronym "abbrv")
  , ("glspl", doAcronymPlural "short")
  , ("Glspl", doAcronymPlural "short")
  , ("glsdescplural", doAcronymPlural "long")
  , ("Glsdescplural", doAcronymPlural "long")
  , ("GLSdescplural", doAcronymPlural "long")
  -- acronyms package
  , ("ac", doAcronym "short")
  , ("acf", doAcronym "full")
  , ("acs", doAcronym "abbrv")
  , ("acl", doAcronym "long")
  , ("acp", doAcronymPlural "short")
  , ("acfp", doAcronymPlural "full")
  , ("acsp", doAcronymPlural "abbrv")
  , ("aclp", doAcronymPlural "long")
  , ("Ac", doAcronym "short")
  , ("Acf", doAcronym "full")
  , ("Acs", doAcronym "abbrv")
  , ("Acl", doAcronym "long")
  , ("Acp", doAcronymPlural "short")
  , ("Acfp", doAcronymPlural "full")
  , ("Acsp", doAcronymPlural "abbrv")
  , ("Aclp", doAcronymPlural "long")
  ]

doAcronym :: PandocMonad m => Text -> LP m Inlines
doAcronym form = do
  acro <- braced
  return . mconcat $ [spanWith ("",[],[("acronym-label", untokenize acro),
    ("acronym-form", "singular+" <> form)])
    $ str $ untokenize acro]

doAcronymPlural :: PandocMonad m => Text -> LP m Inlines
doAcronymPlural form = do
  acro <- braced
  let plural = str "s"
  return . mconcat $ [spanWith ("",[],[("acronym-label", untokenize acro),
    ("acronym-form", "plural+" <> form)]) $
   mconcat [str $ untokenize acro, plural]]


