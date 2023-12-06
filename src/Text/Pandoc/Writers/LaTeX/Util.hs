{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Writers.LaTeX.Util
   Copyright   : Copyright (C) 2006-2023 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable
-}
module Text.Pandoc.Writers.LaTeX.Util (
    stringToLaTeX
  , StringContext(..)
  , toLabel
  , inCmd
  , wrapDiv
  , hypertarget
  , labelFor
  , getListingsLanguage
  , mbBraced
  )
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Text.Pandoc.Class (PandocMonad, toLang)
import Text.Pandoc.Options (WriterOptions(..), isEnabled)
import Text.Pandoc.Writers.LaTeX.Types (LW, WriterState(..))
import Text.Pandoc.Writers.LaTeX.Lang (toBabel)
import Text.Pandoc.Highlighting (toListingsLanguage)
import Text.DocLayout
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize (showFl)
import Control.Monad.State.Strict (gets, modify)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Extensions (Extension(Ext_smart))
import Data.Char (isLetter, isSpace, isDigit, isAscii, ord, isAlphaNum)
import Text.Printf (printf)
import Text.Pandoc.Shared (safeRead)
import qualified Data.Text.Normalize as Normalize
import Data.List (uncons)

data StringContext = TextString
                   | URLString
                   | CodeString
                   deriving (Eq)

-- escape things as needed for LaTeX
stringToLaTeX :: PandocMonad m => StringContext -> Text -> LW m Text
stringToLaTeX context zs = do
  opts <- gets stOptions
  when (T.any (== '\x200c') zs) $
    modify (\s -> s { stZwnj = True })
  return $ T.pack $
    foldr (go opts context) mempty $ T.unpack $
    if writerPreferAscii opts
       then Normalize.normalize Normalize.NFD zs
       else zs
 where
  go :: WriterOptions -> StringContext -> Char -> String -> String
  go opts ctx x xs   =
    let ligatures = isEnabled Ext_smart opts && ctx == TextString
        isUrl = ctx == URLString
        mbAccentCmd =
          if writerPreferAscii opts && ctx == TextString
             then uncons xs >>= \(c,_) -> lookupAccent c
             else Nothing
        emits s =
          case mbAccentCmd of
               Just cmd ->
                 cmd <> "{" <> s <> "}" <> drop 1 xs -- drop combining accent
               Nothing  -> s <> xs
        emitc c =
          case mbAccentCmd of
               Just cmd ->
                 cmd <> "{" <> [c] <> "}" <> drop 1 xs -- drop combining accent
               Nothing  -> c : xs
        emitcseq cs =
          case xs of
            c:_ | isLetter c
                , ctx == TextString
                             -> cs <> " " <> xs
                | isSpace c  -> cs <> "{}" <> xs
                | ctx == TextString
                             -> cs <> xs
            _ -> cs <> "{}" <> xs
        emitquote cs =
          case xs of
            '`':_  -> cs <> "\\," <> xs -- add thin space
            '\'':_ -> cs <> "\\," <> xs -- add thin space
            _      -> cs <> xs
    in case x of
         _ | isUrl ->
           case x of
             '\\' -> emitc '/' -- NB / works as path sep even on Windows
             '#' -> emits "\\#" -- #9014
             '%' -> emits "\\%" -- #9014
             '{' -> emits "\\%7B"
             '}' -> emits "\\%7D"
             '|' -> emits "\\%7C"
             '^' -> emits "\\%5E"
             '[' -> emits "\\%5B"
             ']' -> emits "\\%5D"
             '`' -> emits "\\%60"
             _ -> emitc x
         '{' -> emits "\\{"
         '}' -> emits "\\}"
         '?' | ligatures ->  -- avoid ?` ligature
           case xs of
             '`':_ -> emits "?{}"
             _     -> emitc x
         '!' | ligatures ->  -- avoid !` ligature
           case xs of
             '`':_ -> emits "!{}"
             _     -> emitc x
         '`' | ctx == CodeString -> emitcseq "\\textasciigrave"
         '$' -> emits "\\$"
         '%' -> emits "\\%"
         '&' -> emits "\\&"
         '_' -> emits "\\_"
         '#' -> emits "\\#"
         '-' -> case xs of
                     -- prevent adjacent hyphens from forming ligatures
                     ('-':_) -> emits "-\\/"
                     _       -> emitc '-'
         '~' -> emitcseq "\\textasciitilde"
         '^' -> emits "\\^{}"
         '\\' -> emitcseq "\\textbackslash"
         '|'  -> emitcseq "\\textbar"
         '<'  -> emitcseq "\\textless"
         '>'  -> emitcseq "\\textgreater"
         '['  -> emits "{[}"  -- to avoid interpretation as
         ']'  -> emits "{]}"  -- optional arguments
         '\'' -> emitcseq "\\textquotesingle"
         '\160' -> emits "~"
         '\x200B' -> emits "\\hspace{0pt}"  -- zero-width space
         '\x202F' -> emits "\\,"
         '\x2026' | ligatures -> emitcseq "\\ldots"
         '\x2018' | ligatures -> emitquote "`"
         '\x2019' | ligatures -> emitquote "'"
         '\x201C' | ligatures -> emitquote "``"
         '\x201D' | ligatures -> emitquote "''"
         '\x2014' | ligatures -> emits "---"
         '\x2013' | ligatures -> emits "--"
         _ | writerPreferAscii opts
             -> case x of
                  'ı' -> emitcseq "\\i"
                  'ȷ' -> emitcseq "\\j"
                  'å' -> emitcseq "\\aa"
                  'Å' -> emitcseq "\\AA"
                  'ß' -> emitcseq "\\ss"
                  'ø' -> emitcseq "\\o"
                  'Ø' -> emitcseq "\\O"
                  'Ł' -> emitcseq "\\L"
                  'ł' -> emitcseq "\\l"
                  'æ' -> emitcseq "\\ae"
                  'Æ' -> emitcseq "\\AE"
                  'œ' -> emitcseq "\\oe"
                  'Œ' -> emitcseq "\\OE"
                  '£' -> emitcseq "\\pounds"
                  '€' -> emitcseq "\\euro"
                  '©' -> emitcseq "\\copyright"
                  _   -> emitc x
           | otherwise -> emitc x

lookupAccent :: Char -> Maybe String
lookupAccent '\779'  = Just "\\H"
lookupAccent '\768'  = Just "\\`"
lookupAccent '\769'  = Just "\\'"
lookupAccent '\770'  = Just "\\^"
lookupAccent '\771'  = Just "\\~"
lookupAccent '\776'  = Just "\\\""
lookupAccent '\775'  = Just "\\."
lookupAccent '\772'  = Just "\\="
lookupAccent '\781'  = Just "\\|"
lookupAccent '\817'  = Just "\\b"
lookupAccent '\807'  = Just "\\c"
lookupAccent '\783'  = Just "\\G"
lookupAccent '\777'  = Just "\\h"
lookupAccent '\803'  = Just "\\d"
lookupAccent '\785'  = Just "\\f"
lookupAccent '\778'  = Just "\\r"
lookupAccent '\865'  = Just "\\t"
lookupAccent '\782'  = Just "\\U"
lookupAccent '\780'  = Just "\\v"
lookupAccent '\774'  = Just "\\u"
lookupAccent '\808'  = Just "\\k"
lookupAccent '\8413' = Just "\\textcircled"
lookupAccent _       = Nothing

toLabel :: PandocMonad m => Text -> LW m Text
toLabel z = go `fmap` stringToLaTeX URLString z
 where
   go = T.concatMap $ \x -> case x of
     _ | (isLetter x || isDigit x) && isAscii x -> T.singleton x
       | T.any (== x) "_-+=:;." -> T.singleton x
       | otherwise -> T.pack $ "ux" <> printf "%x" (ord x)

-- | Puts contents into LaTeX command.
inCmd :: Text -> Doc Text -> Doc Text
inCmd cmd contents = char '\\' <> literal cmd <> braces contents

mapAlignment :: Text -> Text
mapAlignment a = case a of
                   "top" -> "T"
                   "top-baseline" -> "t"
                   "bottom" -> "b"
                   "center" -> "c"
                   _ -> a

wrapDiv :: PandocMonad m => Attr -> Doc Text -> LW m (Doc Text)
wrapDiv (_,classes,kvs) t = do
  beamer <- gets stBeamer
  let align dir txt = inCmd "begin" dir $$ txt $$ inCmd "end" dir
  lang <- toLang $ lookup "lang" kvs
  let wrapColumns = if beamer && "columns" `elem` classes
                    then \contents ->
                           let valign = maybe "T" mapAlignment (lookup "align" kvs)
                               totalwidth = maybe [] (\x -> ["totalwidth=" <> x])
                                 (lookup "totalwidth" kvs)
                               onlytextwidth = filter ("onlytextwidth" ==) classes
                               options = text $ T.unpack $ T.intercalate "," $
                                 valign : totalwidth ++ onlytextwidth
                           in inCmd "begin" "columns" <> brackets options
                              $$ contents
                              $$ inCmd "end" "columns"
                    else id
      wrapColumn  = if beamer && "column" `elem` classes
                    then \contents ->
                           let valign =
                                 maybe ""
                                 (brackets . text . T.unpack . mapAlignment)
                                 (lookup "align" kvs)
                               w = maybe "0.48" fromPct (lookup "width" kvs)
                           in  inCmd "begin" "column" <>
                               valign <>
                               braces (literal w <> "\\textwidth")
                               $$ contents
                               $$ inCmd "end" "column"
                    else id
      fromPct xs =
        case T.unsnoc xs of
          Just (ds, '%') -> case safeRead ds of
                              Just digits -> showFl (digits / 100 :: Double)
                              Nothing -> xs
          _              -> xs
      wrapDir = case lookup "dir" kvs of
                  Just "rtl" -> align "RTL"
                  Just "ltr" -> align "LTR"
                  _          -> id
      wrapLang txt = case lang >>= toBabel of
                       Just l -> inCmd "begin" "otherlanguage"
                                            <> (braces (literal l))
                                       $$ blankline <> txt <> blankline
                                       $$ inCmd "end" "otherlanguage"
                       Nothing  -> txt
  return $ wrapColumns . wrapColumn . wrapDir . wrapLang $ t

hypertarget :: PandocMonad m => Text -> LW m (Doc Text)
hypertarget "" = return mempty
hypertarget ident = do
  inHeading <- gets stInHeading
  if inHeading
     then do -- see #9209 (these cases should be rare)
      ref <- literal <$> toLabel ident
      return $ text "\\protect\\hypertarget" <> braces ref <> "{}"
     else do
      label <- labelFor ident
      return $ text "\\phantomsection" <> label

labelFor :: PandocMonad m => Text -> LW m (Doc Text)
labelFor ""    = return empty
labelFor ident = do
  ref <- literal `fmap` toLabel ident
  return $ text "\\label" <> braces ref

-- Determine listings language from list of class attributes.
getListingsLanguage :: [Text] -> Maybe Text
getListingsLanguage xs
  = foldr ((<|>) . toListingsLanguage) Nothing xs

mbBraced :: Text -> Text
mbBraced x = if not (T.all isAlphaNum x)
                then "{" <> x <> "}"
                else x
