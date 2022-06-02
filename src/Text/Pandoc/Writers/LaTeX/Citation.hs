{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.LaTeX.Citation
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable
-}
module Text.Pandoc.Writers.LaTeX.Citation
  ( citationsToNatbib,
    citationsToBiblatex
  ) where

import Data.Text (Text)
import Data.Char (isPunctuation)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Data.List (foldl')
import Text.DocLayout (Doc, brackets, empty, (<+>), text, isEmpty, literal,
                       braces)
import Text.Pandoc.Walk
import Text.Pandoc.Writers.LaTeX.Types ( LW )

citationsToNatbib :: PandocMonad m
                  => ([Inline] -> LW m (Doc Text))
                  -> [Citation]
                  -> LW m (Doc Text)
citationsToNatbib inlineListToLaTeX [one]
  = citeCommand inlineListToLaTeX c p s k
  where
    Citation { citationId = k
             , citationPrefix = p
             , citationSuffix = s
             , citationMode = m
             }
      = one
    c = case m of
             AuthorInText   -> "citet"
             SuppressAuthor -> "citeyearpar"
             NormalCitation -> "citep"

citationsToNatbib inlineListToLaTeX cits
  | noPrefix (tail cits) && noSuffix (init cits) && ismode NormalCitation cits
  = citeCommand inlineListToLaTeX "citep" p s ks
  where
     noPrefix  = all (null . citationPrefix)
     noSuffix  = all (null . citationSuffix)
     ismode m  = all ((==) m  . citationMode)
     p         = citationPrefix  $
                 head cits
     s         = citationSuffix  $
                 last cits
     ks        = T.intercalate ", " $ map citationId cits

citationsToNatbib inlineListToLaTeX (c:cs)
  | citationMode c == AuthorInText = do
     author <- citeCommand inlineListToLaTeX "citeauthor" [] [] (citationId c)
     cits   <- citationsToNatbib inlineListToLaTeX
                  (c { citationMode = SuppressAuthor } : cs)
     return $ author <+> cits

citationsToNatbib inlineListToLaTeX cits = do
  cits' <- mapM convertOne cits
  return $ text "\\citetext{" <> foldl' combineTwo empty cits' <> text "}"
  where
    citeCommand' = citeCommand inlineListToLaTeX
    combineTwo a b | isEmpty a = b
                   | otherwise = a <> text "; " <> b
    convertOne Citation { citationId = k
                        , citationPrefix = p
                        , citationSuffix = s
                        , citationMode = m
                        }
        = case m of
               AuthorInText   -> citeCommand' "citealt"  p s k
               SuppressAuthor -> citeCommand' "citeyear" p s k
               NormalCitation -> citeCommand' "citealp"  p s k

citeCommand :: PandocMonad m
            => ([Inline] -> LW m (Doc Text))
            -> Text
            -> [Inline]
            -> [Inline]
            -> Text
            -> LW m (Doc Text)
citeCommand inlineListToLaTeX c p s k = do
  args <- citeArguments inlineListToLaTeX p s k
  return $ literal ("\\" <> c) <> args

type Prefix = [Inline]
type Suffix = [Inline]
type CiteId = Text
data CiteGroup = CiteGroup Prefix Suffix [CiteId]

citeArgumentsList :: PandocMonad m
              => ([Inline] -> LW m (Doc Text))
              -> CiteGroup
              -> LW m (Doc Text)
citeArgumentsList _inlineListToLaTeX (CiteGroup _ _ []) = return empty
citeArgumentsList inlineListToLaTeX (CiteGroup pfxs sfxs ids) = do
      pdoc <- inlineListToLaTeX pfxs
      sdoc <- inlineListToLaTeX sfxs'
      return $ optargs pdoc sdoc <>
              braces (literal (T.intercalate "," (reverse ids)))
      where sfxs' = stripLocatorBraces $ case sfxs of
                (Str t : r) -> case T.uncons t of
                  Just (x, xs)
                    | T.null xs
                    , isPunctuation x -> dropWhile (== Space) r
                    | isPunctuation x -> Str xs : r
                  _ -> sfxs
                _   -> sfxs
            optargs pdoc sdoc = case (isEmpty pdoc, isEmpty sdoc) of
                 (True, True ) -> empty
                 (True, False) -> brackets sdoc
                 (_   , _    ) -> brackets pdoc <> brackets sdoc

citeArguments :: PandocMonad m
              => ([Inline] -> LW m (Doc Text))
              -> [Inline]
              -> [Inline]
              -> Text
              -> LW m (Doc Text)
citeArguments inlineListToLaTeX p s k =
  citeArgumentsList inlineListToLaTeX (CiteGroup p s [k])

-- strip off {} used to define locator in pandoc-citeproc; see #5722
stripLocatorBraces :: [Inline] -> [Inline]
stripLocatorBraces = walk go
  where go (Str xs) = Str $ T.filter (\c -> c /= '{' && c /= '}') xs
        go x        = x

citationsToBiblatex :: PandocMonad m
                    => ([Inline] -> LW m (Doc Text))
                    -> [Citation] -> LW m (Doc Text)
citationsToBiblatex inlineListToLaTeX [one]
  = citeCommand inlineListToLaTeX cmd p s k
    where
       Citation { citationId = k
                , citationPrefix = p
                , citationSuffix = s
                , citationMode = m
                } = one
       cmd = case m of
                  SuppressAuthor -> "autocite*"
                  AuthorInText   -> "textcite"
                  NormalCitation -> "autocite"

citationsToBiblatex inlineListToLaTeX (c:cs)
  | all (\cit -> null (citationPrefix cit) && null (citationSuffix cit)) (c:cs)
    = do
      let cmd = case citationMode c of
                    SuppressAuthor -> "\\autocite*"
                    AuthorInText   -> "\\textcite"
                    NormalCitation -> "\\autocite"
      return $ text cmd <>
               braces (literal (T.intercalate "," (map citationId (c:cs))))
  | otherwise
    = do
      let cmd = case citationMode c of
                    SuppressAuthor -> "\\autocites*"
                    AuthorInText   -> "\\textcites"
                    NormalCitation -> "\\autocites"

      groups <- mapM (citeArgumentsList inlineListToLaTeX)
                     (reverse (foldl' grouper [] (c:cs)))

      return $ text cmd <> mconcat groups

  where grouper prev cit = case prev of
         ((CiteGroup oPfx [] ids):rest)
             | null pfx && null sfx
           -> CiteGroup oPfx sfx (cid:ids) : rest
         _ -> CiteGroup pfx sfx [cid] : prev
         where pfx = citationPrefix cit
               sfx = citationSuffix cit
               cid = citationId cit

citationsToBiblatex _ _ = return empty
