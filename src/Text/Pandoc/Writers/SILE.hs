{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{- |
   Module      : Text.Pandoc.Writers.SILE
   Copyright   : Copyright (C) 2015-2020 Caleb Maclennan <caleb@alerque.com>
   License     : GNU GPL, version 2 or above

   Maintainer  : Caleb Maclennan <caleb@alerque.com>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' format into SILE.
-}
module Text.Pandoc.Writers.SILE (
    writeSILE
  ) where
import Prelude
import Control.Monad.State.Strict
import Data.Char (isAscii, isDigit, isLetter, isPunctuation, ord)
import Data.List (foldl', intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (unEscapeString)
import Text.DocTemplates (FromContext(lookupContext), renderTemplate)
import Text.Pandoc.Class (PandocMonad, report) -- , toLang)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Shared
import Text.Printf (printf)

data WriterState =
  WriterState {
                stOLLevel       :: Int           -- level of ordered list nesting
              , stOptions       :: WriterOptions -- writer options, so they don't have to be parameter
              , stHasChapters   :: Bool          -- true if document has chapters
              , stEmptyLine     :: Bool          -- true if no content on line
              }

startingState :: WriterOptions -> WriterState
startingState options = WriterState {
                  stOLLevel = 1
                , stOptions = options
                , stHasChapters = case writerTopLevelDivision options of
                                TopLevelPart    -> True
                                TopLevelChapter -> True
                                _               -> False
                , stEmptyLine = True }

-- | Convert Pandoc to SILE.
writeSILE :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeSILE options document =
  evalStateT (pandocToSILE options document) $
    startingState options

type LW m = StateT WriterState m

pandocToSILE :: PandocMonad m
              => WriterOptions -> Pandoc -> LW m Text
pandocToSILE options (Pandoc meta blocks) = do
  let blocks' = blocks
  let colwidth = if writerWrapText options == WrapAuto
                    then Just $ writerColumns options
                    else Nothing
  metadata <- metaToContext options
              blockListToSILE
              (fmap chomp . inlineListToSILE)
              meta
  let chaptersClasses = ["book","jbook","markdown","bible","triglot","docbook"]
  let documentClass =
        case lookupContext "documentclass" (writerVariables options) `mplus`
              (stringify <$> lookupMeta "documentclass" meta) of
                 Just x -> x
                 Nothing -> case writerTopLevelDivision options of
                                 TopLevelPart    -> "book"
                                 TopLevelChapter -> "book"
                                 _               -> "plain"
  when (documentClass `elem` chaptersClasses) $
     modify $ \s -> s{ stHasChapters = True }
  main <- blockListToSILE blocks'
  st <- get
  titleMeta <- stringToSILE TextString $ stringify $ docTitle meta
  authorsMeta <- mapM (stringToSILE TextString . stringify) $ docAuthors meta

  let context  =  defField "toc" (writerTableOfContents options) $
                  defField "toc-depth" (tshow
                                        (writerTOCDepth options -
                                              if stHasChapters st
                                                 then 1
                                                 else 0)) $
                  defField "body" main $
                  defField "title-meta" titleMeta $
                  defField "author-meta"
                        (T.intercalate "; " authorsMeta) $
                  defField "documentclass" documentClass $
                  defField "numbersections" (writerNumberSections options) $
                  defField "has-chapters" (stHasChapters st) $
                  (case T.uncons . render Nothing <$>
                        getField "papersize" metadata of
                        -- uppercase a4, a5, etc.
                      Just (Just ('A', ds))
                        | not (T.null ds) && T.all isDigit ds
                          -> resetField "papersize" ("a" <> ds)
                      _   -> id)
                  metadata
  return $ render colwidth $
    case writerTemplate options of
        Nothing  -> main
        Just tpl -> renderTemplate tpl context

data StringContext = TextString
                   | URLString
                   | CodeString
                   deriving (Eq)

-- escape things as needed for SILE
stringToSILE :: PandocMonad m => StringContext -> Text -> LW m Text
stringToSILE  context zs = do
  opts <- gets stOptions
  return $ T.pack $
    foldr (go opts context) mempty $ T.unpack $ zs
 where
  go :: WriterOptions -> StringContext -> Char -> String -> String
  go _ ctx x xs   =
    let isUrl = ctx == URLString
        emits s = s <> xs
        emitc c = c : xs
    in case x of
         '{' -> emits "\\{"
         '}' -> emits "\\}"
         '%' -> emits "\\%"
         '\\'| isUrl     -> emitc '/' -- NB. / works as path sep even on Windows
             | otherwise -> emits "\\\\"
         _ -> emitc x

toLabel :: PandocMonad m => Text -> LW m Text
toLabel z = go `fmap` stringToSILE URLString z
 where
   go = T.concatMap $ \x -> case x of
     _ | (isLetter x || isDigit x) && isAscii x -> T.singleton x
       | x `elemText` "_-+=:;." -> T.singleton x
       | otherwise -> T.pack $ "ux" <> printf "%x" (ord x)

toOptions :: PandocMonad m => Text -> [Text] -> [(Text, Text)] -> LW m [Text]
toOptions ident classes kvs = do
  ref <- toLabel ident
  -- lang <- toLang $ lookup "lang" kvs
  let classes' = [ val | (val) <- classes ]
  let classes'' = T.intercalate "," classes'
  let options = (if T.null ident
                  then []
                  else [ "id=" <> ref ]) <>
                (if null classes'
                    then []
                    else [ "classes=\"" <> classes'' <> "\"" ] ) <>
                (if null kvs
                    then []
                    else [ key <> "=" <> attr | (key, attr) <- kvs ])
  return options

inCmd :: Text -> Doc Text -> Doc Text
inCmd cmd content = do
  char '\\' <> literal cmd <> braces content

inOptCmd :: Text -> [Text] -> Doc Text -> Doc Text
inOptCmd cmd args content = do
  let args' = if null args
                 then empty
                 else brackets $ hcat (intersperse "," (map literal args))
  char '\\' <> literal cmd <> args' <> (if isEmpty content then empty else braces content)

inOptEnv :: Text -> [Text] -> Doc Text -> Doc Text
inOptEnv cmd args content = do
  let args' = if null args
                 then empty
                 else brackets $ hcat (intersperse "," (map literal args))
      cmd' = braces (literal cmd)
  literal "\\begin" <> args' <> cmd'
        $$ content
        $$ literal "\\end" <> cmd'

-- | Convert Pandoc block element to SILE.
blockToSILE :: PandocMonad m
             => Block     -- ^ Block to convert
             -> LW m (Doc Text)
blockToSILE Null = return empty
blockToSILE (Div (ident,classes,kvs) bs) = do
  options <- toOptions ident classes kvs
  content <- blockListToSILE bs
  return $ inOptEnv "Div" options content
blockToSILE (Plain lst) =
  inlineListToSILE lst
blockToSILE (Para [Str ".",Space,Str ".",Space,Str "."]) = do
  inlineListToSILE [Str ".",Space,Str ".",Space,Str "."]
blockToSILE (Para lst) =
  inlineListToSILE lst
blockToSILE (LineBlock lns) =
  blockToSILE $ linesToPara lns
blockToSILE (BlockQuote lst) = do
  content <- blockListToSILE lst
  return $ inOptEnv "BlockQuote" [] content
blockToSILE (CodeBlock (ident,classes,kvs) str) = do
  options <- toOptions ident classes kvs
  content <- liftM literal $ stringToSILE CodeString str
  return $ inOptEnv "CodeBlock" options content
blockToSILE b@(RawBlock f x)
  | f == Format "sile" || f == Format "sil"
                        = return $ literal x
  | otherwise           = do
      report $ BlockNotRendered b
      return empty
blockToSILE (BulletList lst) = do
  items <- mapM listItemToSILE lst
  let content = vcat items
  return $ inOptEnv "BulletList" [] content
blockToSILE (OrderedList _ []) = return empty -- otherwise error
blockToSILE (OrderedList (start, numstyle, _) lst) = do
  st <- get
  let oldlevel = stOLLevel st
  put $ st {stOLLevel = oldlevel + 1}
  items <- mapM listItemToSILE lst
  let content = vcat items
  modify (\s -> s {stOLLevel = oldlevel})
  let numstyle' = case numstyle of
                      Decimal      -> "arabic"
                      UpperRoman   -> "Roman"
                      LowerRoman   -> "roman"
                      UpperAlpha   -> "Alpha"
                      LowerAlpha   -> "alpha"
                      Example      -> "arabic"
                      DefaultStyle -> "arabic"
  let start' = T.pack $ show start
  let opts =  [("numberstyle", numstyle')] ++
              [("start", start') | start > 1] ++
              [("tight", "true") | isTightList lst]
  options <- toOptions "" [] opts
  return $ inOptEnv "OrderedList" options content
blockToSILE (DefinitionList []) = return empty
blockToSILE (DefinitionList lst) = do
  items <- mapM defListItemToSILE lst
  let content = vcat items
  let opts = [("tight", "true") | all isTightList (map snd lst)]
  options <- toOptions "" [] opts
  return $ inOptEnv "DefinitionList" options content
blockToSILE HorizontalRule =
  return "\\HorizontalRule"
blockToSILE (Header level (id',classes,_) lst) = do
  hdr <- sectionHeader classes id' level lst
  return hdr
blockToSILE Table{} =
  return "\\script{SU.warn(\"Unimplemented, tables!\")}"

blockListToSILE :: PandocMonad m => [Block] -> LW m (Doc Text)
blockListToSILE lst =
  vsep `fmap` mapM (\b -> setEmptyLine True >> blockToSILE b) lst

listItemToSILE :: PandocMonad m => [Block] -> LW m (Doc Text)
listItemToSILE lst =
  inCmd "ListItem" <$> blockListToSILE lst

defListItemToSILE :: PandocMonad m => ([Inline], [[Block]]) -> LW m (Doc Text)
defListItemToSILE (term, defs) = do
    term' <- inlineListToSILE term
    def'  <- liftM vsep $ mapM blockListToSILE defs
    return $ inCmd "term" term' $$
             inCmd "definition" def'

sectionHeader :: PandocMonad m
              => [Text]  -- classes
              -> Text
              -> Int
              -> [Inline]
              -> LW m (Doc Text)
sectionHeader classes id' level lst = do
  content <- inlineListToSILE lst
  book <- gets stHasChapters
  opts <- gets stOptions
  let topLevelDivision = if book && writerTopLevelDivision opts == TopLevelDefault
                         then TopLevelChapter
                         else writerTopLevelDivision opts
  let level' = case topLevelDivision of
                      TopLevelPart    -> level - 2
                      TopLevelChapter -> level - 1
                      TopLevelSection -> level
                      TopLevelDefault -> level
  let level'' = T.pack $ show level'
  let sectionType = case level' of
                          -1 -> "part"
                          0  -> "chapter"
                          1  -> "section"
                          2  -> "subsection"
                          _  -> ""
  options <- toOptions id' classes [ ("level", level''), ("type", sectionType) ]
  return $ inOptCmd "Header" options content

-- | Convert list of inline elements to SILE.
inlineListToSILE :: PandocMonad m
                  => [Inline]  -- ^ Inlines to convert
                  -> LW m (Doc Text)
inlineListToSILE lst = hcat <$>
  mapM inlineToSILE (fixLineInitialSpaces $ lst)
 where fixLineInitialSpaces [] = []
       fixLineInitialSpaces (LineBreak : Str s : xs)
         | Just ('\160', _) <- T.uncons s
         = LineBreak : fixNbsps s <> fixLineInitialSpaces xs
       fixLineInitialSpaces (x:xs) = x : fixLineInitialSpaces xs
       fixNbsps s = let (ys,zs) = T.span (=='\160') s
                    in  replicate (T.length ys) hspace <> [Str zs]
       hspace = RawInline "sile" "\\nbsp{}" -- TODO: use U+00A0

-- | Convert inline element to SILE
inlineToSILE :: PandocMonad m
              => Inline    -- ^ Inline to convert
              -> LW m (Doc Text)
inlineToSILE (Span (id',classes,kvs) ils) = do
  content <- inlineListToSILE ils
  let classToCommand = [ "csl-no-emph", "csl-no-strong", "csl-no-smallcaps" ]
  let cmds = filter (`elem` classToCommand) classes
  let classes' = filter (`notElem` classToCommand) classes
  options <- toOptions id' classes' kvs
  return $ if null cmds
            then inOptCmd "Span" options content
            else inOptCmd "Span" options $ foldr inCmd content cmds

inlineToSILE (Emph lst) =
  inCmd "Emph" <$> inlineListToSILE lst
inlineToSILE (Strong lst) =
  inCmd "Strong" <$> inlineListToSILE lst
inlineToSILE (Strikeout lst) =
  inCmd "Strikeout" <$> inlineListToSILE lst
inlineToSILE (Superscript lst) =
  inCmd "Superscript" <$> inlineListToSILE lst
inlineToSILE (Subscript lst) =
  inCmd "textsubscript" <$> inlineListToSILE lst
inlineToSILE (SmallCaps lst) =
  inCmd "SmallCaps" <$> inlineListToSILE lst
inlineToSILE (Cite cits lst) = do
  st <- get
  let opts = stOptions st
  case writerCiteMethod opts of
     Natbib   -> citationsToNatbib cits
     _        -> inlineListToSILE lst

inlineToSILE (Code (_,_,_) str) = do
  content <- liftM literal $ stringToSILE TextString str
  return $ inCmd "code" content
inlineToSILE (Quoted SingleQuote lst) = do
  opts <- gets stOptions
  content <- inlineListToSILE lst
  return $ if isEnabled Ext_smart opts
              then "‘" <> content <> "’"
              else "'" <> content <> "'"
inlineToSILE (Quoted DoubleQuote lst) = do
  opts <- gets stOptions
  content <- inlineListToSILE lst
  return $ if isEnabled Ext_smart opts
              then "“" <> content <> "”"
              else "\"" <> content <> "\""
inlineToSILE (Str str) = do
  setEmptyLine False
  liftM literal $ stringToSILE TextString str
inlineToSILE (Math _ str) = do
  content <- liftM literal $ stringToSILE TextString str
  return $ inCmd "Math" content
inlineToSILE (RawInline _ str) = do
  setEmptyLine False
  return $ literal str
inlineToSILE LineBreak = return $ "\\hfill\\break" <> cr
inlineToSILE SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  case wrapText of
       WrapAuto     -> return space
       WrapNone     -> return space
       WrapPreserve -> return cr
inlineToSILE Space = return space
inlineToSILE (Link (ident,classes,kvs) txt (src,_))
  | Just ('#', ident') <- T.uncons src = do
      content <- inlineListToSILE txt
      options <- toOptions ident' classes kvs
      return $ inOptCmd "pdf:link" options content
  | otherwise = do
                content <- inlineListToSILE txt
                src' <- stringToSILE URLString (escapeURI src)
                options <- toOptions ident classes (kvs ++ [("src", src')])
                return $ inOptCmd "href" options content
inlineToSILE il@(Image _ _ (src, _))
  | Just _ <- T.stripPrefix "data:" src = do
  report $ InlineNotRendered il
  return empty
inlineToSILE (Image (ident,classes,kvs) txt (source, tit)) = do
  setEmptyLine False
  content <- inlineListToSILE txt
  let source' = if isURI source
                   then source
                   else T.pack $ unEscapeString $ T.unpack source
  source'' <- stringToSILE URLString source'
  let opts = kvs ++
              [("src", source'')] ++
              [("title", tit) | not (T.null tit)]
  options <- toOptions ident classes opts
  return $ inOptCmd "img" options content
inlineToSILE (Note content) = do
  setEmptyLine False
  contents' <- blockListToSILE content
  let optnl = case reverse content of
                   (CodeBlock _ _ : _) -> cr
                   _                   -> empty
  let noteContents = nest 2 contents' <> optnl
  return $ "\\footnote" <> braces noteContents

setEmptyLine :: PandocMonad m => Bool -> LW m ()
setEmptyLine b = modify $ \st -> st{ stEmptyLine = b }

citationsToNatbib :: PandocMonad m => [Citation] -> LW m (Doc Text)
citationsToNatbib
            [one]
  = citeCommand c p s k
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

citationsToNatbib cits
  | noPrefix (tail cits) && noSuffix (init cits) && ismode NormalCitation cits
  = citeCommand "citep" p s ks
  where
     noPrefix  = all (null . citationPrefix)
     noSuffix  = all (null . citationSuffix)
     ismode m  = all ((==) m  . citationMode)
     p         = citationPrefix  $
                 head cits
     s         = citationSuffix  $
                 last cits
     ks        = T.intercalate ", " $ map citationId cits

citationsToNatbib (c:cs) | citationMode c == AuthorInText = do
     author <- citeCommand "citeauthor" [] [] (citationId c)
     cits   <- citationsToNatbib (c { citationMode = SuppressAuthor } : cs)
     return $ author <+> cits

citationsToNatbib cits = do
  cits' <- mapM convertOne cits
  return $ text "\\citetext{" <> foldl' combineTwo empty cits' <> text "}"
  where
    combineTwo a b | isEmpty a = b
                   | otherwise = a <> text "; " <> b
    convertOne Citation { citationId = k
                        , citationPrefix = p
                        , citationSuffix = s
                        , citationMode = m
                        }
        = case m of
               AuthorInText   -> citeCommand "citealt"  p s k
               SuppressAuthor -> citeCommand "citeyear" p s k
               NormalCitation -> citeCommand "citealp"  p s k

citeCommand :: PandocMonad m
            => Text -> [Inline] -> [Inline] -> Text -> LW m (Doc Text)
citeCommand c p s k = do
  args <- citeArguments p s k
  return $ literal ("\\" <> c) <> args

citeArguments :: PandocMonad m
              => [Inline] -> [Inline] -> Text -> LW m (Doc Text)
citeArguments p s k = do
  let s' = stripLocatorBraces $ case s of
        (Str t : r) -> case T.uncons t of
          Just (x, xs)
            | T.null xs
            , isPunctuation x -> dropWhile (== Space) r
            | isPunctuation x -> Str xs : r
          _ -> s
        _                -> s
  pdoc <- inlineListToSILE p
  sdoc <- inlineListToSILE s'
  let optargs = case (isEmpty pdoc, isEmpty sdoc) of
                     (True, True ) -> empty
                     (True, False) -> brackets sdoc
                     (_   , _    ) -> brackets pdoc <> brackets sdoc
  return $ optargs <> braces (literal k)

-- strip off {} used to define locator in pandoc-citeproc; see #5722
stripLocatorBraces :: [Inline] -> [Inline]
stripLocatorBraces = walk go
  where go (Str xs) = Str $ T.filter (\c -> c /= '{' && c /= '}') xs
        go x        = x

