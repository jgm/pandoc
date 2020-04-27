{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
   Module      : Text.Pandoc.Writers.MediaWiki
   Copyright   : Copyright (C) 2008-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to MediaWiki markup.

MediaWiki:  <http://www.mediawiki.org/wiki/MediaWiki>
-}
module Text.Pandoc.Writers.MediaWiki ( writeMediaWiki, highlightingLangs ) where
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout (render, literal)
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML (escapeStringForXML)

data WriterState = WriterState {
    stNotes   :: Bool            -- True if there are notes
  , stOptions :: WriterOptions   -- writer options
  }

data WriterReader = WriterReader {
    options   :: WriterOptions -- Writer options
  , listLevel :: [Char]        -- String at beginning of list items, e.g. "**"
  , useTags   :: Bool          -- True if we should use HTML tags because we're in a complex list
  }

type MediaWikiWriter m = ReaderT WriterReader (StateT WriterState m)

-- | Convert Pandoc to MediaWiki.
writeMediaWiki :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeMediaWiki opts document =
  let initialState = WriterState { stNotes = False, stOptions = opts }
      env = WriterReader { options = opts, listLevel = [], useTags = False }
  in  evalStateT (runReaderT (pandocToMediaWiki document) env) initialState

-- | Return MediaWiki representation of document.
pandocToMediaWiki :: PandocMonad m => Pandoc -> MediaWikiWriter m Text
pandocToMediaWiki (Pandoc meta blocks) = do
  opts <- asks options
  metadata <- metaToContext opts
              (fmap (literal . trimr) . blockListToMediaWiki)
              (fmap (literal . trimr) . inlineListToMediaWiki)
              meta
  body <- blockListToMediaWiki blocks
  notesExist <- gets stNotes
  let notes = if notesExist
                 then "\n<references />"
                 else ""
  let main = body <> notes
  let context = defField "body" main
                $ defField "toc" (writerTableOfContents opts) metadata
  return $
    case writerTemplate opts of
         Nothing  -> main
         Just tpl -> render Nothing $ renderTemplate tpl context

-- | Escape special characters for MediaWiki.
escapeText :: Text -> Text
escapeText =  escapeStringForXML

-- | Convert Pandoc block element to MediaWiki.
blockToMediaWiki :: PandocMonad m
                 => Block         -- ^ Block element
                 -> MediaWikiWriter m Text

blockToMediaWiki Null = return ""

blockToMediaWiki (Div attrs bs) = do
  contents <- blockListToMediaWiki bs
  return $ render Nothing (tagWithAttrs "div" attrs) <> "\n\n" <>
                     contents <> "\n\n" <> "</div>"

blockToMediaWiki (Plain inlines) =
  inlineListToMediaWiki inlines

-- title beginning with fig: indicates that the image is a figure
blockToMediaWiki (Para [Image attr txt (src,T.stripPrefix "fig:" -> Just tit)]) = do
  capt <- inlineListToMediaWiki txt
  img  <- imageToMediaWiki attr
  let opt = if T.null tit
               then
                 if T.null capt
                    then ""
                    else "alt=" <> capt
               else "alt=" <> tit
  return $ "[[" <>
            T.intercalate "|"
            (filter (not . T.null) ["File:" <> src
                                 , "thumb"
                                 , "none"
                                 , img
                                 , opt
                                 , capt
                                 ]) <>
            "]]\n"

blockToMediaWiki (Para inlines) = do
  tags <- asks useTags
  lev <- asks listLevel
  contents <- inlineListToMediaWiki inlines
  return $ if tags
              then  "<p>" <> contents <> "</p>"
              else contents <> if null lev then "\n" else ""

blockToMediaWiki (LineBlock lns) =
  blockToMediaWiki $ linesToPara lns

blockToMediaWiki b@(RawBlock f str)
  | f == Format "mediawiki" = return str
  | f == Format "html"      = return str
  | otherwise               = "" <$ report (BlockNotRendered b)

blockToMediaWiki HorizontalRule = return "\n-----\n"

blockToMediaWiki (Header level _ inlines) = do
  contents <- inlineListToMediaWiki inlines
  let eqs = T.replicate level "="
  return $ eqs <> " " <> contents <> " " <> eqs <> "\n"

blockToMediaWiki (CodeBlock (_,classes,_) str) = do
  let at  = Set.fromList classes `Set.intersection` highlightingLangs
  return $
    case Set.toList at of
       [] -> "<pre" <> (if null classes
                           then ">"
                           else " class=\"" <> T.unwords classes <> "\">") <>
             escapeText str <> "</pre>"
       (l:_) -> "<source lang=\"" <> l <> "\">" <> str <> "</source>"
            -- note:  no escape!  even for <!

blockToMediaWiki (BlockQuote blocks) = do
  contents <- blockListToMediaWiki blocks
  return $ "<blockquote>" <> contents <> "</blockquote>"

blockToMediaWiki (Table _ blkCapt specs thead tbody tfoot) = do
  let (capt, aligns, widths, headers, rows') = toLegacyTable blkCapt specs thead tbody tfoot
  caption <- if null capt
                then return ""
                else do
                   c <- inlineListToMediaWiki capt
                   return $ "|+ " <> trimr c <> "\n"
  let headless = all null headers
  let allrows = if headless then rows' else headers:rows'
  tableBody <- T.intercalate "|-\n" `fmap`
                mapM (tableRowToMediaWiki headless aligns widths)
                     (zip [1..] allrows)
  return $ "{|\n" <> caption <> tableBody <> "|}\n"

blockToMediaWiki x@(BulletList items) = do
  tags <-
    (|| not (isSimpleList x)) <$> asks useTags
  if tags
     then do
        contents <- local (\ s -> s { useTags = True }) $ mapM listItemToMediaWiki items
        return $ "<ul>\n" <> vcat contents <> "</ul>\n"
     else do
        lev <- asks listLevel
        contents <- local (\s -> s { listLevel = listLevel s <> "*" }) $ mapM listItemToMediaWiki items
        return $ vcat contents <> if null lev then "\n" else ""

blockToMediaWiki x@(OrderedList attribs items) = do
  tags <-
    (|| not (isSimpleList x)) <$> asks useTags
  if tags
     then do
        contents <- local (\s -> s { useTags = True }) $ mapM listItemToMediaWiki items
        return $ "<ol" <> listAttribsToText attribs <> ">\n" <> vcat contents <> "</ol>\n"
     else do
        lev <- asks listLevel
        contents <- local (\s -> s { listLevel = listLevel s <> "#" }) $ mapM listItemToMediaWiki items
        return $ vcat contents <> if null lev then "\n" else ""

blockToMediaWiki x@(DefinitionList items) = do
  tags <-
    (|| not (isSimpleList x)) <$> asks useTags
  if tags
     then do
        contents <- local (\s -> s { useTags = True }) $ mapM definitionListItemToMediaWiki items
        return $ "<dl>\n" <> vcat contents <> "</dl>\n"
     else do
        lev <- asks listLevel
        contents <- local (\s -> s { listLevel = listLevel s <> ";" }) $ mapM definitionListItemToMediaWiki items
        return $ vcat contents <> if null lev then "\n" else ""

-- Auxiliary functions for lists:

-- | Convert ordered list attributes to HTML attribute string
listAttribsToText :: ListAttributes -> Text
listAttribsToText (startnum, numstyle, _) =
  let numstyle' = camelCaseToHyphenated $ tshow numstyle
  in  (if startnum /= 1
          then " start=\"" <> tshow startnum <> "\""
          else "") <>
      (if numstyle /= DefaultStyle
          then " style=\"list-style-type: " <> numstyle' <> ";\""
          else "")

-- | Convert bullet or ordered list item (list of blocks) to MediaWiki.
listItemToMediaWiki :: PandocMonad m => [Block] -> MediaWikiWriter m Text
listItemToMediaWiki items = do
  contents <- blockListToMediaWiki items
  tags <- asks useTags
  if tags
     then return $ "<li>" <> contents <> "</li>"
     else do
       marker <- asks listLevel
       return $ T.pack marker <> " " <> contents

-- | Convert definition list item (label, list of blocks) to MediaWiki.
definitionListItemToMediaWiki :: PandocMonad m
                              => ([Inline],[[Block]])
                              -> MediaWikiWriter m Text
definitionListItemToMediaWiki (label, items) = do
  labelText <- inlineListToMediaWiki label
  contents <- mapM blockListToMediaWiki items
  tags <- asks useTags
  if tags
     then return $ "<dt>" <> labelText <> "</dt>\n" <>
           T.intercalate "\n" (map (\d -> "<dd>" <> d <> "</dd>") contents)
     else do
       marker <- asks listLevel
       return $ T.pack marker <> " " <> labelText <> "\n" <>
           T.intercalate "\n" (map (\d -> T.pack (init marker) <> ": " <> d) contents)

-- | True if the list can be handled by simple wiki markup, False if HTML tags will be needed.
isSimpleList :: Block -> Bool
isSimpleList x =
  case x of
       BulletList items                 -> all isSimpleListItem items
       OrderedList (num, sty, _) items  -> all isSimpleListItem items &&
                                            num == 1 && sty `elem` [DefaultStyle, Decimal]
       DefinitionList items             -> all isSimpleListItem $ concatMap snd items
       _                                -> False

-- | True if list item can be handled with the simple wiki syntax.  False if
--   HTML tags will be needed.
isSimpleListItem :: [Block] -> Bool
isSimpleListItem []  = True
isSimpleListItem [x] =
  case x of
       Plain _          -> True
       Para  _          -> True
       BulletList _     -> isSimpleList x
       OrderedList _ _  -> isSimpleList x
       DefinitionList _ -> isSimpleList x
       _                -> False
isSimpleListItem [x, y] | isPlainOrPara x =
  case y of
       BulletList _     -> isSimpleList y
       OrderedList _ _  -> isSimpleList y
       DefinitionList _ -> isSimpleList y
       _                -> False
isSimpleListItem _ = False

isPlainOrPara :: Block -> Bool
isPlainOrPara (Plain _) = True
isPlainOrPara (Para  _) = True
isPlainOrPara _         = False

-- | Concatenates strings with line breaks between them.
vcat :: [Text] -> Text
vcat = T.intercalate "\n"

-- Auxiliary functions for tables:

tableRowToMediaWiki :: PandocMonad m
                    => Bool
                    -> [Alignment]
                    -> [Double]
                    -> (Int, [[Block]])
                    -> MediaWikiWriter m Text
tableRowToMediaWiki headless alignments widths (rownum, cells) = do
  cells' <- mapM (tableCellToMediaWiki headless rownum)
          $ zip3 alignments widths cells
  return $ T.unlines cells'

tableCellToMediaWiki :: PandocMonad m
                     => Bool
                     -> Int
                     -> (Alignment, Double, [Block])
                     -> MediaWikiWriter m Text
tableCellToMediaWiki headless rownum (alignment, width, bs) = do
  contents <- blockListToMediaWiki bs
  let marker = if rownum == 1 && not headless then "!" else "|"
  let percent w = tshow (truncate (100*w) :: Integer) <> "%"
  let attrs = ["align=" <> tshow (alignmentToText alignment) |
                 alignment /= AlignDefault && alignment /= AlignLeft] <>
              ["width=\"" <> percent width <> "\"" |
                 width /= 0.0 && rownum == 1]
  let attr = if null attrs
                then ""
                else T.unwords attrs <> "|"
  let sep = case bs of
                 [Plain _] -> " "
                 [Para  _] -> " "
                 []        -> ""
                 _         -> "\n"
  return $ marker <> attr <> sep <> trimr contents

alignmentToText :: Alignment -> Text
alignmentToText alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

imageToMediaWiki :: PandocMonad m => Attr -> MediaWikiWriter m Text
imageToMediaWiki attr = do
  opts <- gets stOptions
  let (_, cls, _) = attr
      toPx = fmap (showInPixel opts) . checkPct
      checkPct (Just (Percent _)) = Nothing
      checkPct maybeDim           = maybeDim
      go (Just w) Nothing  = w <> "px"
      go (Just w) (Just h) = w <> "x" <> h <> "px"
      go Nothing  (Just h) = "x" <> h <> "px"
      go Nothing  Nothing  = ""
      dims = go (toPx $ dimension Width attr) (toPx $ dimension Height attr)
      classes = if null cls
                   then ""
                   else "class=" <> T.unwords cls
  return $ T.intercalate "|" $ filter (not . T.null) [dims, classes]

-- | Convert list of Pandoc block elements to MediaWiki.
blockListToMediaWiki :: PandocMonad m
                     => [Block]       -- ^ List of block elements
                     -> MediaWikiWriter m Text
blockListToMediaWiki blocks =
  vcat <$> mapM blockToMediaWiki blocks

-- | Convert list of Pandoc inline elements to MediaWiki.
inlineListToMediaWiki :: PandocMonad m => [Inline] -> MediaWikiWriter m Text
inlineListToMediaWiki lst =
  fmap T.concat $ mapM inlineToMediaWiki $ fixup lst
    where
     fixup [] = []
     fixup (Str t : x : xs)
       | not (T.null t) && T.last t == '['
       , isLinkOrImage x =
          Str t : RawInline (Format "mediawiki") "<nowiki/>" : x : fixup xs
     fixup (x:xs) = x : fixup xs
     isLinkOrImage Link{}  = True
     isLinkOrImage Image{} = True
     isLinkOrImage _         = False

-- | Convert Pandoc inline element to MediaWiki.
inlineToMediaWiki :: PandocMonad m => Inline -> MediaWikiWriter m Text

inlineToMediaWiki (Span attrs ils) = do
  contents <- inlineListToMediaWiki ils
  return $ render Nothing (tagWithAttrs "span" attrs) <> contents <> "</span>"

inlineToMediaWiki (Emph lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "''" <> contents <> "''"

inlineToMediaWiki (Underline lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "<u>" <> contents <> "</u>"

inlineToMediaWiki (Strong lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "'''" <> contents <> "'''"

inlineToMediaWiki (Strikeout lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "<s>" <> contents <> "</s>"

inlineToMediaWiki (Superscript lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "<sup>" <> contents <> "</sup>"

inlineToMediaWiki (Subscript lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "<sub>" <> contents <> "</sub>"

inlineToMediaWiki (SmallCaps lst) = inlineListToMediaWiki lst

inlineToMediaWiki (Quoted SingleQuote lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "\8216" <> contents <> "\8217"

inlineToMediaWiki (Quoted DoubleQuote lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "\8220" <> contents <> "\8221"

inlineToMediaWiki (Cite _  lst) = inlineListToMediaWiki lst

inlineToMediaWiki (Code _ str) =
  return $ "<code>" <> escapeText str <> "</code>"

inlineToMediaWiki (Str str) = return $ escapeText str

inlineToMediaWiki (Math mt str) = return $
  "<math display=\"" <>
  (if mt == DisplayMath then "block" else "inline") <>
  "\">" <> str <> "</math>"
  -- note:  str should NOT be escaped

inlineToMediaWiki il@(RawInline f str)
  | f == Format "mediawiki" = return str
  | f == Format "html"      = return str
  | otherwise               = "" <$ report (InlineNotRendered il)

inlineToMediaWiki LineBreak = return "<br />\n"

inlineToMediaWiki SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  listlevel <- asks listLevel
  case wrapText of
       WrapAuto     -> return " "
       WrapNone     -> return " "
       WrapPreserve -> if null listlevel
                          then return "\n"
                          else return " "

inlineToMediaWiki Space = return " "

inlineToMediaWiki (Link _ txt (src, _)) = do
  label <- inlineListToMediaWiki txt
  case txt of
     [Str s] | isURI src && escapeURI s == src -> return src
     _  -> return $ if isURI src
              then "[" <> src <> " " <> label <> "]"
              else "[[" <> src' <> "|" <> label <> "]]"
                     -- with leading / it's a link to a help page
                     where src' = fromMaybe src $ T.stripPrefix "/" src

inlineToMediaWiki (Image attr alt (source, tit)) = do
  img  <- imageToMediaWiki attr
  alt' <- inlineListToMediaWiki alt
  let txt = if T.null alt'
               then if T.null tit
                       then ""
                       else tit
               else alt'
  return $ "[[" <>
           T.intercalate "|"
           (filter (not . T.null)
            [ "File:" <> source
            , img
            , txt
            ]) <> "]]"

inlineToMediaWiki (Note contents) = do
  contents' <- blockListToMediaWiki contents
  modify (\s -> s { stNotes = True })
  return $ "<ref>" <> stripTrailingNewlines contents' <> "</ref>"
  -- note - does not work for notes with multiple blocks

highlightingLangs :: Set.Set Text
highlightingLangs = Set.fromList [
  "abap",
  "abl",
  "abnf",
  "aconf",
  "actionscript",
  "actionscript3",
  "ada",
  "ada2005",
  "ada95",
  "adl",
  "agda",
  "ahk",
  "alloy",
  "ambienttalk",
  "ambienttalk/2",
  "antlr",
  "antlr-actionscript",
  "antlr-as",
  "antlr-c#",
  "antlr-cpp",
  "antlr-csharp",
  "antlr-java",
  "antlr-objc",
  "antlr-perl",
  "antlr-python",
  "antlr-rb",
  "antlr-ruby",
  "apache",
  "apacheconf",
  "apl",
  "applescript",
  "arduino",
  "arexx",
  "as",
  "as3",
  "asm",
  "aspectj",
  "aspx-cs",
  "aspx-vb",
  "asy",
  "asymptote",
  "at",
  "autohotkey",
  "autoit",
  "awk",
  "b3d",
  "basemake",
  "bash",
  "basic",
  "bat",
  "batch",
  "bbcode",
  "because",
  "befunge",
  "bf",
  "blitzbasic",
  "blitzmax",
  "bmax",
  "bnf",
  "boo",
  "boogie",
  "bplus",
  "brainfuck",
  "bro",
  "bsdmake",
  "bugs",
  "c",
  "c#",
  "c++",
  "c++-objdumb",
  "c-objdump",
  "ca65",
  "cadl",
  "camkes",
  "cbmbas",
  "ceylon",
  "cf3",
  "cfc",
  "cfengine3",
  "cfg",
  "cfm",
  "cfs",
  "chai",
  "chaiscript",
  "chapel",
  "cheetah",
  "chpl",
  "cirru",
  "cl",
  "clay",
  "clipper",
  "clj",
  "cljs",
  "clojure",
  "clojurescript",
  "cmake",
  "cobol",
  "cobolfree",
  "coffee",
  "coffee-script",
  "coffeescript",
  "common-lisp",
  "componentpascal",
  "console",
  "control",
  "coq",
  "cp",
  "cpp",
  "cpp-objdump",
  "cpsa",
  "crmsh",
  "croc",
  "cry",
  "cryptol",
  "csh",
  "csharp",
  "csound",
  "csound-csd",
  "csound-document",
  "csound-orc",
  "csound-sco",
  "csound-score",
  "css",
  "css+django",
  "css+erb",
  "css+genshi",
  "css+genshitext",
  "css+jinja",
  "css+lasso",
  "css+mako",
  "css+mozpreproc",
  "css+myghty",
  "css+php",
  "css+ruby",
  "css+smarty",
  "cu",
  "cucumber",
  "cuda",
  "cxx-objdump",
  "cypher",
  "cython",
  "d",
  "d-objdump",
  "dart",
  "debcontrol",
  "debsources",
  "delphi",
  "dg",
  "diff",
  "django",
  "docker",
  "dockerfile",
  "dosbatch",
  "doscon",
  "dosini",
  "dpatch",
  "dtd",
  "duby",
  "duel",
  "dylan",
  "dylan-console",
  "dylan-lid",
  "dylan-repl",
  "earl-grey",
  "earlgrey",
  "easytrieve",
  "ebnf",
  "ec",
  "ecl",
  "eg",
  "eiffel",
  "elisp",
  "elixir",
  "elm",
  "emacs",
  "erb",
  "erl",
  "erlang",
  "evoque",
  "ex",
  "exs",
  "ezhil",
  "factor",
  "fan",
  "fancy",
  "felix",
  "fish",
  "fishshell",
  "flx",
  "fortran",
  "fortranfixed",
  "foxpro",
  "fsharp",
  "fy",
  "gap",
  "gas",
  "gawk",
  "genshi",
  "genshitext",
  "gherkin",
  "glsl",
  "gnuplot",
  "go",
  "golo",
  "gooddata-cl",
  "gosu",
  "groff",
  "groovy",
  "gst",
  "haml",
  "handlebars",
  "haskell",
  "haxe",
  "haxeml",
  "hexdump",
  "hs",
  "html",
  "html+cheetah",
  "html+django",
  "html+erb",
  "html+evoque",
  "html+genshi",
  "html+handlebars",
  "html+jinja",
  "html+kid",
  "html+lasso",
  "html+mako",
  "html+myghty",
  "html+php",
  "html+ruby",
  "html+smarty",
  "html+spitfire",
  "html+twig",
  "html+velocity",
  "htmlcheetah",
  "htmldjango",
  "http",
  "hx",
  "hxml",
  "hxsl",
  "hy",
  "hybris",
  "hylang",
  "i6",
  "i6t",
  "i7",
  "idl",
  "idl4",
  "idr",
  "idris",
  "iex",
  "igor",
  "igorpro",
  "ik",
  "inform6",
  "inform7",
  "ini",
  "io",
  "ioke",
  "irb",
  "irc",
  "isabelle",
  "j",
  "jade",
  "jags",
  "jasmin",
  "jasminxt",
  "java",
  "javascript",
  "javascript+cheetah",
  "javascript+django",
  "javascript+erb",
  "javascript+genshi",
  "javascript+genshitext",
  "javascript+jinja",
  "javascript+lasso",
  "javascript+mako",
  "javascript+mozpreproc",
  "javascript+myghty",
  "javascript+php",
  "javascript+ruby",
  "javascript+smarty",
  "javascript+spitfire",
  "jbst",
  "jcl",
  "jinja",
  "jl",
  "jlcon",
  "jproperties",
  "js",
  "js+cheetah",
  "js+django",
  "js+erb",
  "js+genshi",
  "js+genshitext",
  "js+jinja",
  "js+lasso",
  "js+mako",
  "js+myghty",
  "js+php",
  "js+ruby",
  "js+smarty",
  "js+spitfire",
  "json",
  "json-ld",
  "jsonld",
  "jsonml+bst",
  "jsp",
  "julia",
  "kal",
  "kconfig",
  "kernel-config",
  "kid",
  "koka",
  "kotlin",
  "ksh",
  "lagda",
  "lasso",
  "lassoscript",
  "latex",
  "lcry",
  "lcryptol",
  "lean",
  "less",
  "lhaskell",
  "lhs",
  "lid",
  "lidr",
  "lidris",
  "lighttpd",
  "lighty",
  "limbo",
  "linux-config",
  "liquid",
  "lisp",
  "literate-agda",
  "literate-cryptol",
  "literate-haskell",
  "literate-idris",
  "live-script",
  "livescript",
  "llvm",
  "logos",
  "logtalk",
  "lsl",
  "lua",
  "m2",
  "make",
  "makefile",
  "mako",
  "man",
  "maql",
  "mask",
  "mason",
  "mathematica",
  "matlab",
  "matlabsession",
  "mawk",
  "menuconfig",
  "mf",
  "minid",
  "mma",
  "modelica",
  "modula2",
  "moin",
  "monkey",
  "moo",
  "moocode",
  "moon",
  "moonscript",
  "mozhashpreproc",
  "mozpercentpreproc",
  "mq4",
  "mq5",
  "mql",
  "mql4",
  "mql5",
  "msc",
  "mscgen",
  "mupad",
  "mxml",
  "myghty",
  "mysql",
  "nasm",
  "nawk",
  "nb",
  "nemerle",
  "nesc",
  "newlisp",
  "newspeak",
  "nginx",
  "nim",
  "nimrod",
  "nit",
  "nix",
  "nixos",
  "nroff",
  "nsh",
  "nsi",
  "nsis",
  "numpy",
  "obj-c",
  "obj-c++",
  "obj-j",
  "objc",
  "objc++",
  "objdump",
  "objdump-nasm",
  "objective-c",
  "objective-c++",
  "objective-j",
  "objectivec",
  "objectivec++",
  "objectivej",
  "objectpascal",
  "objj",
  "ocaml",
  "octave",
  "odin",
  "ooc",
  "opa",
  "openbugs",
  "openedge",
  "pacmanconf",
  "pan",
  "parasail",
  "pas",
  "pascal",
  "pawn",
  "pcmk",
  "perl",
  "perl6",
  "php",
  "php3",
  "php4",
  "php5",
  "pig",
  "pike",
  "pkgconfig",
  "pl",
  "pl6",
  "plpgsql",
  "po",
  "posh",
  "postgres",
  "postgres-console",
  "postgresql",
  "postgresql-console",
  "postscr",
  "postscript",
  "pot",
  "pov",
  "powershell",
  "praat",
  "progress",
  "prolog",
  "properties",
  "proto",
  "protobuf",
  "ps1",
  "ps1con",
  "psm1",
  "psql",
  "puppet",
  "py",
  "py3",
  "py3tb",
  "pycon",
  "pypy",
  "pypylog",
  "pyrex",
  "pytb",
  "python",
  "python3",
  "pyx",
  "qbasic",
  "qbs",
  "qml",
  "qvt",
  "qvto",
  "r",
  "racket",
  "ragel",
  "ragel-c",
  "ragel-cpp",
  "ragel-d",
  "ragel-em",
  "ragel-java",
  "ragel-objc",
  "ragel-rb",
  "ragel-ruby",
  "raw",
  "rb",
  "rbcon",
  "rconsole",
  "rd",
  "rebol",
  "red",
  "red/system",
  "redcode",
  "registry",
  "resource",
  "resourcebundle",
  "rest",
  "restructuredtext",
  "rexx",
  "rhtml",
  "rkt",
  "roboconf-graph",
  "roboconf-instances",
  "robotframework",
  "rout",
  "rql",
  "rsl",
  "rst",
  "rts",
  "ruby",
  "rust",
  "s",
  "sage",
  "salt",
  "sass",
  "sc",
  "scala",
  "scaml",
  "scheme",
  "scilab",
  "scm",
  "scss",
  "sh",
  "shell",
  "shell-session",
  "shen",
  "slim",
  "sls",
  "smali",
  "smalltalk",
  "smarty",
  "sml",
  "snobol",
  "sources.list",
  "sourceslist",
  "sp",
  "sparql",
  "spec",
  "spitfire",
  "splus",
  "sql",
  "sqlite3",
  "squeak",
  "squid",
  "squid.conf",
  "squidconf",
  "ssp",
  "st",
  "stan",
  "supercollider",
  "sv",
  "swift",
  "swig",
  "systemverilog",
  "tads3",
  "tap",
  "tcl",
  "tcsh",
  "tcshcon",
  "tea",
  "termcap",
  "terminfo",
  "terraform",
  "tex",
  "text",
  "tf",
  "thrift",
  "todotxt",
  "trac-wiki",
  "trafficscript",
  "treetop",
  "ts",
  "turtle",
  "twig",
  "typescript",
  "udiff",
  "urbiscript",
  "v",
  "vala",
  "vapi",
  "vb.net",
  "vbnet",
  "vctreestatus",
  "velocity",
  "verilog",
  "vfp",
  "vgl",
  "vhdl",
  "vim",
  "winbatch",
  "winbugs",
  "x10",
  "xbase",
  "xml",
  "xml+cheetah",
  "xml+django",
  "xml+erb",
  "xml+evoque",
  "xml+genshi",
  "xml+jinja",
  "xml+kid",
  "xml+lasso",
  "xml+mako",
  "xml+myghty",
  "xml+php",
  "xml+ruby",
  "xml+smarty",
  "xml+spitfire",
  "xml+velocity",
  "xq",
  "xql",
  "xqm",
  "xquery",
  "xqy",
  "xslt",
  "xten",
  "xtend",
  "xul+mozpreproc",
  "yaml",
  "yaml+jinja",
  "zephir" ]
