{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{- |
   Module      : Text.Pandoc.Lua.Documentation
   Copyright   : Copyright © 2026 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Render Lua documentation
-}
module Text.Pandoc.Lua.Documentation
  ( renderDocumentation
  ) where

import Data.Default (def)
import Data.List (intersperse)
import Data.Sequence (Seq ((:|>)))
import Data.Version (showVersion)
import HsLua as Lua
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition (Pandoc (Pandoc))
import Text.Pandoc.Extensions (extensionsFromList, Extension (..))
import Text.Pandoc.Options (ReaderOptions (readerExtensions))
import Text.Pandoc.Readers (readCommonMark)
import Text.Pandoc.Shared (compactify)
import Text.Pandoc.Walk (walk)

import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.UTF8 as UTF8

-- | Render the documentation object as pandoc Blocks
renderDocumentation :: DocumentationObject -> B.Blocks
renderDocumentation = \case
  DocObjectFunction fn -> renderFunctionDoc Nothing fn
  DocObjectModule mdl  -> renderModuleDoc mdl
  DocObjectType tp     -> renderTypeDoc Nothing tp

renderTypeDoc :: Maybe T.Text -> TypeDoc -> B.Blocks
renderTypeDoc mbmodname td = mconcat
  [ B.headerWith (ident, [], []) 1 (B.str $ typeDocName td)
  , parseCommonMark $ typeDocDescription td
  , if null $ typeDocMethods td
    then mempty
    else
      B.header 2 "Methods" <>
      (shiftHeadings 2 . mconcat . map (renderFunctionDoc Nothing) $
       typeDocMethods td)
  ]
 where
  ident = case mbmodname of
    Just modname  -> mconcat [ "type-", modname, ".", typeDocName td ]
    Nothing       -> mconcat [ "type-", typeDocName td ]

-- Shift headings
shiftHeadings :: Int -> B.Blocks -> B.Blocks
shiftHeadings incr blks = flip walk blks $ \case
  B.Header level attr inner -> B.Header (level + incr) attr inner
  x -> x

renderModuleDoc :: ModuleDoc -> B.Blocks
renderModuleDoc moddoc =
  let modname = moduleDocName moddoc
  in mconcat
  [ B.headerWith ("module-" <> modname, [], []) 1
      (B.str $ "Module " <> modname)
  , parseCommonMark (moduleDocDescription moddoc)
  , if null (moduleDocFields moddoc)
    then mempty
    else
      let ident = modname <> "-fields"
      in B.headerWith (ident, [], []) 2 (B.str "Fields") <>
         shiftHeadings 0 (mconcat (map (renderFieldDoc modname)
                                   (moduleDocFields moddoc)))
  , if null (moduleDocFunctions moddoc)
    then mempty
    else
      let ident = modname <> "-functions"
      in B.headerWith (ident, [], []) 2 (B.str "Functions") <>
         (shiftHeadings 2 . mconcat . map (renderFunctionDoc $ Just modname) $
          moduleDocFunctions moddoc)
  , if null (moduleDocTypes moddoc)
    then mempty
    else
      let ident = modname <> "-types"
      in B.headerWith (ident, [], []) 2 (B.str "Types") <>
         (shiftHeadings 2 . mconcat . map (renderTypeDoc $ Just modname) .
          reverse $ moduleDocTypes moddoc)
  ]

parseCommonMark :: T.Text -> B.Blocks
parseCommonMark txt =
  let exts = extensionsFromList
        [ Ext_wikilinks_title_after_pipe
        , Ext_smart
        ]
      result = runPure $ do
        Pandoc _ blks <- readCommonMark (def {readerExtensions = exts}) txt
        return $ B.fromList blks
  in either mempty id result

appendInlines :: B.Blocks -> B.Inlines -> B.Blocks
appendInlines blks inlns = case B.unMany blks of
  front :|> (B.Para xs) -> B.Many front <> B.para (addTo xs)
  front :|> (B.Plain xs) -> B.Many front <> B.plain (addTo xs)
  _ -> blks <> B.para inlns
 where addTo xs = B.fromList xs <> B.space <> inlns

appendType :: B.Blocks -> TypeSpec -> B.Blocks
appendType blks typespec =
  appendInlines blks (B.str "(" <> typeToInlines typespec <> B.str ")")

typeToInlines :: TypeSpec -> B.Inlines
typeToInlines = \case
  bt@BasicType{}   -> builtin $ tystr bt
  NamedType "integer" -> builtin "integer"
  NamedType name   -> B.linkWith ("", ["documented-type"], [])
                        ("#" <> n2t name) mempty $ B.str (n2t name)
  SeqType itemtype -> "{" <> typeToInlines itemtype <> ",...}"
  SumType summands -> mconcat . intersperse (B.str "|") $ map typeToInlines summands
  AnyType          -> "any"
  x                -> tystr x
 where
  tystr = B.str . T.pack . typeSpecToString
  n2t = UTF8.toText . fromName
  builtin = B.spanWith ("", ["builtin-lua-type"], [])

renderFunctionDoc :: Maybe T.Text -> FunctionDoc -> B.Blocks
renderFunctionDoc mbmodule fndoc =
  let name = case mbmodule of
        Just _   ->  T.takeWhileEnd (/= '.') $ funDocName fndoc
        Nothing  -> funDocName fndoc
      ident = funDocName fndoc
      level = 1
      argsString = argslist (funDocParameters fndoc)
      paramToDefItem p = ( B.code $ parameterName p
                         , compactify
                           [ appendType
                               (parseCommonMark $ parameterDescription p)
                               (parameterType p)
                           ]
                         )
      paramlist = B.definitionList . map paramToDefItem $
                  funDocParameters fndoc
  in mconcat
     [ B.headerWith (ident, [], []) level (B.str name)
     , B.plain (B.code $ name <> " (" <> argsString <> ")")
     , parseCommonMark (funDocDescription fndoc)
     , if null (funDocParameters fndoc)
       then mempty
       else B.para "Parameters:" <> paramlist
     , if funDocResults fndoc == ResultsDocList []
       then mempty
       else B.para "Returns:" <> renderResults (funDocResults fndoc)
     , case funDocSince fndoc of
         Nothing -> mempty
         Just version ->
           B.para $ B.emph $ "Since: " <> (B.str . T.pack $ showVersion version)
     ]

renderResults :: ResultsDoc -> B.Blocks
renderResults (ResultsDocMult descr) = parseCommonMark descr
renderResults (ResultsDocList rvd) = B.bulletList $ map renderResultVal rvd
 where
   renderResultVal (ResultValueDoc typespec descr) =
     parseCommonMark descr `appendType` typespec

argslist :: [ParameterDoc] -> T.Text
argslist params =
  -- Expect optional values to come after required values.
  let (required, optional') = break parameterIsOptional params
      reqs = map parameterName required
      opts = map parameterName optional'
  in if null opts
     then T.intercalate ", " reqs
     else T.intercalate ", " reqs <>
          (if null required then "[" else "[, ") <>
          T.intercalate "[, " opts <> T.replicate (length opts) "]"

renderFieldDoc :: T.Text -> FieldDoc -> B.Blocks
renderFieldDoc _modname fd =
  B.headerWith (ident, [], []) 3 (B.str name) <>
  appendType (parseCommonMark $ fieldDocDescription fd) (fieldDocType fd)
 where
  ident = fieldDocName fd
  name = T.takeWhileEnd (/= '.') $ fieldDocName fd
