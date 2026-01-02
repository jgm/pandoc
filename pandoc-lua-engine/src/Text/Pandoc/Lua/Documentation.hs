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
import Data.Sequence (Seq ((:|>)))
import Data.Version (showVersion)
import HsLua as Lua
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition
import Text.Pandoc.Readers (readCommonMark)

import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B

-- | Render the documentation object as pandoc Blocks
renderDocumentation :: DocumentationObject -> B.Blocks
renderDocumentation = \case
  DocObjectFunction fn -> renderFunctionDoc Nothing fn
  DocObjectModule mdl  -> renderModuleDoc mdl
  DocObjectType tp     -> renderTypeDoc tp

renderTypeDoc :: TypeDoc -> B.Blocks
renderTypeDoc _ = mempty

renderModuleDoc :: ModuleDoc -> B.Blocks
renderModuleDoc moddoc = mconcat
  [ B.header 1 (B.str $ moduleDocName moddoc)
  , parseCommonMark (moduleDocDescription moddoc)
  , if null (moduleDocFields moddoc)
    then mempty
    else
      let ident = moduleDocName moddoc <> "-fields"
      in B.headerWith (ident, [], []) 2 (B.str "Fields") <>
         mconcat (map renderFieldDoc (moduleDocFields moddoc))
  , if null (moduleDocFunctions moddoc)
    then mempty
    else
      let ident = moduleDocName moddoc <> "-functions"
      in B.headerWith (ident, [], []) 2 (B.str "Functions") <>
         mconcat (map (renderFunctionDoc Nothing) (moduleDocFunctions moddoc))
  ]

parseCommonMark :: T.Text -> B.Blocks
parseCommonMark txt =
  let result = runPure $ do
        Pandoc _ blks <- readCommonMark def txt
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
typeToInlines = B.str . T.pack . typeSpecToString

renderFunctionDoc :: Maybe T.Text -> FunctionDoc -> B.Blocks
renderFunctionDoc mbmodule fndoc =
  let name = funDocName fndoc
      level = 1
      ident = maybe "" (<> ".") mbmodule <> name
      argsString = argslist (funDocParameters fndoc)
      paramToDefItem p = ( B.code $ parameterName p
                         , [ appendType
                               (parseCommonMark $ parameterDescription p)
                               (parameterType p)
                           ]
                         )
      paramlist = B.definitionList $ map paramToDefItem $ funDocParameters fndoc
  in mconcat
     [ B.headerWith (ident, [], []) level (B.str name)
     , B.plain (B.code $ name <> " (" <> argsString <> ")")
     , parseCommonMark (funDocDescription fndoc)
     , if null (funDocParameters fndoc)
       then mempty
       else B.para "Parameters:" <> paramlist
     , case funDocSince fndoc of
         Nothing -> mempty
         Just version ->
           B.para $ B.emph "Since: " <> (B.str . T.pack $ showVersion version)
     ]

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

renderFieldDoc :: FieldDoc -> B.Blocks
renderFieldDoc fd =
  B.headerWith (fieldDocName fd, [], []) 3 (B.str (fieldDocName fd)) <>
  appendType (parseCommonMark $ fieldDocDescription fd) (fieldDocType fd)
