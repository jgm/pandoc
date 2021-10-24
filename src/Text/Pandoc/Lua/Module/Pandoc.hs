{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Pandoc
   Copyright   : Copyright © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc module for lua.
-}
module Text.Pandoc.Lua.Module.Pandoc
  ( pushModule
  ) where

import Prelude hiding (read)
import Control.Applicative (optional)
import Control.Monad ((>=>), forM_, when)
import Control.Monad.Catch (catch, throwM)
import Control.Monad.Except (throwError)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import HsLua as Lua hiding (pushModule)
import HsLua.Class.Peekable (PeekError)
import System.Exit (ExitCode (..))
import Text.Pandoc.Class.PandocIO (runIO)
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Filter (SingletonsList (..), walkInlines,
                               walkInlineLists, walkBlocks, walkBlockLists)
import Text.Pandoc.Lua.Marshaling ()
import Text.Pandoc.Lua.Marshaling.AST
import Text.Pandoc.Lua.Marshaling.Attr (mkAttr, mkAttributeList)
import Text.Pandoc.Lua.Marshaling.List (List (..))
import Text.Pandoc.Lua.PandocLua (PandocLua, addFunction, liftPandocLua,
                                  loadDefaultModule)
import Text.Pandoc.Options (ReaderOptions (readerExtensions))
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Readers (Reader (..), getReader)
import Text.Pandoc.Walk (Walkable)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Text.Pandoc.Lua.Util as LuaUtil
import Text.Pandoc.Error

-- | Push the "pandoc" package to the Lua stack. Requires the `List`
-- module to be loadable.
pushModule :: PandocLua NumResults
pushModule = do
  loadDefaultModule "pandoc"
  addFunction "read" read
  addFunction "pipe" pipe
  addFunction "walk_block" (walkElement peekBlock pushBlock)
  addFunction "walk_inline" (walkElement peekInline pushInline)
  -- Constructors
  addFunction "Attr" (liftPandocLua mkAttr)
  addFunction "AttributeList" (liftPandocLua mkAttributeList)
  addFunction "Pandoc" mkPandoc
  liftPandocLua $ do
    let addConstr fn = do
          pushName (functionName fn)
          pushDocumentedFunction fn
          rawset (nth 3)
    forM_ otherConstructors addConstr
    forM_ inlineConstructors addConstr
    -- add constructors to Inlines.constructor
    newtable  -- constructor
    forM_ (inlineConstructors @PandocError) $ \fn -> do
      let name = functionName fn
      pushName name
      pushName name
      rawget (nth 4)
      rawset (nth 3)
    -- set as pandoc.Inline.constructor
    pushName "Inline"
    newtable *> pushName "constructor" *> pushvalue (nth 4) *> rawset (nth 3)
    rawset (nth 4)
    pop 1 -- remaining constructor table
  return 1

inlineConstructors :: LuaError e =>  [DocumentedFunction e]
inlineConstructors =
  [ defun "Cite"
    ### liftPure2 Cite
    <#> parameter (peekList peekCitation) "citations" "list of Citations" ""
    <#> parameter peekFuzzyInlines "content" "Inline" "placeholder content"
    =#> functionResult pushInline "Inline" "cite element"
  , defun "Code"
    ### liftPure2 (flip Code)
    <#> parameter peekText "code" "string" "code string"
    <#> parameter peekAttr "attr" "Attr" "additional attributes"
    =#> functionResult pushInline "Inline" "code element"
  , mkInlinesConstr "Emph" Emph
  , defun "Image"
    ### liftPure4 (\caption src mtitle mattr ->
                     let attr = fromMaybe nullAttr mattr
                         title = fromMaybe mempty mtitle
                     in Image attr caption (src, title))
    <#> parameter peekFuzzyInlines "Inlines" "caption" "image caption / alt"
    <#> parameter peekText "string" "src" "path/URL of the image file"
    <#> optionalParameter peekText "string" "title" "brief image description"
    <#> optionalParameter peekAttr "Attr" "attr" "image attributes"
    =#> functionResult pushInline "Inline" "image element"
  , defun "LineBreak"
    ### return LineBreak
    =#> functionResult pushInline "Inline" "line break"
  , defun "Link"
    ### liftPure4 (\content target mtitle mattr ->
                     let attr = fromMaybe nullAttr mattr
                         title = fromMaybe mempty mtitle
                     in Link attr content (target, title))
    <#> parameter peekFuzzyInlines "Inlines" "content" "text for this link"
    <#> parameter peekText "string" "target" "the link target"
    <#> optionalParameter peekText "string" "title" "brief link description"
    <#> optionalParameter peekAttr "Attr" "attr" "link attributes"
    =#> functionResult pushInline "Inline" "link element"
  , defun "Math"
    ### liftPure2 Math
    <#> parameter peekMathType "quotetype" "Math" "rendering method"
    <#> parameter peekText "text" "string" "math content"
    =#> functionResult pushInline "Inline" "math element"
  , defun "Note"
    ### liftPure Note
    <#> parameter peekFuzzyBlocks "content" "Blocks" "note content"
    =#> functionResult pushInline "Inline" "note"
  , defun "Quoted"
    ### liftPure2 Quoted
    <#> parameter peekQuoteType "quotetype" "QuoteType" "type of quotes"
    <#> parameter peekFuzzyInlines "content" "Inlines" "inlines in quotes"
    =#> functionResult pushInline "Inline" "quoted element"
  , defun "RawInline"
    ### liftPure2 RawInline
    <#> parameter peekFormat "format" "Format" "format of content"
    <#> parameter peekText "text" "string" "string content"
    =#> functionResult pushInline "Inline" "raw inline element"
  , mkInlinesConstr "SmallCaps" SmallCaps
  , defun "SoftSpace"
    ### return SoftBreak
    =#> functionResult pushInline "Inline" "soft break"
  , defun "Space"
    ### return Space
    =#> functionResult pushInline "Inline" "new space"
  , defun "Span"
    ### liftPure2 (\inlns mattr -> Span (fromMaybe nullAttr mattr) inlns)
    <#> parameter peekFuzzyInlines "content" "Inlines" "inline content"
    <#> optionalParameter peekAttr "attr" "Attr" "additional attributes"
    =#> functionResult pushInline "Inline" "span element"
  , defun "Str"
    ### liftPure (\s -> s `seq` Str s)
    <#> parameter peekText "text" "string" ""
    =#> functionResult pushInline "Inline" "new Str object"
  , mkInlinesConstr "Strong" Strong
  , mkInlinesConstr "Strikeout" Strikeout
  , mkInlinesConstr "Subscript" Subscript
  , mkInlinesConstr "Superscript" Superscript
  , mkInlinesConstr "Underline" Underline
  ]

mkInlinesConstr :: LuaError e
                => Name -> ([Inline] -> Inline) -> DocumentedFunction e
mkInlinesConstr name constr = defun name
  ### liftPure (\x -> x `seq` constr x)
  <#> parameter peekFuzzyInlines "content" "Inlines" ""
  =#> functionResult pushInline "Inline" "new object"

otherConstructors :: LuaError e => [DocumentedFunction e]
otherConstructors =
  [ defun "Citation"
    ### (\cid mode mprefix msuffix mnote_num mhash ->
          cid `seq` mode `seq` mprefix `seq` msuffix `seq`
          mnote_num `seq` mhash `seq` return $! Citation
            { citationId = cid
            , citationMode = mode
            , citationPrefix = fromMaybe mempty mprefix
            , citationSuffix = fromMaybe mempty msuffix
            , citationNoteNum = fromMaybe 0 mnote_num
            , citationHash = fromMaybe 0 mhash
            })
    <#> parameter peekText "string" "cid" "citation ID (e.g. bibtex key)"
    <#> parameter peekRead "citation mode" "mode" "citation rendering mode"
    <#> optionalParameter peekFuzzyInlines "prefix" "Inlines" ""
    <#> optionalParameter peekFuzzyInlines "suffix" "Inlines" ""
    <#> optionalParameter peekIntegral "note_num" "integer" "note number"
    <#> optionalParameter peekIntegral "hash" "integer" "hash number"
    =#> functionResult pushCitation "Citation" "new citation object"
    #? "Creates a single citation."
  ]

walkElement :: (Walkable (SingletonsList Inline) a,
                Walkable (SingletonsList Block) a,
                Walkable (List Inline) a,
                Walkable (List Block) a)
            => Peeker PandocError a -> Pusher PandocError a
            -> LuaE PandocError NumResults
walkElement peek' push' = do
  x <- forcePeek $ peek' (nthBottom 1)
  f <- peek (nthBottom 2)
  let walk' =  walkInlines f
           >=> walkInlineLists f
           >=> walkBlocks f
           >=> walkBlockLists f
  walk' x >>= push'
  return (NumResults 1)

read :: T.Text -> Optional T.Text -> PandocLua NumResults
read content formatSpecOrNil = liftPandocLua $ do
  let formatSpec = fromMaybe "markdown" (Lua.fromOptional formatSpecOrNil)
  res <- Lua.liftIO . runIO $
           getReader formatSpec >>= \(rdr,es) ->
             case rdr of
               TextReader r ->
                 r def{ readerExtensions = es } content
               _ -> throwError $ PandocSomeError
                      "Only textual formats are supported"
  case res of
    Right pd -> (1 :: NumResults) <$ Lua.push pd -- success, push Pandoc
    Left  (PandocUnknownReaderError f) -> Lua.raiseError $
       "Unknown reader: " <> f
    Left  (PandocUnsupportedExtensionError e f) -> Lua.raiseError $
       "Extension " <> e <> " not supported for " <> f
    Left  e      -> Lua.raiseError $ show e

-- | Pipes input through a command.
pipe :: String           -- ^ path to executable
     -> [String]         -- ^ list of arguments
     -> BL.ByteString    -- ^ input passed to process via stdin
     -> PandocLua NumResults
pipe command args input = liftPandocLua $ do
  (ec, output) <- Lua.liftIO $ pipeProcess Nothing command args input
    `catch` (throwM . PandocIOError "pipe")
  case ec of
    ExitSuccess -> 1 <$ Lua.push output
    ExitFailure n -> do
      pushPipeError (PipeError (T.pack command) n output)
      Lua.error

data PipeError = PipeError
  { pipeErrorCommand :: T.Text
  , pipeErrorCode :: Int
  , pipeErrorOutput :: BL.ByteString
  }

peekPipeError :: PeekError e => StackIndex -> LuaE e PipeError
peekPipeError idx =
  PipeError
  <$> (Lua.getfield idx "command"    *> Lua.peek (-1) <* Lua.pop 1)
  <*> (Lua.getfield idx "error_code" *> Lua.peek (-1) <* Lua.pop 1)
  <*> (Lua.getfield idx "output"     *> Lua.peek (-1) <* Lua.pop 1)

pushPipeError :: PeekError e => Pusher e PipeError
pushPipeError pipeErr = do
  Lua.newtable
  LuaUtil.addField "command" (pipeErrorCommand pipeErr)
  LuaUtil.addField "error_code" (pipeErrorCode pipeErr)
  LuaUtil.addField "output" (pipeErrorOutput pipeErr)
  pushPipeErrorMetaTable
  Lua.setmetatable (-2)
    where
      pushPipeErrorMetaTable :: PeekError e => LuaE e ()
      pushPipeErrorMetaTable = do
        v <- Lua.newmetatable "pandoc pipe error"
        when v $ do
          pushName "__tostring"
          pushHaskellFunction pipeErrorMessage
          rawset (nth 3)

      pipeErrorMessage :: PeekError e => LuaE e NumResults
      pipeErrorMessage = do
        (PipeError cmd errorCode output) <- peekPipeError (nthBottom 1)
        pushByteString . BSL.toStrict . BSL.concat $
          [ BSL.pack "Error running "
          , BSL.pack $ T.unpack cmd
          , BSL.pack " (error code "
          , BSL.pack $ show errorCode
          , BSL.pack "): "
          , if output == mempty then BSL.pack "<no output>" else output
          ]
        return (NumResults 1)

mkPandoc :: PandocLua NumResults
mkPandoc = liftPandocLua $ do
  doc <- forcePeek $ do
    blks <- peekBlocks (nthBottom 1)
    mMeta <- optional $ peekMeta (nthBottom 2)
    pure $ Pandoc (fromMaybe nullMeta mMeta) blks
  pushPandoc doc
  return 1
