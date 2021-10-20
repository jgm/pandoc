{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{- |
Module      : Text.Pandoc.Lua.Marshaling.Attr
Copyright   : © 2012-2021 John MacFarlane
              © 2017-2021 Albert Krewinkel
License     : GNU GPL, version 2 or above

Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
Stability   : alpha

Marshaling/unmarshaling instances for document AST elements.
-}
module Text.Pandoc.Lua.Marshaling.Attr
  ( typeAttr
  , peekAttr
  , pushAttr
  , mkAttr
  , mkAttributeList
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad ((<$!>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import HsLua
import HsLua.Marshalling.Peekers (peekIndexRaw)
import Safe (atMay)
import Text.Pandoc.Definition (Attr, nullAttr)
import Text.Pandoc.Lua.Marshaling.List (pushPandocList)

import qualified Data.Text as T

typeAttr :: LuaError e => DocumentedType e Attr
typeAttr = deftype "Attr"
  [ operation Eq $ lambda
    ### liftPure2 (==)
    <#> parameter peekAttr "a1" "Attr" ""
    <#> parameter peekAttr "a2" "Attr" ""
    =#> functionResult pushBool "boolean" "whether the two are equal"
  ]
  [ property "identifier" "element identifier"
      (pushText, \(ident,_,_) -> ident)
      (peekText, \(_,cls,kv) -> (,cls,kv))
  , property "classes" "element classes"
      (pushPandocList pushText, \(_,classes,_) -> classes)
      (peekList peekText, \(ident,_,kv) -> (ident,,kv))
  , property "attributes" "various element attributes"
      (pushAttribs, \(_,_,attribs) -> attribs)
      (peekAttribs, \(ident,cls,_) -> (ident,cls,))
  , method $ defun "clone"
    ### return
    <#> parameter peekAttr "attr" "Attr" ""
    =#> functionResult pushAttr "Attr" "new Attr element"
  ]

pushAttr :: LuaError e => Pusher e Attr
pushAttr = pushUD typeAttr

peekAttribs :: LuaError e => Peeker e [(Text,Text)]
peekAttribs idx = liftLua (ltype idx) >>= \case
  TypeUserdata -> peekUD typeAttributeList idx
  TypeTable    -> liftLua (rawlen idx) >>= \case
    0 -> peekKeyValuePairs peekText peekText idx
    _ -> peekList (peekPair peekText peekText) idx
  _            -> fail "unsupported type"

pushAttribs :: LuaError e => Pusher e [(Text, Text)]
pushAttribs = pushUD typeAttributeList

typeAttributeList :: LuaError e => DocumentedType e [(Text, Text)]
typeAttributeList = deftype "AttributeList"
  [ operation Eq $ lambda
    ### liftPure2 (==)
    <#> parameter peekAttribs "a1" "AttributeList" ""
    <#> parameter peekAttribs "a2" "AttributeList" ""
    =#> functionResult pushBool "boolean" "whether the two are equal"

  , operation Index $ lambda
    ### liftPure2 lookupKey
    <#> udparam typeAttributeList "t" "attributes list"
    <#> parameter peekKey "string|integer" "key" "lookup key"
    =#> functionResult (maybe pushnil pushAttribute) "string|table"
          "attribute value"

  , operation Newindex $ lambda
    ### setKey
    <#> udparam typeAttributeList "t" "attributes list"
    <#> parameter peekKey "string|integer" "key" "lookup key"
    <#> optionalParameter peekAttribute "string|nil" "value" "new value"
    =#> []

  , operation Len $ lambda
    ### liftPure length
    <#> udparam typeAttributeList "t" "attributes list"
    =#> functionResult pushIntegral "integer" "number of attributes in list"

  , operation Pairs $ lambda
    ### pushIterator (\(k, v) -> 2 <$ pushText k <* pushText v)
    <#> udparam typeAttributeList "t" "attributes list"
    =?> "iterator triple"

  , operation Tostring $ lambda
    ### liftPure show
    <#> udparam typeAttributeList "t" "attributes list"
    =#> functionResult pushString "string" ""
  ]
  []

data Key = StringKey Text | IntKey Int

peekKey :: LuaError e => Peeker e (Maybe Key)
peekKey idx = liftLua (ltype idx) >>= \case
  TypeNumber -> Just . IntKey <$!> peekIntegral idx
  TypeString -> Just . StringKey <$!> peekText idx
  _          -> return Nothing

data Attribute
  = AttributePair (Text, Text)
  | AttributeValue Text

pushAttribute :: LuaError e => Pusher e Attribute
pushAttribute = \case
  (AttributePair kv) -> pushPair pushText pushText kv
  (AttributeValue v) -> pushText v

-- | Retrieve an 'Attribute'.
peekAttribute :: LuaError e => Peeker e Attribute
peekAttribute idx = (AttributeValue <$!> peekText idx)
  <|> (AttributePair <$!> peekPair peekText peekText idx)

lookupKey :: [(Text,Text)] -> Maybe Key -> Maybe Attribute
lookupKey !kvs = \case
  Just (StringKey str) -> AttributeValue <$> lookup str kvs
  Just (IntKey n)      -> AttributePair <$!> atMay kvs (n - 1)
  Nothing              -> Nothing

setKey :: forall e. LuaError e
       => [(Text, Text)] -> Maybe Key -> Maybe Attribute
       -> LuaE e ()
setKey kvs mbKey mbValue = case mbKey of
  Just (StringKey str) ->
    case break ((== str) . fst) kvs of
      (prefix, _:suffix) -> case mbValue of
        Nothing -> setNew $ prefix ++ suffix
        Just (AttributeValue value) -> setNew $ prefix ++ (str, value):suffix
        _ -> failLua "invalid attribute value"
      _  -> case mbValue of
        Nothing -> return ()
        Just (AttributeValue value) -> setNew (kvs ++ [(str, value)])
        _ -> failLua "invalid attribute value"
  Just (IntKey idx) ->
    case splitAt (idx - 1) kvs of
      (prefix, (k,_):suffix) -> setNew $ case mbValue of
        Nothing -> prefix ++ suffix
        Just (AttributePair kv) -> prefix ++ kv : suffix
        Just (AttributeValue v) -> prefix ++ (k, v) : suffix
      (prefix, []) -> case mbValue of
        Nothing -> setNew prefix
        Just (AttributePair kv) -> setNew $ prefix ++ [kv]
        _ -> failLua $ "trying to set an attribute key-value pair, "
             ++ "but got a single string instead."

  _  -> failLua "invalid attribute key"
  where
    setNew :: [(Text, Text)] -> LuaE e ()
    setNew new =
      putuserdata (nthBottom 1) (udName @e typeAttributeList) new >>= \case
        True -> return ()
        False -> failLua "failed to modify attributes list"

peekAttr :: LuaError e => Peeker e Attr
peekAttr idx = retrieving "Attr" $ liftLua (ltype idx) >>= \case
  TypeString -> (,[],[]) <$!> peekText idx -- treat string as ID
  TypeUserdata -> peekUD typeAttr idx
  TypeTable -> peekAttrTable idx
  x -> liftLua . failLua $ "Cannot get Attr from " ++ show x

-- | Helper function which gets an Attr from a Lua table.
peekAttrTable :: LuaError e => Peeker e Attr
peekAttrTable idx = do
  len' <- liftLua $ rawlen idx
  let peekClasses = peekList peekText
  if len' > 0
    then do
      ident <- peekIndexRaw 1 peekText idx
      classes <- fromMaybe [] <$!> optional (peekIndexRaw 2 peekClasses idx)
      attribs <- fromMaybe [] <$!> optional (peekIndexRaw 3 peekAttribs idx)
      return $ ident `seq` classes `seq` attribs `seq`
        (ident, classes, attribs)
    else retrieving "HTML-like attributes" $ do
      kvs <- peekKeyValuePairs peekText peekText idx
      let ident = fromMaybe "" $ lookup "id" kvs
      let classes = maybe [] T.words $ lookup "class" kvs
      let attribs = filter ((`notElem` ["id", "class"]) . fst) kvs
      return $ ident `seq` classes `seq` attribs `seq`
        (ident, classes, attribs)

mkAttr :: LuaError e => LuaE e NumResults
mkAttr = do
  attr <- ltype (nthBottom 1) >>= \case
    TypeString -> forcePeek $ do
      mident <- optional (peekText (nthBottom 1))
      mclass <- optional (peekList peekText (nthBottom 2))
      mattribs <- optional (peekAttribs (nthBottom 3))
      return (fromMaybe "" mident, fromMaybe [] mclass, fromMaybe [] mattribs)
    TypeTable  -> forcePeek $ peekAttrTable (nthBottom 1)
    TypeUserdata -> forcePeek $ peekUD typeAttr (nthBottom 1) <|> do
      attrList <- peekUD typeAttributeList (nthBottom 1)
      return ("", [], attrList)
    TypeNil    -> pure nullAttr
    TypeNone   -> pure nullAttr
    x          -> failLua $ "Cannot create Attr from " ++ show x
  pushAttr attr
  return 1

mkAttributeList :: LuaError e => LuaE e NumResults
mkAttributeList = do
  attribs <- forcePeek $ peekAttribs (nthBottom 1)
  pushUD typeAttributeList attribs
  return 1
