{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ViewPatterns#-}
{-
Copyright (C) 2016 Mathieu Duponchelle <mathieu.duponchelle@opencreed.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.Mallard
   Copyright   : Copyright (C) 2016 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Mallard' to 'Pandoc' document.

http://projectmallard.org/
-}

module Text.Pandoc.Readers.Mallard ( readMallard ) where

import Debug.Trace
import Data.Char (toUpper)
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.XML.Light
import Text.Pandoc.Compat.TagSoupEntity (lookupEntity)
import Data.Generics
import Data.Char (isSpace)
import Control.Monad.State
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Compat.Except
import Data.Default

type DB = ExceptT PandocError (State DBState)

data DBState = DBState{ dbSectionLevel :: Int
                      , dbMeta         :: Meta
                      , dbAcceptsMeta  :: Bool
                      , dbPage         :: Bool
                      , dbContent      :: [Content]
                      } deriving Show

instance Default DBState where
  def = DBState{ dbSectionLevel = 0
               , dbMeta = mempty
               , dbAcceptsMeta = False
               , dbPage = False
               , dbContent = [] }


readMallard :: ReaderOptions -> String -> Either PandocError Pandoc
readMallard _ inp  = (\blocks -> Pandoc (dbMeta st') (toList . mconcat $ blocks)) <$>  bs
  where (bs , st') = flip runState (def{ dbContent = tree }) . runExceptT . mapM parseBlock $ tree
        tree = normalizeTree . parseXML $ inp

-- normalize input, consolidating adjacent Text and CRef elements
normalizeTree :: [Content] -> [Content]
normalizeTree = everywhere (mkT go)
  where go :: [Content] -> [Content]
        go (Text (CData CDataRaw _ _):xs) = xs
        go (Text (CData CDataText s1 z):Text (CData CDataText s2 _):xs) =
           Text (CData CDataText (s1 ++ s2) z):xs
        go (Text (CData CDataText s1 z):CRef r:xs) =
           Text (CData CDataText (s1 ++ convertEntity r) z):xs
        go (CRef r:Text (CData CDataText s1 z):xs) =
             Text (CData CDataText (convertEntity r ++ s1) z):xs
        go (CRef r1:CRef r2:xs) =
             Text (CData CDataText (convertEntity r1 ++ convertEntity r2) Nothing):xs
        go xs = xs

convertEntity :: String -> String
convertEntity e = maybe (map toUpper e) (:[]) (lookupEntity e)

acceptingMetadata :: DB a -> DB a
acceptingMetadata p = do
  modify (\s -> s { dbAcceptsMeta = True } )
  res <- p
  modify (\s -> s { dbAcceptsMeta = False })
  return res

checkInMeta :: Monoid a => DB () -> DB a
checkInMeta p = do
  accepts <- dbAcceptsMeta <$> get
  when accepts p
  return mempty

addMeta :: ToMetaValue a => String -> a -> DB ()
addMeta field val = modify (setMeta field val)

instance HasMeta DBState where
  setMeta field v s =  s {dbMeta = setMeta field v (dbMeta s)}
  deleteMeta field s = s {dbMeta = deleteMeta field (dbMeta s)}

getBlocks :: Element -> DB Blocks
getBlocks e =  mconcat <$> (mapM parseBlock $ elContent e)

-- Trim leading and trailing newline characters
trimNl :: String -> String
trimNl = reverse . go . reverse . go
  where go ('\n':xs) = xs
        go xs        = xs

strContentRecursive :: Element -> String
strContentRecursive = strContent .
  (\e' -> e'{ elContent = map elementToStr $ elContent e' })

elementToStr :: Content -> Content
elementToStr (Elem e') = Text $ CData CDataText (strContentRecursive e') Nothing
elementToStr x = x

isBlockElement :: Content -> Bool
isBlockElement (Elem e) = qName (elName e) `elem` blocktags
  where blocktags = ["cite","code","comment","credit","desc","example","include",
  	   "info","links","list","listing","media","note","p","page","revision",
	   "section","title"]
isBlockElement _ = False

getInlines :: Element -> DB Inlines
getInlines e' = (trimInlines . mconcat) <$> (mapM parseInline $ elContent e')

-- convenience function to get an attribute value, defaulting to ""
attrValue :: String -> Element -> String
attrValue attr elt =
  case lookupAttrBy (\x -> qName x == attr) (elAttribs elt) of
    Just z  -> z
    Nothing -> ""

-- convenience function
named :: String -> Element -> Bool
named s e = qName (elName e) == s

parseImage :: Element -> DB Inlines
parseImage e = do
  (imageUrl, attr) <-
    let atVal a = attrValue a e
        w = case atVal "width" of
              "" -> []
              d  -> [("width", d)]
        h = case atVal "depth" of
              "" -> []
              d  -> [("height", d)]
        atr = (atVal "id", words $ atVal "role", w ++ h)
    in return (atVal "src", atr)
  let getCaption el = case filterChild (\x -> named "caption" x
                                            || named "textobject" x
                                            || named "alt" x) el of
                        Nothing -> return mempty
                        Just z  -> mconcat <$> (mapM parseInline $ elContent z)
  let (caption, title) = (getCaption e, "")
  liftM (imageWith attr imageUrl title) caption

parseBlock :: Content -> DB Blocks
parseBlock (Text (CData CDataRaw _ _)) = return mempty -- DOCTYPE
parseBlock (Text (CData _ s _)) = if all isSpace s
                                     then return mempty
                                     else return $ plain $ trimInlines $ text s
parseBlock (CRef x) = return $ plain $ str $ map toUpper x
parseBlock (Elem e) =
  case qName (elName e) of
        "?xml"     -> return mempty
	"cite"     -> return mempty
	"code"     -> return $ codeBlock $ strContent e
	"comment"  -> return mempty
	"credit"   -> checkInMeta getCredit
	"desc"     -> checkInMeta getDesc
	"example"  -> parseBlockquote
	"include"  -> return mempty
	"info"     -> metaBlock
	"links"    -> parseLinks
        "list"     -> bulletList <$> listitems
	"media"    -> parseMedia
	"listing"  -> parseListing
        "note"     -> blockQuote . (para (strong $ str "Note") <>)
                        <$> getBlocks e
	"p"        -> parseMixed para (elContent e)
        "page"     -> modify (\st -> st{ dbPage = True }) >> sect 0
	"revision" -> checkInMeta getRevision
        "section"  -> gets dbSectionLevel >>= sect . (+1)
	"title"    -> return mempty
        _          -> getBlocks e
   where getCredit = do
  	    authorname <- case filterChild (named "name") e of
               Just t -> getInlines t
               Nothing -> return mempty
	    addMeta "author-name" authorname
  	    authoremail <- case filterChild (named "email") e of
               Just t -> getInlines t
               Nothing -> return mempty
	    addMeta "author-email" authoremail
  	    date <- case filterChild (named "years") e of
               Just t -> getInlines t
               Nothing -> return mempty
	    addMeta "authoring-date" date
   	 getDesc = do
	    desc <- getInlines e
	    addMeta "description" (desc)
   	 listitems = mapM getBlocks $ filterChildren (named "item") e
   	 parseBlockquote = do
            attrib <- case filterChild (named "cite") e of
                             Nothing  -> return mempty
                             Just z   -> (para . (str "— " <>) . mconcat)
                                         <$> (mapM parseInline $ elContent z)
            contents <- getBlocks e
            return $ blockQuote (contents <> attrib)
	 parseInfoLink t = case (attrValue "type" t) of
			       "guide"   -> do
					    addMeta "guide-xref" (str(attrValue "xref" t))
                                            addMeta "guide-group" (str(attrValue "group" t))
			       "next"    -> do
                                            addMeta "next-xref" (str(attrValue "xref" t))
			       "seealso" -> do
					    addMeta "seealso-xref" (str(attrValue "xref" t))
			       _         -> return mempty
         metaBlock = do
		     mapM parseInfoLink $ filterChildren (named "link") e
		     acceptingMetadata (getBlocks e) >> return mempty
	 parseLinks = do
  	    headerText <- case filterChild (named "title") e of
               Just t -> headerWith ("",[],[]) 2 <$> getInlines t
               Nothing -> return mempty
	    let title = ""
	    let xref = attrValue "groups" e
	    let backlink = link xref title mempty
	    return (headerText <> (para (backlink)))
	 getRevision = do
	    addMeta "revision-date" (str(attrValue "date" e))
	    addMeta "revision-version" (str(attrValue "version" e))
	    addMeta "revision-status" (str(attrValue "status" e))
	 parseMedia = do
	    case attrValue "type" e of
	    	"image" -> para <$> parseImage e
		_ -> getBlocks e
         parseListing = do
           return $ codeBlockWith ("Hello", [], [])
                  $ trimNl $ strContentRecursive e
         parseMixed container conts = do
           let (ils,rest) = break isBlockElement conts
           ils' <- (trimInlines . mconcat) <$> mapM parseInline ils
           let p = if ils' == mempty then mempty else container ils'
           case rest of
                 []     -> return p
                 (r:rs) -> do
                    b <- parseBlock r
                    x <- parseMixed container rs
                    return $ p <> b <> x
         sect n = do ispage <- gets dbPage
                     let n' = if ispage || n == 0 then n + 1 else n
                     headerText <- case filterChild (named "title") e `mplus`
                                        (filterChild (named "info") e >>=
                                            filterChild (named "title")) of
                                      Just t -> getInlines t
                                      Nothing -> return mempty
		     case n of
		     	0 -> do addMeta "title" headerText
			_ -> return mempty
                     modify $ \st -> st{ dbSectionLevel = n }
                     b <- getBlocks e
                     let ident = attrValue "id" e
                     modify $ \st -> st{ dbSectionLevel = n - 1 }
                     return $ headerWith (ident,[],[]) n' headerText <> b

parseInline :: Content -> DB Inlines
parseInline (Text (CData _ s _)) = return $ text s
parseInline (CRef ref) =
  return $ maybe (text $ map toUpper ref) (text . (:[])) $ lookupEntity ref
parseInline (Elem e) =
  case qName (elName e) of
  	"app"      -> emph <$> innerInlines
	"cmd"      -> strong <$> innerInlines
	"file"	   -> return mempty
	"gui"	   -> strong <$> innerInlines
        "em"       -> case attrValue "style" e of
                             "bold"   -> strong <$> innerInlines
                             "strong" -> strong <$> innerInlines
                             "strikethrough" -> strikeout <$> innerInlines
                             _        -> emph <$> innerInlines
	"input"    -> spanWith nullAttr <$> innerInlines
  	"link" 	   -> getLink
        _          -> innerInlines
   where innerInlines = (trimInlines . mconcat) <$>
                          (mapM parseInline $ elContent e)
	 getLink = do 
	 	   acc <- gets dbAcceptsMeta
	 	   case acc of
		         False -> link (getHRef) "" <$> innerInlines
		         True  -> return mempty
	 getHRef = case findAttr (QName "href" Nothing Nothing) e of
                          Just h -> h
                          _      -> attrValue "xref" e
		
