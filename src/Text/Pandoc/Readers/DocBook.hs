module Text.Pandoc.Readers.DocBook ( readDocBook ) where
import Text.Pandoc.Parsing (ParserState(..))
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.XML.Light
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Char (isSpace)
import Control.Monad.State
import Control.Applicative ((<$>))

type DB = State DBState

data DBState = DBState{ dbSectionLevel :: Int
                      , dbQuoteType    :: QuoteType
                      , dbDocTitle     :: Inlines
                      , dbDocAuthors   :: [Inlines]
                      , dbDocDate      :: Inlines
                      } deriving Show

readDocBook :: ParserState -> String -> Pandoc
readDocBook st inp = setTitle (dbDocTitle st')
                   $ setAuthors (dbDocAuthors st')
                   $ setDate (dbDocDate st')
                   $ doc $ mconcat bs
  where (bs, st') = runState (mapM parseBlock $ parseXML inp)
                             DBState{ dbSectionLevel = 0
                                    , dbQuoteType = DoubleQuote
                                    , dbDocTitle = mempty
                                    , dbDocAuthors = []
                                    , dbDocDate = mempty
                                    }

parseBlock :: Content -> DB Blocks
parseBlock (Text (CData CDataRaw _ _)) = return mempty -- DOCTYPE
parseBlock (Text (CData _ s _)) = if all isSpace s
                                     then return mempty
                                     else return $ plain $ text s
parseBlock (Elem e) =
  case qName (elName e) of
        "para"  -> para <$> getInlines e
        "blockquote" -> blockQuote <$> getBlocks e
        "sect1" -> sect 1
        "sect2" -> sect 2
        "sect3" -> sect 3
        "sect4" -> sect 4
        "sect5" -> sect 5
        "sect6" -> sect 6
        "section" -> gets dbSectionLevel >>= sect . (+1)
        "itemizedlist" -> bulletList <$> listitems
        "orderedlist" -> orderedList <$> listitems -- TODO list attributes
        "articleinfo" -> getTitle >> getAuthors >> getDate >> return mempty
        "programlisting" -> return $ codeBlock $ strContent e  -- TODO attrs
        "?xml"  -> return mempty
        _       -> getBlocks e
   where getBlocks e' =  mconcat <$> (mapM parseBlock $ elContent e')
         getInlines e' = (trimInlines . mconcat) <$>
                       (mapM parseInline $ elContent e')
         isTitle e' = qName (elName e') == "title"
         skipWhite (Text (CData _ s _):xs) | all isSpace s = skipWhite xs
                                           | otherwise     = xs
         skipWhite xs = xs
         listitems = mapM getBlocks $ findChildren (unqual "listitem") e
         getTitle = case findChild (unqual "title") e of
                         Just t  -> do
                            tit <- getInlines t
                            modify $ \st -> st{dbDocTitle = tit}
                         Nothing -> return ()
         getAuthors = do
                      auths <- mapM getInlines
                               $ findChildren (unqual "author") e
                      modify $ \st -> st{dbDocAuthors = auths}
         getDate = case findChild (unqual "date") e of
                         Just t  -> do
                            dat <- getInlines t
                            modify $ \st -> st{dbDocDate = dat}
                         Nothing -> return ()
         sect n = case skipWhite (elContent e) of
                        ((Elem t):body)
                          | isTitle t -> do
                                  h <- header n <$> (getInlines t)
                                  modify $ \st -> st{ dbSectionLevel = n }
                                  b <- mconcat <$> (mapM parseBlock body)
                                  modify $ \st -> st{ dbSectionLevel = n - 1 }
                                  return $ h <> b
                        body      -> do
                                  let h = header n mempty
                                  modify $ \st -> st{ dbSectionLevel = n }
                                  b <- mconcat <$> (mapM parseBlock body)
                                  modify $ \st -> st{ dbSectionLevel = n - 1 }
                                  return $ h <> b
parseBlock (CRef _) = return mempty

parseInline :: Content -> DB Inlines
parseInline (Text (CData _ s _)) = return $ text s
parseInline (Elem e) =
  case qName (elName e) of
        "subscript" -> subscript <$> innerInlines
        "superscript" -> superscript <$> innerInlines
        "quote" -> do
            qt <- gets dbQuoteType
            let qt' = if qt == SingleQuote then DoubleQuote else SingleQuote
            modify $ \st -> st{ dbQuoteType = qt' }
            contents <- innerInlines
            modify $ \st -> st{ dbQuoteType = qt }
            return $ if qt == SingleQuote
                        then singleQuoted contents
                        else doubleQuoted contents
        "literal" -> return $ code $ strContent e -- TODO attrs
        "ulink" -> link
            (fromMaybe "" (lookupAttrBy (\attr -> qName attr == "url")
              (elAttribs e))) "" <$> innerInlines
        "emphasis" -> case lookupAttrBy (\attr -> qName attr == "role")
                           (elAttribs e) of
                             Just "strong" -> strong <$> innerInlines
                             _             -> emph <$> innerInlines
        "footnote" -> (note . mconcat) <$> (mapM parseBlock $ elContent e)
        _          -> innerInlines
   where innerInlines = (trimInlines . mconcat) <$>
                          (mapM parseInline $ elContent e)
parseInline (CRef _) = return mempty

