module Text.Pandoc.Readers.DocBook ( readDocBook ) where
import Text.Pandoc.Parsing (ParserState(..), defaultParserState)
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.XML.Light
import Data.Monoid
import Data.Char (isSpace)
import Control.Monad.State
import Control.Applicative ((<$>))

type DB = State DBState

data DBState = DBState{ dbSectionLevel :: Int }
             deriving (Read, Show)

defaultDBState :: DBState
defaultDBState = DBState { dbSectionLevel = 0 }

readDocBook :: ParserState -> String -> Pandoc
readDocBook st inp = Pandoc (Meta [] [] []) $ toList blocks
  where blocks = mconcat $ evalState (mapM parseBlock $ parseXML inp)
                             defaultDBState

parseBlock :: Content -> DB Blocks
parseBlock (Text (CData _ s _)) = if all isSpace s
                                        then return mempty
                                        else return $ plain $ text s
parseBlock (Elem e) =
  case qName (elName e) of
        "para"  -> para <$> getInlines e
        "sect1" -> sect 1
        "sect2" -> sect 2
        "sect3" -> sect 3
        "sect4" -> sect 4
        "sect5" -> sect 5
        "sect6" -> sect 6
        "title" -> return $ mempty
        _       -> innerBlocks
   where innerBlocks = mconcat <$> (mapM parseBlock $ elContent e)
         getInlines e' = (trimInlines . mconcat) <$>
                       (mapM parseInline $ elContent e')
         isTitle e' = qName (elName e') == "title"
         skipWhite (Text (CData _ s _):xs) | all isSpace s = skipWhite xs
                                           | otherwise     = xs
         skipWhite xs = xs
         sect n = case skipWhite (elContent e) of
                        ((Elem t):body)
                          | isTitle t -> do
                                  h <- header n <$> (getInlines t)
                                  b <- mconcat <$> (mapM parseBlock body)
                                  return $ h <> b
                        _        -> (header n mempty <>) <$> innerBlocks
parseBlock (CRef _) = return mempty

parseInline :: Content -> DB Inlines
parseInline (Text (CData _ s _)) = return $ text s
parseInline (Elem e) =
  case qName (elName e) of
        "emphasis" -> case lookupAttrBy (\attr -> qName attr == "role")
                           (elAttribs e) of
                             Just "strong" -> strong <$> innerInlines
                             _             -> emph <$> innerInlines
        _          -> innerInlines
   where innerInlines = (trimInlines . mconcat) <$>
                          (mapM parseInline $ elContent e)
parseInline (CRef _) = return mempty

