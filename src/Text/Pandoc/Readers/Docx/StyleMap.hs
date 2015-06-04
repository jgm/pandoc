module Text.Pandoc.Readers.Docx.StyleMap (  StyleMaps(..)
                                          , defaultStyleMaps
                                          , getStyleMaps
                                          , getStyleId
                                          , hasStyleName
                                          ) where

import           Text.XML.Light
import           Text.Pandoc.Readers.Docx.Util
import           Control.Monad.State
import           Data.Char  (toLower)
import qualified Data.Map                      as M

newtype ParaStyleMap = ParaStyleMap ( M.Map String String )
newtype CharStyleMap = CharStyleMap ( M.Map String String )

class StyleMap a where
  alterMap :: (M.Map String String -> M.Map String String) -> a -> a
  getMap :: a -> M.Map String String

instance StyleMap ParaStyleMap where
  alterMap f (ParaStyleMap m) = ParaStyleMap $ f m
  getMap (ParaStyleMap m) = m

instance StyleMap CharStyleMap where
  alterMap f (CharStyleMap m) = CharStyleMap $ f m
  getMap (CharStyleMap m) = m

insert :: (StyleMap a) => Maybe String -> Maybe String -> a -> a
insert (Just k) (Just v) m = alterMap (M.insert k v) m
insert _ _ m = m

getStyleId :: (StyleMap a) => String -> a -> String
getStyleId s = M.findWithDefault (filter (/=' ') s) (map toLower s) . getMap

hasStyleName :: (StyleMap a) => String -> a -> Bool
hasStyleName styleName = M.member (map toLower styleName) . getMap

data StyleMaps = StyleMaps { sNameSpaces   :: NameSpaces
                           , sParaStyleMap :: ParaStyleMap
                           , sCharStyleMap :: CharStyleMap
                           }

data StyleType = ParaStyle | CharStyle

defaultStyleMaps :: StyleMaps
defaultStyleMaps = StyleMaps { sNameSpaces = []
                             , sParaStyleMap = ParaStyleMap M.empty
                             , sCharStyleMap = CharStyleMap M.empty
                             }

type StateM a = State StyleMaps a

getStyleMaps :: Element -> StyleMaps
getStyleMaps docElem = execState genStyleMap state'
    where
    state' = defaultStyleMaps {sNameSpaces = elemToNameSpaces docElem}
    genStyleItem e = do
      styleType <- getStyleType e
      styleId <- getAttrStyleId e
      nameValLowercase <- fmap (map toLower) `fmap` getNameVal e
      case styleType of
        Just ParaStyle -> modParaStyleMap $ insert nameValLowercase styleId
        Just CharStyle -> modCharStyleMap $ insert nameValLowercase styleId
        _              -> return ()
    genStyleMap = do
      style <- elemName' "style"
      let styles = findChildren style docElem
      forM_ styles genStyleItem

modParaStyleMap :: (ParaStyleMap -> ParaStyleMap) -> StateM ()
modParaStyleMap f = modify $ \s ->
  s {sParaStyleMap = f $ sParaStyleMap s}

modCharStyleMap :: (CharStyleMap -> CharStyleMap) -> StateM ()
modCharStyleMap f = modify $ \s ->
  s {sCharStyleMap = f $ sCharStyleMap s}

getStyleType :: Element -> StateM (Maybe StyleType)
getStyleType e = do
  styleTypeStr <- getAttrType e
  case styleTypeStr of
    Just "paragraph" -> return $ Just ParaStyle
    Just "character" -> return $ Just CharStyle
    _                -> return   Nothing

getAttrType :: Element -> StateM (Maybe String)
getAttrType el = do
  name <- elemName' "type"
  return $ findAttr name el

getAttrStyleId :: Element -> StateM (Maybe String)
getAttrStyleId el = do
  name <- elemName' "styleId"
  return $ findAttr name el

getNameVal :: Element -> StateM (Maybe String)
getNameVal el = do
  name <- elemName' "name"
  val <- elemName' "val"
  return $ findChild name el >>= findAttr val

elemName' :: String -> StateM QName
elemName' name = do
  namespaces <- gets sNameSpaces
  return $ elemName namespaces "w" name
