-- Create pandoc.1 man page from README
import Text.Pandoc
import Data.ByteString.UTF8 (toString, fromString)
import Data.Char (toUpper)
import qualified Data.ByteString as B
import Control.Monad
import System.FilePath

main = do
  rmContents <- liftM toString $ B.readFile "README"
  let (Pandoc meta blocks) = readMarkdown defaultParserState rmContents
  let newBlocks = removeWrapperSect blocks
  manTemplate <- liftM toString $ B.readFile "manpage.template"
  let opts = defaultWriterOptions{ writerStandalone = True
                                 , writerTemplate = manTemplate }
  let manPage = writeMan opts $
                processWith (concatMap removeLinks) $
                processWith capitalizeHeaders $
                Pandoc meta newBlocks
  B.writeFile ("man" </> "man1" </> "pandoc.1") $ fromString manPage

removeLinks :: Inline -> [Inline]
removeLinks (Link l _) = l
removeLinks x = [x]

capitalizeHeaders :: Block -> Block
capitalizeHeaders (Header 1 xs) = Header 1 $ processWith capitalize xs
capitalizeHeaders x = x

capitalize :: Inline -> Inline
capitalize (Str xs) = Str $ map toUpper xs
capitalize x = x

removeWrapperSect :: [Block] -> [Block]
removeWrapperSect (Header 1 [Str "Wrappers"]:xs) =
  dropWhile notLevelOneHeader xs
    where notLevelOneHeader (Header 1 _) = False
          notLevelOneHeader _ = True
removeWrapperSect (x:xs) = x : removeWrapperSect xs
removeWrapperSect [] = []
