import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import System.FilePath (combine, joinPath, takeFileName)
import System.Directory (getDirectoryContents, removeFile)
import System.IO (readFile, writeFile)
import Control.Monad (foldM)
import Data.List (isPrefixOf)

main = defaultMainWithHooks myHooks

myHooks = defaultUserHooks { postConf = myPostConf, postClean = myPostClean }

pandocPath = combine "Text" "Pandoc"

-- Builds Text/Pandoc/ASCIIMathML.hs, Text/Pandoc/Writers/S5.hs, and
-- Text/Pandoc/Writers/DefaultHeaders.hs from templates and data.
myPostConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostConf _ configFlags pkgDescription buildInfo = do
  putStrLn "Generating source files from templates..."
  fillAsciiMathMLTemplate
  fillS5WriterTemplate
  fillDefaultHeadersTemplate

-- Fill templateFile with data in dataFiles and write to outputFile.
fillTemplate :: [FilePath] -> FilePath -> FilePath -> IO ()
fillTemplate dataFiles templateFile outputFile = do
  template <- readFile (combine "templates" templateFile)
  filled <- foldM processFile template $ map (combine "templates") dataFiles
  writeTemplate (combine pandocPath outputFile) filled 

fillAsciiMathMLTemplate :: IO ()
fillAsciiMathMLTemplate =
  fillTemplate ["ASCIIMathML.js.comment", "ASCIIMathML.js.packed"] "ASCIIMathML.hs" "ASCIIMathML.hs"

fillS5WriterTemplate :: IO ()
fillS5WriterTemplate = 
  let s5Path = joinPath ["ui", "default"]
      files = map (combine s5Path) ["slides.js.comment", "slides.js.packed", "s5-core.css", 
              "framing.css", "pretty.css", "opera.css", "outline.css", "print.css"]
  in  fillTemplate files "S5.hs" (combine "Writers" "S5.hs")

fillDefaultHeadersTemplate :: IO ()
fillDefaultHeadersTemplate = do
  files <- getDirectoryContents (combine "templates" "headers") >>= 
             return . map (combine "headers") . filter (\x -> not (x `elem` [".",".."]))
  fillTemplate files "DefaultHeaders.hs" "DefaultHeaders.hs"

-- Post-clean: remove the files generated from templates.
myPostClean :: Args -> CleanFlags -> PackageDescription -> Maybe LocalBuildInfo -> IO ()
myPostClean _ _ _ _ = do
  putStrLn "Removing source files generated from templates:"
  removeGeneratedFile $ joinPath [pandocPath, "ASCIIMathML.hs"]
  removeGeneratedFile $ joinPath [pandocPath, "Writers", "S5.hs"]
  removeGeneratedFile $ joinPath [pandocPath, "Writers", "DefaultHeaders.hs"]

-- Remove file and print message.
removeGeneratedFile :: FilePath -> IO () 
removeGeneratedFile fpath = do
  putStrLn $ "  " ++ fpath
  removeFile fpath

-- Write the filled template file and print an explanatory message.
writeTemplate :: FilePath -> String -> IO ()
writeTemplate outfile contents = do
  putStrLn $ "  " ++ outfile
  writeFile outfile contents

-- Read contents of fpath and insert in template replacing @fpath@.
processFile :: String -> FilePath -> IO String
processFile template fpath = do 
  contents <- readFile fpath >>= return . show
  return $ substitute ("@" ++ takeFileName fpath ++ "@") contents template

-- Replace each occurrence of one sublist in a list with another.
substitute :: (Eq a) => [a] -> [a] -> [a] -> [a]
substitute _ _ [] = []
substitute [] _ lst = lst
substitute target replacement lst = 
    if target `isPrefixOf` lst
       then replacement ++ (substitute target replacement $ drop (length target) lst)
       else (head lst):(substitute target replacement $ tail lst)

