module Main where

import Data.List (isInfixOf, intercalate, isPrefixOf)
import Data.Maybe (isNothing)

import Control.Monad (unless, guard)
import Control.Exception (tryJust, bracket)

import System.IO (stderr)
import System.IO.Error (isDoesNotExistError)
import System.Environment ( getArgs, getProgName )
import Prelude hiding ( putStr, putStrLn, writeFile, readFile, getContents )
import System.IO.UTF8
import System.Exit (ExitCode (..), exitWith)
import System.FilePath
import System.Directory
import System.Process (readProcessWithExitCode)


run :: FilePath -> [String] -> IO (Either String String)
run file opts = do
  (code, out, err) <- readProcessWithExitCode file opts ""
  let msg = out ++ err
  case code of
    ExitFailure _ -> return $ Left  $! msg
    ExitSuccess   -> return $ Right $! msg

parsePandocArgs :: [String] -> IO (Maybe ([String], String))
parsePandocArgs args = do
  result <- run "pandoc" $ ["--dump-args"] ++ args
  return $ either (const Nothing) (parse . map trim . lines) result
  where parse []         = Nothing
        parse ("-":[])   = Just ([], "stdin") -- no output or input
        parse ("-":x:xs) = Just (x:xs, dropExtension x) -- no output
        parse ( x :xs)   = Just (xs,   dropExtension x) -- at least output
        --trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
        trim = takeWhile (/='\r') . dropWhile (=='\r')

runPandoc :: [String] -> FilePath -> IO (Either String FilePath)
runPandoc inputs output = do
  let texFile = replaceExtension output "tex"
  result <- run "pandoc" $
    ["-s", "--no-wrap", "-r", "markdown", "-w", "latex"]
    ++ inputs ++ ["-o", texFile]
  return $ either Left (const $ Right texFile) result

runLatexRaw :: FilePath -> IO (Either (Either String String) FilePath)
runLatexRaw file = do
  -- we ignore the ExitCode because pdflatex always fails the first time
  run "pdflatex" ["-interaction=batchmode", "-output-directory",
    takeDirectory file, dropExtension file] >> return ()
  let pdfFile = replaceExtension file "pdf"
  let logFile = replaceExtension file "log"
  txt <- tryJust (guard . isDoesNotExistError) (readFile logFile)
  let  checks = checkLatex $ either (const "") id txt
  case checks of
  -- err  , bib , ref , msg
    (True , _    , _   , msg) -> return $ Left $ Left msg   -- failure
    (False, True , _   , msg) -> runBibtex file >>
                                (return $ Left $ Right msg) -- citations
    (False, _    , True, msg) -> return $ Left $ Right msg  -- references
    (False, False, False, _ ) -> return $ Right pdfFile     -- success

runLatex :: FilePath -> IO (Either String FilePath)
runLatex file = step 3
  where
  step n = do
    result <- runLatexRaw file
    case result of
      Left (Left err) -> return $ Left err
      Left (Right _) | n > 1  -> step (n-1 :: Int)
      Left (Right msg) -> return $ Left msg
      Right pdfFile   -> return $ Right pdfFile

checkLatex :: String -> (Bool, Bool, Bool, String)
checkLatex ""  = (True, False, False, "Could not read log file")
checkLatex txt = (err , bib, ref, unlines $! msgs ++ tips)
  where
  xs `oneOf` x = any (flip isInfixOf x) xs
  msgs = filter (oneOf ["Error:", "Warning:"]) (lines txt)
  tips = checkPackages msgs
  err = any (oneOf ["LaTeX Error:", "Latex Error:"]) msgs
  bib = any (oneOf ["Warning: Citation"
                   ,"Warning: There were undefined citations"]) msgs
  ref = any (oneOf ["Warning: Reference"
                   ,"Warning: Label"
                   ,"Warning: There were undefined references"
                   ,"--toc", "--table-of-contents"]) msgs

checkPackages :: [String] -> [String]
checkPackages = concatMap chks
  where -- for each message, search 'pks' for matches and give a hint
  chks x = concatMap (chk x) pks
  chk x (k,v) = if sub k `isInfixOf` x then tip k v else []
  sub k   = "`" ++ k ++ ".sty' not found"
  tip k v = ["Please install the '" ++ k ++
             "' package from CTAN:", "  " ++ v]
  pks = [("ucs"
         ,"http://www.ctan.org/tex-archive/macros/latex/contrib/unicode/")
        ,("ulem"
         ,"http://www.ctan.org/tex-archive/macros/latex/contrib/misc/")
        ,("graphicx"
         ,"http://www.ctan.org/tex-archive/macros/latex/required/graphics/")
        ,("fancyhdr"
         ,"http://www.ctan.org/tex-archive/macros/latex/contrib/fancyhdr/")
        ,("array"
         ,"http://www.ctan.org/tex-archive/macros/latex/required/tools/")]

runBibtex :: FilePath -> IO (Either String FilePath)
runBibtex file = do
  let auxFile = replaceExtension file "aux"
  result <- run "bibtex" [auxFile]
  return $ either Left (const $ Right auxFile) result

exit :: String -> IO a
exit x = do
  progName <- getProgName
  hPutStrLn stderr $ progName ++ ": " ++ x
  exitWith $ ExitFailure 1

saveStdin :: FilePath -> IO (Either String FilePath)
saveStdin file = do
  text <- getContents
  writeFile file text
  fileExist <- doesFileExist file
  case fileExist of
    False -> return $ Left $! "Could not create " ++ file
    True  -> return $ Right file

saveOutput :: FilePath -> FilePath -> IO ()
saveOutput input output = do
  copyFile input output
  hPutStrLn stderr $! "Created " ++ output

main :: IO ()
main = bracket
  -- acquire resource
  (do dir <- getTemporaryDirectory
      let tmp = dir </> "pandoc"
      createDirectoryIfMissing True tmp
      return tmp)

  -- release resource
  ( \tmp -> removeDirectoryRecursive tmp)

  -- run computation
  $ \tmp -> do
    -- check for executable files
    let execs = ["pandoc", "pdflatex", "bibtex"]
    paths <- mapM findExecutable execs
    let miss = map snd $ filter (isNothing . fst) $ zip paths execs
    unless (null miss) $ exit $! "Could not find " ++ intercalate ", " miss
    args <- getArgs
    -- check for invalid arguments and print help message if needed
    let goodopts = ["-f","-r","-N", "-p","-R","-H","-B","-A", "-C","-o"]
    let goodoptslong = ["--from","--read","--strict",
                   "--preserve-tabs","--tab-stop","--parse-raw",
                   "--toc","--table-of-contents",
                   "--number-sections","--include-in-header",
                   "--include-before-body","--include-after-body",
                   "--custom-header","--output"]
    let isOpt ('-':_) = True
        isOpt _       = False
    let opts = filter isOpt args
    -- note that a long option can come in this form: --opt=val
    let isGoodopt x = x `elem` (goodopts ++ goodoptslong) ||
                      any (\o -> (o ++ "=") `isPrefixOf` x) goodoptslong
    unless (all isGoodopt opts) $ do
      (code, out, _err) <- readProcessWithExitCode "pandoc" ["--help"] ""
      putStrLn "markdown2pdf [OPTIONS] [FILES]\nOptions:"
      putStr $ unlines $
               filter (\l -> any (`isInfixOf` l) goodoptslong) $ lines out
      exitWith code

    -- parse arguments
    -- if no input given, use 'stdin'
    pandocArgs <- parsePandocArgs args
    (input, output) <- case pandocArgs of
      Nothing      -> exit "Could not parse arguments"
      Just ([],out) -> do
        stdinFile <- saveStdin (replaceDirectory (takeBaseName out) tmp)
        case stdinFile of
          Left err  -> exit err
          Right f   -> return ([f], out)
      -- no need because we'll pass all arguments to pandoc
      Just (_ ,out) -> return ([], out)
    -- run pandoc
    pandocRes <- runPandoc (input ++ args) $ replaceDirectory output tmp
    case pandocRes of
      Left err -> exit err
      Right texFile  -> do
        -- run pdflatex
        latexRes <- runLatex texFile
        case latexRes of
          Left err      -> exit err
          Right pdfFile -> do
            -- save the output creating a backup if necessary
            saveOutput pdfFile $
              replaceDirectory pdfFile (takeDirectory output)

