module Main where

import Data.List (isInfixOf, intercalate, isPrefixOf)
import Data.Maybe (isNothing)
import qualified Data.ByteString as BS
import Codec.Binary.UTF8.String (decodeString, encodeString)
import Data.ByteString.UTF8 (toString)
import Control.Monad (unless, guard, liftM, when)
import Control.Concurrent (putMVar, takeMVar, newEmptyMVar, forkIO)
import Control.Exception (tryJust, bracket, evaluate)

import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Environment ( getArgs, getProgName )
import qualified Text.Pandoc.UTF8 as UTF8
import System.Exit (ExitCode (..), exitWith)
import System.FilePath
import System.Directory
import System.Process

-- A variant of 'readProcessWithExitCode' that does not
-- cause an error if the output is not UTF-8. (Copied
-- with slight variants from 'System.Process'.)
readProcessWithExitCode'
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode' cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }

    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- liftM toString $ BS.hGetContents outh
    _ <- forkIO $ evaluate (length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err  <- liftM toString $ BS.hGetContents errh
    _ <- forkIO $ evaluate (length err) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, out, err)

run :: FilePath -> [String] -> IO (Either String String)
run file opts = do
  (code, out, err) <- readProcessWithExitCode' (encodeString file)
                        (map encodeString opts) ""
  let msg = out ++ err
  case code of
    ExitFailure _ -> return $ Left  $! msg
    ExitSuccess   -> return $ Right $! msg

parsePandocArgs :: [String] -> IO (Maybe ([String], String))
parsePandocArgs args = do
  result <- run "pandoc" $ ["--dump-args"] ++ args
  return $ either error (parse . map trim . lines) result
  where parse []         = Nothing
        parse ("-":[])   = Just ([], "stdin") -- no output or input
        parse ("-":x:xs) = Just (x:xs, dropExtension x) -- no output
        parse ( x :xs)   = Just (xs,   dropExtension x) -- at least output
        --trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
        trim = takeWhile (/='\r') . dropWhile (=='\r')

runPandoc :: [String] -> FilePath -> IO (Either String FilePath)
runPandoc inputsAndArgs output = do
  let texFile = addExtension output "tex"
  result <- run "pandoc" $
    ["-s", "--no-wrap", "-r", "markdown", "-w", "latex"]
    ++ inputsAndArgs ++ ["-o", texFile]
  return $ either Left (const $ Right texFile) result

runLatexRaw :: String -> FilePath -> IO (Either (Either String String) FilePath)
runLatexRaw latexProgram file = do
  -- we ignore the ExitCode because pdflatex always fails the first time
  run latexProgram ["-halt-on-error", "-interaction", "nonstopmode",
    "-output-directory", takeDirectory file, dropExtension file] >> return ()
  let pdfFile = replaceExtension file "pdf"
  let logFile = replaceExtension file "log"
  txt <- tryJust (guard . isDoesNotExistError)
         (liftM toString $ BS.readFile logFile)
  let  checks = checkLatex $ either (const "") id txt
  case checks of
  -- err  , bib , ref , msg
    (True , _    , _   , msg) -> return $ Left $ Left msg   -- failure
    (False, True , _   , msg) -> runBibtex file >>
                                (return $ Left $ Right msg) -- citations
    (False, _    , True, msg) -> return $ Left $ Right msg  -- references
    (False, False, False, _ ) -> return $ Right pdfFile     -- success

runLatex :: String -> FilePath -> IO (Either String FilePath)
runLatex latexProgram file = step 3
  where
  step n = do
    result <- runLatexRaw latexProgram file
    case result of
      Left (Left err) -> return $ Left err
      Left (Right _) | n > 1  -> step (n-1 :: Int)
      Right _ | n > 2 -> step (n-1 :: Int)
      Left (Right msg) -> return $ Left msg
      Right pdfFile   -> return $ Right pdfFile

checkLatex :: String -> (Bool, Bool, Bool, String)
checkLatex ""  = (True, False, False, "Could not read log file")
checkLatex txt = (err , bib, ref, unlines $! msgs ++ tips)
  where
  xs `oneOf` x = any (flip isInfixOf x) xs
  msgs = dropWhile (not . errorline) $ lines txt
  errorline ('!':_) = True
  errorline _ = False
  tips = checkPackages msgs
  err = any (oneOf ["!", "LaTeX Error:", "Latex Error:"]) msgs
  bib = any (oneOf ["Warning: Citation"
                   ,"Warning: There were undefined citations"]) msgs
  ref = any (oneOf ["Warning: Reference"
                   ,"Warning: Label"
                   ,"Warning: There were undefined references"
                   ]) msgs

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
  UTF8.hPutStrLn stderr $ progName ++ ": " ++ x
  exitWith $ ExitFailure 1

saveStdin :: FilePath -> IO (Either String FilePath)
saveStdin file = do
  text <- liftM toString $ BS.getContents
  UTF8.writeFile file text
  fileExist <- doesFileExist (encodeString file)
  case fileExist of
    False -> return $ Left $! "Could not create " ++ file
    True  -> return $ Right file

saveOutput :: FilePath -> FilePath -> IO ()
saveOutput input output = do
  copyFile (encodeString input) (encodeString output)
  UTF8.hPutStrLn stderr $! "Created " ++ output

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
    args <- liftM (map decodeString) getArgs
    -- check for invalid arguments and print help message if needed
    let goodopts = ["-f","-r","-N", "-p","-R","-H","-B","-A", "-C","-o","-V"]
    let goodoptslong = ["--from","--read","--strict",
                   "--preserve-tabs","--tab-stop","--parse-raw",
                   "--toc","--table-of-contents", "--xetex", "--luatex",
                   "--number-sections","--include-in-header",
                   "--include-before-body","--include-after-body",
                   "--custom-header","--output",
                   "--template", "--variable",
                   "--csl", "--bibliography", "--data-dir", "--listings"]
    let isOpt ('-':_) = True
        isOpt _       = False
    let opts = filter isOpt args
    -- note that a long option can come in this form: --opt=val
    let isGoodopt x = x `elem` (goodopts ++ goodoptslong) ||
                      any (\o -> (o ++ "=") `isPrefixOf` x) goodoptslong
    let markdown2pdfOpts = ["--xetex","--luatex"]
    unless (all isGoodopt opts) $ do
      (code, out, _err) <- readProcessWithExitCode "pandoc" ["--help"] ""
      UTF8.putStrLn "markdown2pdf [OPTIONS] [FILES]\nOptions:"
      UTF8.putStr $ unlines $
                 filter (\l -> any (`isInfixOf` l) goodoptslong) (lines out)
                 ++ map (replicate 24 ' ' ++) markdown2pdfOpts
      exitWith code

    let args' = filter (`notElem` markdown2pdfOpts) args

    -- check for executable files
    let latexProgram = if "--xetex" `elem` opts
                          then "xelatex"
                          else if "--luatex" `elem` opts
                                  then "lualatex"
                                  else "pdflatex"
    let execs = ["pandoc", latexProgram, "bibtex"]
    paths <- mapM findExecutable execs
    let miss = map snd $ filter (isNothing . fst) $ zip paths execs
    unless (null miss) $ exit $! "Could not find " ++ intercalate ", " miss

    -- parse arguments
    -- if no input given, use 'stdin'
    pandocArgs <- parsePandocArgs args'
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
    pandocRes <- runPandoc (input ++ args') $ replaceDirectory output tmp
    case pandocRes of
      Left err -> exit err
      Right texFile  -> do
        -- run pdflatex
        latexRes <- runLatex latexProgram texFile
        case latexRes of
          Left err      -> exit err
          Right pdfFile -> do
            -- save the output creating a backup if necessary
            saveOutput pdfFile $
              replaceDirectory pdfFile (takeDirectory output)

