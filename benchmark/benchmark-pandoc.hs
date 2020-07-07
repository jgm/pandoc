{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2012-2019 John MacFarlane <jgm@berkeley.edu>

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
import Prelude
import Text.Pandoc
import Text.Pandoc.MIME
import Control.Monad.Except (throwError, liftIO)
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.ByteString as B
import qualified Data.Text as T
import Criterion.Main
import Criterion.Types (Config(..))
import Data.List (intersect)
import Data.Maybe (mapMaybe, catMaybes)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL

data Input = InputText {unInputText :: T.Text}
           | InputBS {unInputBS :: BL.ByteString}

readerBench :: Pandoc
            -> T.Text
            -> IO (Maybe Benchmark)
readerBench doc name = do
  let (rdr, rexts) = either (error . show) id . runPure $ getReader name
  res <- runIO $ do
          (wtr, wexts) <- getWriter name
          case (rdr, wtr) of
            (TextReader r, TextWriter w) -> do
                     setResourcePath ["./test"]
                     inp <- w def{ writerWrapText = WrapAuto
                                 , writerExtensions = wexts } doc
                     return (r def{ readerExtensions = rexts } . unInputText, InputText inp)
            (ByteStringReader r, ByteStringWriter w) -> do
                     setResourcePath ["./test"]
                     tmpl <- Just <$> compileDefaultTemplate name
                     inp <- w def{ writerWrapText = WrapAuto
                                 , writerExtensions = wexts
                                 , writerTemplate = tmpl } doc
                     liftIO $ BL.writeFile "/tmp/test.odt" inp
                     return (r def{ readerExtensions = rexts } . unInputBS, InputBS inp)
            _ -> throwError $ PandocSomeError $ "text/bytestring format mismatch: "
                                 <> name
  return $ case res of
       Right (readerFun, inp) ->
          Just $ bench (T.unpack $ name <> " reader")
               $ nf (\i -> either (error . show) id $ runPure (readerFun i))
                 inp
       Left _ -> Nothing

getImages :: IO [(FilePath, MimeType, BL.ByteString)]
getImages = do
  ll <- BL.readFile "test/lalune.jpg"
  mv <- BL.readFile "test/movie.jpg"
  return [("lalune.jpg", "image/jpg", ll)
         ,("movie.jpg", "image/jpg", mv)]

writerBench :: Pandoc
            -> T.Text
            -> Maybe Benchmark
writerBench doc name =
  case res of
       Right writerFun ->
          Just $ env getImages $ \imgs ->
            bench (T.unpack $ name <> " writer")
               $ nf (\d -> either (error . show) id $
                            runPure (do mapM_
                                          (\(fp, mt, bs) ->
                                              insertMedia fp (Just mt) bs)
                                          imgs
                                        writerFun d)) doc
       Left _ -> Nothing
  where res = runPure $ do
          (wtr, wexts) <- getWriter name
          case wtr of
            TextWriter w ->
              return $ w def{ writerExtensions = wexts }
            _ -> throwError $ PandocSomeError
                 $ "could not get text writer for " <> name

main :: IO ()
main = do
  args <- filter (\x -> T.take 1 x /= "-") . fmap T.pack <$> getArgs
  print args
  let matchReader (n, _) =
         null args || ("reader" `elem` args && n `elem` args)
      matchWriter (n, TextWriter _) =
         null args || ("writer" `elem` args && n `elem` args)
      matchWriter _                 = False
      allWriters = map fst (writers :: [(T.Text, Writer PandocPure)])
      matchedReaders = map fst (filter matchReader readers
                                    :: [(T.Text, Reader PandocPure)])
      matchedWriters = map fst (filter matchWriter writers
                                    :: [(T.Text, Writer PandocPure)])
  inp <- UTF8.toText <$> B.readFile "test/testsuite.txt"
  let opts = def
  let doc = either (error . show) id $ runPure $ readMarkdown opts inp
  readerBs <- fmap catMaybes
              $ mapM (readerBench doc)
              $ filter (/="haddock")
              (matchedReaders `intersect` allWriters)
                 -- we need the corresponding writer to generate
                 -- input for the reader
  let writerBs = mapMaybe (writerBench doc) matchedWriters
  defaultMainWith defaultConfig{ timeLimit = 6.0 }
    (writerBs ++ readerBs)
