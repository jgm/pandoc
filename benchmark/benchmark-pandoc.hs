{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
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
-- import Text.Pandoc.Error (PandocError(..)) TODO text: restore
import Control.Monad.Except (throwError)
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.ByteString as B
import Criterion.Main
import Criterion.Types (Config(..))
import Data.List (intersect)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL

readerBench :: Pandoc
            -> String
            -> Maybe Benchmark
readerBench doc name =
  case res of
       Right (readerFun, inp) ->
          Just $ bench (name ++ " reader")
               $ nf (\i -> either (error . show) id $ runPure (readerFun i))
                 inp
       Left _ -> Nothing
  where res = runPure $ do
          (rdr, rexts) <- getReader name
          (wtr, wexts) <- getWriter name
          case (rdr, wtr) of
            (TextReader r, TextWriter w) -> do
                     setResourcePath ["../test"]
                     inp <- w def{ writerWrapText = WrapAuto
                                 , writerExtensions = wexts } doc
                     return $ (r def{ readerExtensions = rexts }, inp)
            _ -> throwError $ PandocSomeError $ "not a text format: "
                                 ++ name

getImages :: IO [(FilePath, String, BL.ByteString)] -- TODO text: replace String with MimeType
getImages = do
  ll <- BL.readFile "test/lalune.jpg"
  mv <- BL.readFile "test/movie.jpg"
  return [("lalune.jpg", "image/jpg", ll)
         ,("movie.jpg", "image/jpg", mv)]

writerBench :: Pandoc
            -> String
            -> Maybe Benchmark
writerBench doc name =
  case res of
       Right writerFun ->
          Just $ env getImages $ \imgs ->
            bench (name ++ " writer")
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
                 $ "could not get text writer for " ++ name

main :: IO ()
main = do
  args <- filter (\x -> take 1 x /= "-") <$> getArgs
  print args
  let matchReader (n, TextReader _) =
         null args || ("reader" `elem` args && n `elem` args)
      matchReader _                 = False
  let matchWriter (n, TextWriter _) =
         null args || ("writer" `elem` args && n `elem` args)
      matchWriter _                 = False
  let matchedReaders = map fst $ (filter matchReader readers
                                    :: [(String, Reader PandocPure)])
  let matchedWriters = map fst $ (filter matchWriter writers
                                    :: [(String, Writer PandocPure)])
  inp <- UTF8.toText <$> B.readFile "test/testsuite.txt"
  let opts = def
  let doc = either (error . show) id $ runPure $ readMarkdown opts inp
  let readerBs = mapMaybe (readerBench doc)
                 $ filter (/="haddock")
                 (matchedReaders `intersect` matchedWriters)
                 -- we need the corresponding writer to generate
                 -- input for the reader
  let writerBs = mapMaybe (writerBench doc) matchedWriters
  defaultMainWith defaultConfig{ timeLimit = 6.0 }
    (writerBs ++ readerBs)
