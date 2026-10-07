{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-
Copyright (C) 2012-2024 John MacFarlane <jgm@berkeley.edu>

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
import Text.Pandoc
import Text.Pandoc.MIME
import Text.Pandoc.Shared (stringify, stringifyInlines)
import Text.Pandoc.MediaBag (mediaItems)
import Control.DeepSeq (force)
import Control.Monad.Except (throwError)
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.ByteString as B
import qualified Data.Text as T
import Test.Tasty.Bench
-- import Gauge
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import Text.Pandoc.Format (FlavoredFormat(..))
import System.IO.Temp
import Data.Maybe

readerBench :: [(FilePath, MimeType, BL.ByteString)]
            -> Pandoc
            -> T.Text
            -> Maybe Benchmark
readerBench _ _ name
  | name `elem` ["bibtex", "biblatex", "csljson"] = Nothing
readerBench imgs doc name = either (const Nothing) Just $
  runPure $ do
    (rdr, rexts) <- getReader $ FlavoredFormat name mempty
    (wtr, wexts) <- getWriter $ FlavoredFormat name mempty
    tpl <- compileDefaultTemplate name 
    case (rdr, wtr) of
      (TextReader r, TextWriter w) -> do
        inp <- w def{ writerWrapText = WrapAuto
                    , writerExtensions = wexts
                    , writerTemplate = Just tpl } doc
        return $ bench (T.unpack name)
               $ nf (\x -> either (error . show) id $
                       runPure $ do
                         mapM_ (\(fp,mt,bs) -> insertMedia fp (Just mt) bs) imgs
                         r def x)
                    inp
      (ByteStringReader r, ByteStringWriter w) -> do
        inp <- w def{ writerWrapText = WrapAuto
                    , writerExtensions = wexts
                    , writerTemplate = Just tpl } doc
        return $ bench (T.unpack name)
               $ nf (\x -> either (error . show) id $
                       runPure $ do
                         mapM_ (\(fp,mt,bs) -> insertMedia fp (Just mt) bs) imgs
                         r def{readerExtensions = rexts} x)
                    inp
      _ -> throwError $ PandocSomeError $ "text/bytestring format mismatch: "
                           <> name

getSample :: FilePath -> IO (Pandoc, [(FilePath, MimeType, BL.ByteString)])
getSample fp = do
  inp <- UTF8.toText <$> B.readFile fp
  let opts = def
  doc' <- runIOorExplode $ do
            (_, rexts) <- getReader $ FlavoredFormat "markdown" mempty
            readMarkdown
              opts{ readerExtensions = enableExtension Ext_rebase_relative_paths
                                              rexts }
              [(fp, inp)]
  withSystemTempDirectory "pandoc-bench-resources" $ \tmpdir ->
    runIOorExplode $ do
      doc <- extractMedia tmpdir doc'
      fillMediaBag doc
      items <- mediaItems <$> getMediaBag
      pure $! (doc, items)

writerBench :: [(FilePath, MimeType, BL.ByteString)]
            -> Pandoc
            -> T.Text
            -> Maybe Benchmark
writerBench _ _ name
  | name `elem` ["bibtex", "biblatex", "csljson"] = Nothing
writerBench imgs doc name = either (const Nothing) Just $
  runPure $ do
    (wtr, wexts) <- getWriter $ FlavoredFormat name mempty
    tpl <- compileDefaultTemplate name
    let opts = def{ writerExtensions = wexts, writerTemplate = Just tpl }
    case wtr of
      TextWriter writerFun ->
        return $ bench (T.unpack name)
               $ nf (\d -> either (error . show) id $
                       runPure $ do
                         mapM_ (\(fp,mt,bs) -> insertMedia fp (Just mt) bs) imgs
                         writerFun opts d)
                    doc
      ByteStringWriter writerFun ->
        return $ bench (T.unpack name)
               $ nf (\d -> either (error . show) id $
                       runPure $ do
                         mapM_ (\(fp,mt,bs) -> insertMedia fp (Just mt) bs) imgs
                         writerFun opts d)
                    doc

-- | A large inline sequence exercising both the common constructors
-- and the ones 'stringify' special-cases ('Quoted', 'Note', 'Cite').
bigInlines :: [Inline]
bigInlines = concat $ replicate 1000
  [ Str "Lorem", Space
  , Emph [Str "ipsum", Space, Strong [Str "dolor"]], Space
  , Quoted DoubleQuote [Str "sit", Space, Str "amet"], SoftBreak
  , Link nullAttr [Str "consectetur"] ("https://example.com", "title")
  , Space, Code nullAttr "adipiscing", Space
  , Note [Para [Str "footnote", Space, Emph [Str "text"]]]
  , Cite [] [Str "elit"], LineBreak
  ]

main :: IO ()
main = do
  samples <- mapM (\(name, fp) -> (name,) <$> getSample fp)
                [("markup-heavy", "benchmark/markup-heavy.md")
                ,("text-heavy", "benchmark/text-heavy.md")]
  defaultMain $
    map
      (\(name, (doc, imgs)) ->
        bgroup name
          [ bgroup "writers" $ mapMaybe (writerBench imgs doc . fst)
                               (sortOn fst
                                 writers :: [(T.Text, Writer PandocPure)])
          , bgroup "readers" $ mapMaybe (readerBench imgs doc . fst)
                               (sortOn fst
                                 readers :: [(T.Text, Reader PandocPure)])
          ])
      samples
    ++
    [ env (pure $ force bigInlines) $ \ils ->
      bgroup "stringify"
        [ bench "stringify" $ nf stringify ils
        , bench "stringifyInlines" $ nf stringifyInlines ils
        ]
    ]
