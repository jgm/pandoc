{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.LaTeX.Math
  ( dollarsMath
  , inlineEnvironments
  , inlineEnvironment
  , mathInline
  , mathDisplay
  , theoremstyle
  , theoremEnvironment
  , newtheorem
  , proof
  )
where
import Data.Maybe (fromMaybe)
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Builder as B
import qualified Data.Sequence as Seq
import Text.Pandoc.Readers.LaTeX.Parsing
import Text.Pandoc.Readers.LaTeX.Types
import Text.Pandoc.Class
import Text.Pandoc.Shared (trimMath, stripTrailingNewlines)
import Text.Pandoc.Parsing hiding (blankline, mathDisplay, mathInline,
                            optional, space, spaces, withRaw, (<|>))
import Control.Applicative ((<|>), optional)
import Control.Monad (guard, mzero)
import qualified Data.Map as M
import Data.Text (Text)

dollarsMath :: PandocMonad m => LP m Inlines
dollarsMath = do
  symbol '$'
  display <- option False (True <$ symbol '$')
  (do contents <- try $ untokenize <$> pDollarsMath 0
      if display
         then mathDisplay contents <$ symbol '$'
         else return $ mathInline contents)
   <|> (guard display >> return (mathInline ""))

-- Int is number of embedded groupings
pDollarsMath :: PandocMonad m => Int -> LP m [Tok]
pDollarsMath n = do
  tk@(Tok _ toktype t) <- anyTok
  case toktype of
       Symbol | t == "$"
              , n == 0 -> return []
              | t == "\\" -> do
                  tk' <- anyTok
                  (tk :) . (tk' :) <$> pDollarsMath n
              | t == "{" -> (tk :) <$> pDollarsMath (n+1)
              | t == "}" ->
                if n > 0
                then (tk :) <$> pDollarsMath (n-1)
                else mzero
       _ -> (tk :) <$> pDollarsMath n

mathDisplay :: Text -> Inlines
mathDisplay = displayMath . trimMath

mathInline :: Text -> Inlines
mathInline = math . trimMath

mathEnvWith :: PandocMonad m
            => (Inlines -> a) -> Maybe Text -> Text -> LP m a
mathEnvWith f innerEnv name = f . mathDisplay . inner <$> mathEnv name
   where inner x = case innerEnv of
                        Nothing -> x
                        Just y  -> "\\begin{" <> y <> "}\n" <> x <>
                                   "\\end{" <> y <> "}"

mathEnv :: PandocMonad m => Text -> LP m Text
mathEnv name = do
  skipopts
  optional blankline
  res <- manyTill anyTok (end_ name)
  return $ stripTrailingNewlines $ untokenize res

inlineEnvironment :: PandocMonad m => LP m Inlines
inlineEnvironment = try $ do
  controlSeq "begin"
  name <- untokenize <$> braced
  M.findWithDefault mzero name inlineEnvironments

inlineEnvironments :: PandocMonad m => M.Map Text (LP m Inlines)
inlineEnvironments = M.fromList [
    ("displaymath", mathEnvWith id Nothing "displaymath")
  , ("math", math <$> mathEnv "math")
  , ("equation", mathEnvWith id Nothing "equation")
  , ("equation*", mathEnvWith id Nothing "equation*")
  , ("gather", mathEnvWith id (Just "gathered") "gather")
  , ("gather*", mathEnvWith id (Just "gathered") "gather*")
  , ("multline", mathEnvWith id (Just "gathered") "multline")
  , ("multline*", mathEnvWith id (Just "gathered") "multline*")
  , ("eqnarray", mathEnvWith id (Just "aligned") "eqnarray")
  , ("eqnarray*", mathEnvWith id (Just "aligned") "eqnarray*")
  , ("align", mathEnvWith id (Just "aligned") "align")
  , ("align*", mathEnvWith id (Just "aligned") "align*")
  , ("alignat", mathEnvWith id (Just "aligned") "alignat")
  , ("alignat*", mathEnvWith id (Just "aligned") "alignat*")
  , ("dmath", mathEnvWith id Nothing "dmath")
  , ("dmath*", mathEnvWith id Nothing "dmath*")
  , ("dgroup", mathEnvWith id (Just "aligned") "dgroup")
  , ("dgroup*", mathEnvWith id (Just "aligned") "dgroup*")
  , ("darray", mathEnvWith id (Just "aligned") "darray")
  , ("darray*", mathEnvWith id (Just "aligned") "darray*")
  ]

theoremstyle :: PandocMonad m => LP m Blocks
theoremstyle = do
  stylename <- untokenize <$> braced
  let mbstyle = case stylename of
                  "plain"      -> Just PlainStyle
                  "definition" -> Just DefinitionStyle
                  "remark"     -> Just RemarkStyle
                  _            -> Nothing
  case mbstyle of
    Nothing  -> return ()
    Just sty -> updateState $ \s -> s{ sLastTheoremStyle = sty }
  return mempty

newtheorem :: PandocMonad m => LP m Inlines -> LP m Blocks
newtheorem inline = do
  number <- option True (False <$ symbol '*' <* sp)
  name <- untokenize <$> braced
  sp
  series <- option Nothing $ Just . untokenize <$> bracketedToks
  sp
  showName <- tokWith inline
  sp
  syncTo <- option Nothing $ Just . untokenize <$> bracketedToks
  sty <- sLastTheoremStyle <$> getState
  let spec = TheoremSpec { theoremName = showName
                         , theoremStyle = sty
                         , theoremSeries = series
                         , theoremSyncTo = syncTo
                         , theoremNumber = number
                         , theoremLastNum = DottedNum [0] }
  tmap <- sTheoremMap <$> getState
  updateState $ \s -> s{ sTheoremMap =
                            M.insert name spec tmap }
  return mempty

theoremEnvironment :: PandocMonad m
                   => LP m Blocks -> LP m Inlines -> Text -> LP m Blocks
theoremEnvironment blocks opt name = do
  tmap <- sTheoremMap <$> getState
  case M.lookup name tmap of
    Nothing -> mzero
    Just tspec -> do
       optTitle <- option mempty $ (\x -> space <> "(" <> x <> ")") <$> opt
       mblabel <- option Nothing $ Just . untokenize <$>
                   try (spaces >> controlSeq "label" >> spaces >> braced)
       bs <- env name blocks
       number <-
         if theoremNumber tspec
            then do
               let name' = fromMaybe name $ theoremSeries tspec
               num <- getNextNumber
                   (maybe (DottedNum [0]) theoremLastNum .
                    M.lookup name' . sTheoremMap)
               updateState $ \s ->
                 s{ sTheoremMap =
                       M.adjust
                       (\spec -> spec{ theoremLastNum = num })
                       name'
                       (sTheoremMap s)
                  }

               case mblabel of
                 Just ident ->
                   updateState $ \s ->
                     s{ sLabels = M.insert ident
                         (B.toList $
                           theoremName tspec <> "\160" <>
                           str (renderDottedNum num)) (sLabels s) }
                 Nothing -> return ()
               return $ space <> B.text (renderDottedNum num)
            else return mempty
       let titleEmph = case theoremStyle tspec of
                         PlainStyle      -> B.strong
                         DefinitionStyle -> B.strong
                         RemarkStyle     -> B.emph
       let title = titleEmph (theoremName tspec <> number)
                      <> optTitle <> "." <> space
       return $ divWith (fromMaybe "" mblabel, [name], []) $ addTitle title
              $ case theoremStyle tspec of
                  PlainStyle -> walk italicize bs
                  _          -> bs



proof :: PandocMonad m => LP m Blocks -> LP m Inlines -> LP m Blocks
proof blocks opt = do
  title <- option (B.text "Proof") opt
  bs <- env "proof" blocks
  return $
    B.divWith ("", ["proof"], []) $
      addQed $ addTitle (B.emph (title <> ".")) bs

addTitle :: Inlines -> Blocks -> Blocks
addTitle ils bs =
  case B.toList bs of
    (Para xs : rest)
      -> B.fromList (Para (B.toList ils ++ (Space : xs)) : rest)
    _ -> B.para ils <> bs

addQed :: Blocks -> Blocks
addQed bs =
  case Seq.viewr (B.unMany bs) of
    s Seq.:> Para ils
      -> B.Many (s Seq.|> Para (ils ++ B.toList qedSign))
    _ -> bs <> B.para qedSign
 where
  qedSign = B.str "\xa0\x25FB"

italicize :: Block -> Block
italicize x@(Para [Image{}]) = x -- see #6925
italicize (Para ils) = Para [Emph ils]
italicize (Plain ils) = Plain [Emph ils]
italicize x = x


