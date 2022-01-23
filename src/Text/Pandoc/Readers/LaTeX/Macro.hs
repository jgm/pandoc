{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.LaTeX.Macro
  ( macroDef
  )
where
import Text.Pandoc.Extensions (Extension(..))
import Text.Pandoc.Logging (LogMessage(MacroAlreadyDefined))
import Text.Pandoc.Readers.LaTeX.Parsing
import Text.Pandoc.Readers.LaTeX.Types
import Text.Pandoc.Class
import Text.Pandoc.Shared (safeRead)
import Text.Pandoc.Parsing hiding (blankline, mathDisplay, mathInline,
                            optional, space, spaces, withRaw, (<|>))
import Control.Applicative ((<|>), optional)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))

macroDef :: (PandocMonad m, Monoid a) => (Text -> a) -> LP m a
macroDef constructor = do
    (_, s) <- withRaw (commandDef <|> environmentDef)
    (constructor (untokenize s) <$
      guardDisabled Ext_latex_macros)
     <|> return mempty
  where commandDef = do
          nameMacroPairs <- newcommand <|>
            checkGlobal (letmacro <|> edefmacro <|> defmacro <|> newif)
          guardDisabled Ext_latex_macros <|>
            mapM_ insertMacro nameMacroPairs
        environmentDef = do
          mbenv <- newenvironment
          case mbenv of
            Nothing -> return ()
            Just (name, macro1, macro2) ->
              guardDisabled Ext_latex_macros <|>
                do insertMacro (name, macro1)
                   insertMacro ("end" <> name, macro2)
        -- @\newenvironment{envname}[n-args][default]{begin}{end}@
        -- is equivalent to
        -- @\newcommand{\envname}[n-args][default]{begin}@
        -- @\newcommand{\endenvname}@

insertMacro :: PandocMonad m => (Text, Macro) -> LP m ()
insertMacro (name, macro'@(Macro GlobalScope _ _ _ _)) =
  updateState $ \s ->
     s{ sMacros = NonEmpty.map (M.insert name macro') (sMacros s) }
insertMacro (name, macro'@(Macro GroupScope _ _ _ _)) =
  updateState $ \s ->
     s{ sMacros = M.insert name macro' (NonEmpty.head (sMacros s)) :|
                      NonEmpty.tail (sMacros s) }

lookupMacro :: PandocMonad m => Text -> LP m Macro
lookupMacro name = do
   macros :| _ <- sMacros <$> getState
   case M.lookup name macros of
     Just m -> return m
     Nothing -> fail "Macro not found"

letmacro :: PandocMonad m => LP m [(Text, Macro)]
letmacro = do
  controlSeq "let"
  withVerbatimMode $ do
    Tok _ (CtrlSeq name) _ <- anyControlSeq
    optional $ symbol '='
    spaces
    -- we first parse in verbatim mode, and then expand macros,
    -- because we don't want \let\foo\bar to turn into
    -- \let\foo hello if we have previously \def\bar{hello}
    target <- anyControlSeq <|> singleChar
    case target of
      (Tok _ (CtrlSeq name') _) ->
         (do m <- lookupMacro name'
             pure [(name, m)])
         <|> pure [(name,
                    Macro GroupScope ExpandWhenDefined [] Nothing [target])]
      _ -> pure [(name, Macro GroupScope ExpandWhenDefined [] Nothing [target])]

checkGlobal :: PandocMonad m => LP m [(Text, Macro)] -> LP m [(Text, Macro)]
checkGlobal p =
  (controlSeq "global" *>
      (map (\(n, Macro _ expand arg optarg contents) ->
                (n, Macro GlobalScope expand arg optarg contents)) <$> p))
   <|> p

edefmacro :: PandocMonad m => LP m [(Text, Macro)]
edefmacro = do
  scope <- (GroupScope <$ controlSeq "edef")
       <|> (GlobalScope <$ controlSeq "xdef")
  (name, contents) <- withVerbatimMode $ do
    Tok _ (CtrlSeq name) _ <- anyControlSeq
    -- we first parse in verbatim mode, and then expand macros,
    -- because we don't want \let\foo\bar to turn into
    -- \let\foo hello if we have previously \def\bar{hello}
    contents <- bracedOrToken
    return (name, contents)
  -- expand macros
  contents' <- parseFromToks (many anyTok) contents
  return [(name, Macro scope ExpandWhenDefined [] Nothing contents')]

defmacro :: PandocMonad m => LP m [(Text, Macro)]
defmacro = do
  -- we use withVerbatimMode, because macros are to be expanded
  -- at point of use, not point of definition
  scope <- (GroupScope <$ controlSeq "def")
       <|> (GlobalScope <$ controlSeq "gdef")
  withVerbatimMode $ do
    Tok _ (CtrlSeq name) _ <- anyControlSeq
    argspecs <- many (argspecArg <|> argspecPattern)
    contents <- bracedOrToken
    return [(name, Macro scope ExpandWhenUsed argspecs Nothing contents)]

-- \newif\iffoo' defines:
-- \iffoo to be \iffalse
-- \footrue to be a command that defines \iffoo to be \iftrue
-- \foofalse to be a command that defines \iffoo to be \iffalse
newif :: PandocMonad m => LP m [(Text, Macro)]
newif = do
  controlSeq "newif"
  withVerbatimMode $ do
    Tok pos (CtrlSeq name) _ <- anyControlSeq
    -- \def\iffoo\iffalse
    -- \def\footrue{\def\iffoo\iftrue}
    -- \def\foofalse{\def\iffoo\iffalse}
    let base = T.drop 2 name
    return [ (name, Macro GroupScope ExpandWhenUsed [] Nothing
                    [Tok pos (CtrlSeq "iffalse") "\\iffalse"])
           , (base <> "true",
                   Macro GroupScope ExpandWhenUsed [] Nothing
                   [ Tok pos (CtrlSeq "def") "\\def"
                   , Tok pos (CtrlSeq name) ("\\" <> name)
                   , Tok pos Symbol "{"
                   , Tok pos (CtrlSeq "iftrue") "\\iftrue"
                   , Tok pos Symbol "}"
                   ])
           , (base <> "false",
                   Macro GroupScope ExpandWhenUsed [] Nothing
                   [ Tok pos (CtrlSeq "def") "\\def"
                   , Tok pos (CtrlSeq name) ("\\" <> name)
                   , Tok pos Symbol "{"
                   , Tok pos (CtrlSeq "iffalse") "\\iffalse"
                   , Tok pos Symbol "}"
                   ])
           ]

argspecArg :: PandocMonad m => LP m ArgSpec
argspecArg = do
  Tok _ (Arg i) _ <- satisfyTok isArgTok
  return $ ArgNum i

argspecPattern :: PandocMonad m => LP m ArgSpec
argspecPattern =
  Pattern <$> many1 (satisfyTok (\(Tok _ toktype' txt) ->
                              (toktype' == Symbol || toktype' == Word) &&
                              (txt /= "{" && txt /= "\\" && txt /= "}")))

newcommand :: PandocMonad m => LP m [(Text, Macro)]
newcommand = do
  Tok pos (CtrlSeq mtype) _ <- controlSeq "newcommand" <|>
                             controlSeq "renewcommand" <|>
                             controlSeq "providecommand" <|>
                             controlSeq "DeclareMathOperator" <|>
                             controlSeq "DeclareRobustCommand"
  withVerbatimMode $ do
    Tok _ (CtrlSeq name) txt <- do
      optional (symbol '*')
      anyControlSeq <|>
        (symbol '{' *> spaces *> anyControlSeq <* spaces <* symbol '}')
    spaces
    numargs <- option 0 $ try bracketedNum
    let argspecs = map ArgNum [1..numargs]
    spaces
    optarg <- option Nothing $ Just <$> try bracketedToks
    spaces
    contents' <- bracedOrToken
    let contents =
         case mtype of
              "DeclareMathOperator" ->
                 Tok pos (CtrlSeq "mathop") "\\mathop"
                 : Tok pos Symbol "{"
                 : Tok pos (CtrlSeq "mathrm") "\\mathrm"
                 : Tok pos Symbol "{"
                 : (contents' ++
                   [ Tok pos Symbol "}", Tok pos Symbol "}" ])
              _                     -> contents'
    let macro = Macro GroupScope ExpandWhenUsed argspecs optarg contents
    (do lookupMacro name
        case mtype of
          "providecommand" -> return []
          "renewcommand" -> return [(name, macro)]
          _ -> [] <$ report (MacroAlreadyDefined txt pos))
      <|> pure [(name, macro)]

newenvironment :: PandocMonad m => LP m (Maybe (Text, Macro, Macro))
newenvironment = do
  pos <- getPosition
  Tok _ (CtrlSeq mtype) _ <- controlSeq "newenvironment" <|>
                             controlSeq "renewenvironment" <|>
                             controlSeq "provideenvironment"
  withVerbatimMode $ do
    optional $ symbol '*'
    spaces
    name <- untokenize <$> braced
    spaces
    numargs <- option 0 $ try bracketedNum
    spaces
    optarg <- option Nothing $ Just <$> try bracketedToks
    let argspecs = map (\i -> ArgNum i) [1..numargs]
    startcontents <- spaces >> bracedOrToken
    endcontents <- spaces >> bracedOrToken
    -- we need the environment to be in a group so macros defined
    -- inside behave correctly:
    let bg = Tok pos (CtrlSeq "bgroup") "\\bgroup "
    let eg = Tok pos (CtrlSeq "egroup") "\\egroup "
    let result = (name,
                    Macro GroupScope ExpandWhenUsed argspecs optarg
                      (bg:startcontents),
                    Macro GroupScope ExpandWhenUsed [] Nothing
                      (endcontents ++ [eg]))
    (do lookupMacro name
        case mtype of
          "provideenvironment" -> return Nothing
          "renewenvironment" -> return (Just result)
          _ -> do
             report $ MacroAlreadyDefined name pos
             return Nothing)
      <|> return (Just result)

bracketedNum :: PandocMonad m => LP m Int
bracketedNum = do
  ds <- untokenize <$> bracketedToks
  case safeRead ds of
       Just i -> return i
       _      -> return 0
