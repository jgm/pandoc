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

macroDef :: (PandocMonad m, Monoid a) => (Text -> a) -> LP m a
macroDef constructor = do
    (_, s) <- withRaw (commandDef <|> environmentDef)
    (constructor (untokenize s) <$
      guardDisabled Ext_latex_macros)
     <|> return mempty
  where commandDef = do
          nameMacroPairs <- newcommand <|> letmacro <|> defmacro <|> newif
          guardDisabled Ext_latex_macros <|>
           mapM_ (\(name, macro') ->
                   updateState (\s -> s{ sMacros = M.insert name macro'
                                          (sMacros s) })) nameMacroPairs
        environmentDef = do
          mbenv <- newenvironment
          case mbenv of
            Nothing -> return ()
            Just (name, macro1, macro2) ->
              guardDisabled Ext_latex_macros <|>
                do updateState $ \s -> s{ sMacros =
                    M.insert name macro1 (sMacros s) }
                   updateState $ \s -> s{ sMacros =
                    M.insert ("end" <> name) macro2 (sMacros s) }
        -- @\newenvironment{envname}[n-args][default]{begin}{end}@
        -- is equivalent to
        -- @\newcommand{\envname}[n-args][default]{begin}@
        -- @\newcommand{\endenvname}@

letmacro :: PandocMonad m => LP m [(Text, Macro)]
letmacro = do
  controlSeq "let"
  (name, contents) <- withVerbatimMode $ do
    Tok _ (CtrlSeq name) _ <- anyControlSeq
    optional $ symbol '='
    spaces
    -- we first parse in verbatim mode, and then expand macros,
    -- because we don't want \let\foo\bar to turn into
    -- \let\foo hello if we have previously \def\bar{hello}
    contents <- bracedOrToken
    return (name, contents)
  contents' <- doMacros' 0 contents
  return [(name, Macro ExpandWhenDefined [] Nothing contents')]

defmacro :: PandocMonad m => LP m [(Text, Macro)]
defmacro = do
  -- we use withVerbatimMode, because macros are to be expanded
  -- at point of use, not point of definition
  controlSeq "def"
  withVerbatimMode $ do
    Tok _ (CtrlSeq name) _ <- anyControlSeq
    argspecs <- many (argspecArg <|> argspecPattern)
    contents <- bracedOrToken
    return [(name, Macro ExpandWhenUsed argspecs Nothing contents)]

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
    return [ (name, Macro ExpandWhenUsed [] Nothing
                    [Tok pos (CtrlSeq "iffalse") "\\iffalse"])
           , (base <> "true",
                   Macro ExpandWhenUsed [] Nothing
                   [ Tok pos (CtrlSeq "def") "\\def"
                   , Tok pos (CtrlSeq name) ("\\" <> name)
                   , Tok pos (CtrlSeq "iftrue") "\\iftrue"
                   ])
           , (base <> "false",
                   Macro ExpandWhenUsed [] Nothing
                   [ Tok pos (CtrlSeq "def") "\\def"
                   , Tok pos (CtrlSeq name) ("\\" <> name)
                   , Tok pos (CtrlSeq "iffalse") "\\iffalse"
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
    macros <- sMacros <$> getState
    case M.lookup name macros of
        Just macro
          | mtype == "newcommand" -> do
              report $ MacroAlreadyDefined txt pos
              return [(name, macro)]
          | mtype == "providecommand" -> return [(name, macro)]
        _ -> return [(name, Macro ExpandWhenUsed argspecs optarg contents)]

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
    macros <- sMacros <$> getState
    case M.lookup name macros of
         Just _
           | mtype == "newenvironment" -> do
               report $ MacroAlreadyDefined name pos
               return Nothing
           | mtype == "provideenvironment" ->
               return Nothing
         _ -> return $ Just (name,
                      Macro ExpandWhenUsed argspecs optarg startcontents,
                      Macro ExpandWhenUsed [] Nothing endcontents)

bracketedNum :: PandocMonad m => LP m Int
bracketedNum = do
  ds <- untokenize <$> bracketedToks
  case safeRead ds of
       Just i -> return i
       _      -> return 0
