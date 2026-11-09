{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.LaTeX.Macro
  ( macroDef
  )
where
import Text.Pandoc.Extensions (Extension(..))
import Text.Pandoc.Logging (LogMessage(MacroAlreadyDefined))
import Text.Pandoc.Readers.LaTeX.Parsing
import Text.Pandoc.TeX
import Text.Pandoc.Class
import Text.Pandoc.Shared (safeRead)
import Text.Pandoc.Parsing hiding (blankline, mathDisplay, mathInline,
                            optional, space, spaces, withRaw, (<|>))
import Control.Applicative ((<|>), optional)
import Control.Monad (guard)
import Data.Char (chr, isLetter, ord)
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))

macroDef :: (PandocMonad m, Monoid a) => (Text -> a) -> LP m a
macroDef constructor = do
    Tok _ (CtrlSeq name) _ <- peekTok
    -- fail quickly, before trying each alternative in turn, unless
    -- the next token can begin a macro definition:
    guard $ name `Set.member` macroDefCommands
    (_, s) <- withRaw (commandDef <|> environmentDef)
    (constructor (untokenize s) <$
      guardDisabled Ext_latex_macros)
     <|> return mempty
  where commandDef = do
          nameMacroPairs <- newcommand <|> newDocumentCommand <|>
            newDocumentEnvironment <|> commandCopy <|> environmentCopy <|>
            checkGlobal (letmacro <|> edefmacro <|> defmacro <|> newif)
          guardDisabled Ext_latex_macros <|>
            mapM_ insertMacro
              (filter ((`Set.notMember` pandocInternalMacros) . fst)
                      nameMacroPairs)
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

-- | Macros defined in pandoc's LaTeX template and handled directly
-- by the reader. Their definitions are too complex for the reader's
-- macro expansion, so we ignore them.
pandocInternalMacros :: Set.Set Text
pandocInternalMacros = Set.fromList
  [ "pandocbounded", "pandocfootnotemark", "pandocfootnotetext" ]

-- | Control sequences that can begin a macro definition.  This
-- must include every control sequence that one of the parsers
-- used in 'macroDef' can start with.
macroDefCommands :: Set.Set Text
macroDefCommands = Set.fromList
  [ "global"  -- see checkGlobal
  , "let", "edef", "xdef", "def", "gdef", "newif"
  , "newcommand", "renewcommand", "providecommand"
  , "DeclareMathOperator", "DeclareRobustCommand"
  , "NewDocumentCommand", "RenewDocumentCommand"
  , "ProvideDocumentCommand", "DeclareDocumentCommand"
  , "NewExpandableDocumentCommand", "RenewExpandableDocumentCommand"
  , "ProvideExpandableDocumentCommand", "DeclareExpandableDocumentCommand"
  , "NewDocumentEnvironment", "RenewDocumentEnvironment"
  , "ProvideDocumentEnvironment", "DeclareDocumentEnvironment"
  , "NewCommandCopy", "RenewCommandCopy", "DeclareCommandCopy"
  , "NewEnvironmentCopy", "RenewEnvironmentCopy", "DeclareEnvironmentCopy"
  , "newenvironment", "renewenvironment", "provideenvironment"
  ]

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
newif = try $ do
  controlSeq "newif"
  withVerbatimMode $ do
    Tok pos (CtrlSeq name) _ <- anyControlSeq
    guard $ "if" `T.isPrefixOf` name
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

-- | Parses a definition of the form
-- @\NewDocumentCommand\cmd{argspec}{body}@ (and the Renew, Provide,
-- Declare, and Expandable variants), with xparse (LaTeX3) argument
-- specifiers.
newDocumentCommand :: PandocMonad m => LP m [(Text, Macro)]
newDocumentCommand = try $ do
  Tok pos (CtrlSeq mtype) _ <-
        controlSeq "NewDocumentCommand"
    <|> controlSeq "RenewDocumentCommand"
    <|> controlSeq "ProvideDocumentCommand"
    <|> controlSeq "DeclareDocumentCommand"
    <|> controlSeq "NewExpandableDocumentCommand"
    <|> controlSeq "RenewExpandableDocumentCommand"
    <|> controlSeq "ProvideExpandableDocumentCommand"
    <|> controlSeq "DeclareExpandableDocumentCommand"
  withVerbatimMode $ do
    Tok _ (CtrlSeq name) txt <- do
      spaces
      anyControlSeq <|>
        (symbol '{' *> spaces *> anyControlSeq <* spaces <* symbol '}')
    spaces
    (argspecs, _) <- xparseArgSpecs False
    spaces
    contents <- bracedOrToken
    let macro = Macro GroupScope ExpandWhenUsed argspecs Nothing contents
    (do lookupMacro name
        if "Provide" `T.isPrefixOf` mtype
           then return []
           else if "New" `T.isPrefixOf` mtype
                   then [] <$ report (MacroAlreadyDefined txt pos)
                   else return [(name, macro)]) -- Renew or Declare
      <|> pure [(name, macro)]

-- | Parses a definition of the form
-- @\NewDocumentEnvironment{name}{argspec}{begin-code}{end-code}@
-- (and the Renew, Provide, and Declare variants), with xparse
-- (LaTeX3) argument specifiers.  Unlike with @\newenvironment@,
-- the arguments are also available in the end-code; to support
-- this we bind a group-scoped helper macro, at the point where
-- @\begin{name}@ is expanded, whose body is the end-code with the
-- arguments already substituted; @\end{name}@ just expands the
-- helper.  (Group scoping makes this work for nested environments.)
newDocumentEnvironment :: PandocMonad m => LP m [(Text, Macro)]
newDocumentEnvironment = try $ do
  Tok pos (CtrlSeq mtype) _ <-
        controlSeq "NewDocumentEnvironment"
    <|> controlSeq "RenewDocumentEnvironment"
    <|> controlSeq "ProvideDocumentEnvironment"
    <|> controlSeq "DeclareDocumentEnvironment"
  withVerbatimMode $ do
    spaces
    name <- T.strip . untokenize <$> braced
    spaces
    (argspecs, usesBody) <- xparseArgSpecs True
    startcontents <- spaces >> bracedOrToken
    endcontents <- spaces >> bracedOrToken
    -- we need the environment to be in a group so macros defined
    -- inside behave correctly:
    let bg = Tok pos (CtrlSeq "bgroup") "\\bgroup "
    let eg = Tok pos (CtrlSeq "egroup") "\\egroup "
    let helperName = "pandocxparseenvend" <> letterize name
    let helper = Tok pos (CtrlSeq helperName) ("\\" <> helperName <> " ")
    let defHelper = Tok pos (CtrlSeq "def") "\\def"
                  : helper
                  : Tok pos Symbol "{"
                  : endcontents ++ [Tok pos Symbol "}"]
    let result
          | usesBody =
            -- a 'b' argspec grabs everything up to \end{name} as the
            -- last argument (the argspecs end with its ArgNum, and we
            -- add a Pattern that consumes the \end{name}), so begin-
            -- and end-code run together:
            [ (name,
               Macro GroupScope ExpandWhenUsed
                 (argspecs ++ [Pattern (tokenize pos ("\\end{" <> name <> "}"))])
                 Nothing
                 (bg : defHelper ++ startcontents ++ [helper, eg])) ]
          | otherwise =
            [ (name,
               Macro GroupScope ExpandWhenUsed argspecs Nothing
                 (bg : defHelper ++ startcontents))
            , ("end" <> name,
               Macro GroupScope ExpandWhenUsed [] Nothing [helper, eg]) ]
    (do lookupMacro name
        if "Provide" `T.isPrefixOf` mtype
           then return []
           else if "New" `T.isPrefixOf` mtype
                   then [] <$ report (MacroAlreadyDefined name pos)
                   else return result) -- Renew or Declare
      <|> pure result

-- | Parses @\NewCommandCopy\new\old@ (and the Renew and Declare
-- variants): like @\let@, restricted to control sequences.
commandCopy :: PandocMonad m => LP m [(Text, Macro)]
commandCopy = try $ do
  Tok pos (CtrlSeq mtype) _ <-
        controlSeq "NewCommandCopy"
    <|> controlSeq "RenewCommandCopy"
    <|> controlSeq "DeclareCommandCopy"
  withVerbatimMode $ do
    Tok _ (CtrlSeq name) txt <- do
      spaces
      anyControlSeq <|>
        (symbol '{' *> spaces *> anyControlSeq <* spaces <* symbol '}')
    spaces
    target@(Tok _ (CtrlSeq targetName) _) <- anyControlSeq
    result <- (do m <- lookupMacro targetName
                  pure [(name, m)])
          <|> pure [(name, Macro GroupScope ExpandWhenDefined [] Nothing
                             [target])]
    (do lookupMacro name
        if "New" `T.isPrefixOf` mtype
           then [] <$ report (MacroAlreadyDefined txt pos)
           else return result) -- Renew or Declare
      <|> pure result

-- | Parses @\NewEnvironmentCopy{new}{old}@ (and the Renew and
-- Declare variants), copying both the begin and the end macros.
-- If the target environment is not macro-defined, the copy expands
-- to @\begin{old}@ / @\end{old}@.
environmentCopy :: PandocMonad m => LP m [(Text, Macro)]
environmentCopy = try $ do
  Tok pos (CtrlSeq mtype) _ <-
        controlSeq "NewEnvironmentCopy"
    <|> controlSeq "RenewEnvironmentCopy"
    <|> controlSeq "DeclareEnvironmentCopy"
  withVerbatimMode $ do
    spaces
    name <- untokenize <$> braced
    spaces
    target <- untokenize <$> braced
    let copyOne to from fallback =
          (do m <- lookupMacro from
              pure (to, m))
          <|> pure (to, Macro GroupScope ExpandWhenUsed [] Nothing
                          (tokenize pos fallback))
    beginmacro <- copyOne name target ("\\begin{" <> target <> "}")
    endmacro <- copyOne ("end" <> name) ("end" <> target)
                        ("\\end{" <> target <> "}")
    let result = [beginmacro, endmacro]
    (do lookupMacro name
        if "New" `T.isPrefixOf` mtype
           then [] <$ report (MacroAlreadyDefined name pos)
           else return result) -- Renew or Declare
      <|> pure result

-- | Encode a text using letters only (so that the result can be
-- part of a control sequence name that survives retokenization).
letterize :: Text -> Text
letterize = T.concatMap go
  where go c | isLetter c = T.singleton c
             | otherwise = "x" <> T.map toLetter (T.pack (show (ord c)))
        toLetter d = chr (ord d + 49)  -- '0'..'9' -> 'a'..'j'

-- | Parses a braced xparse argument specification, e.g.
-- @{s O{default} m}@.  The Bool parameter determines whether a
-- @b@ (environment body) specifier is allowed; the Bool in the
-- result is True if one was used (it can only come last).
xparseArgSpecs :: PandocMonad m => Bool -> LP m ([ArgSpec], Bool)
xparseArgSpecs allowBody = symbol '{' *> go 1
 where
  go n = do
    spaces
    (([], False) <$ symbol '}') <|>
      (do (spec, isBody) <- xparseArgSpec allowBody n
          if isBody
             then ([spec], True) <$ (spaces <* symbol '}')
             else do (rest, usesBody) <- go (n + numslots spec)
                     return (spec : rest, usesBody))
  -- most specifiers bind one argument; embellishments bind one
  -- argument per embellishment token
  numslots (EmbellishArg embs) = length embs
  numslots (ProcessedArg _ spec) = numslots spec
  numslots _ = 1

xparseArgSpec :: PandocMonad m => Bool -> Int -> LP m (ArgSpec, Bool)
xparseArgSpec allowBody n = go True
 where
  -- the parameter is False if the ! modifier has been seen; it
  -- disables space-skipping before optional arguments, and (for the
  -- b and c body specifiers) trimming of the ends of the body
  go noBang = do
   Tok pos _ c <- singleChar
   let plain spec = pure (spec, False)
   case c of
    "+" -> spaces *> go noBang  -- "long": no distinction
    "!" -> spaces *> go False   -- no space-skipping
    "=" -> spaces *> braced *> spaces *> go noBang
           -- key-value interface for the argument: not modeled
    ">" -> do proc <- spaces *> braced
              (spec, isBody) <- spaces *> go noBang
              if isBody
                 then pure (spec, isBody)
                      -- processors are not supported on 'b' arguments
                 else case spec of
                        ProcessedArg procs inner ->
                          pure (ProcessedArg (proc : procs) inner, False)
                        _ -> pure (ProcessedArg [proc] spec, False)
    "m" -> plain $ ArgNum n
    "o" -> plain $ DelimArg noBang (lbTok pos) (rbTok pos) Nothing
    "O" -> do dflt <- spaces *> braced
              plain $ DelimArg noBang (lbTok pos) (rbTok pos) (Just dflt)
    "s" -> plain $ BoolArg noBang (Tok pos Symbol "*")
    "t" -> specTok >>= plain . BoolArg noBang
    "d" -> do o <- specTok
              c' <- specTok
              plain $ DelimArg noBang o c' Nothing
    "D" -> do o <- specTok
              c' <- specTok
              dflt <- spaces *> braced
              plain $ DelimArg noBang o c' (Just dflt)
    "r" -> do o <- specTok
              c' <- specTok
              plain $ DelimArg noBang o c' Nothing
    "R" -> do o <- specTok
              c' <- specTok
              dflt <- spaces *> braced
              plain $ DelimArg noBang o c' (Just dflt)
    "v" -> plain VerbArg
    "e" -> do embtoks <- spaces *> braced
              plain $ EmbellishArg [(t, Nothing) | t <- embTokens embtoks]
    "E" -> do embtoks <- spaces *> braced
              spaces
              defaults <- symbol '{' *> many (try (spaces *> braced))
                            <* spaces <* symbol '}'
              plain $ EmbellishArg $
                zip (embTokens embtoks) (map Just defaults ++ repeat Nothing)
    "b" | allowBody -> pure (BodyArg noBang n, True)
    "c" | allowBody -> pure (VerbBodyArg noBang n, True)  -- verbatim body
    _   -> fail "unsupported xparse argument specifier"
  lbTok pos = Tok pos Symbol "["
  rbTok pos = Tok pos Symbol "]"
  specTok = spaces *> singleChar
  embTokens = filter (not . tokTypeIn [Spaces, Newline, Comment])

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
