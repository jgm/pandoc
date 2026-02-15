{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Writers.StarMath
  ( writeStarMath
  ) where

import qualified Data.Text as T
import Text.TeXMath (DisplayType, writeTeX)
import Text.TeXMath.Types
  ( Exp(..)
  , Alignment(..)
  , FractionType(..)
  , TextType(..)
  , TeXSymbolType(..)
  )

-- | Render TeXMath expressions as StarMath syntax.
-- Falls back to TeX output for expressions that are not yet supported.
writeStarMath :: DisplayType -> [Exp] -> T.Text
writeStarMath _dt exps =
  case renderExps exps of
    Just rendered -> T.strip rendered
    Nothing       -> writeTeX exps

data AlignContext = AlignDefault | AlignLeftCtx | AlignRightCtx
  deriving (Eq)

renderExps :: [Exp] -> Maybe T.Text
renderExps = renderExpsIn AlignDefault

renderExpsIn :: AlignContext -> [Exp] -> Maybe T.Text
renderExpsIn ctx exps = do
  rendered <- mapM (renderExpIn ctx) exps
  let pieces = zip exps rendered
  let merged = mergePieces pieces
  pure $ if startsWithOperatorNeedingLhs exps
            then "{} " <> T.stripStart merged
            else merged

mergePieces :: [(Exp, T.Text)] -> T.Text
mergePieces [] = ""
mergePieces ((e0, t0) : rest) = snd $ foldl' step (e0, t0) rest
 where
  step (prevE, acc) (curE, curT) =
    if T.null curT
       then (prevE, acc)
       else
         let sep = if needsSeparator prevE curE then " " else ""
         in (curE, acc <> sep <> curT)

needsSeparator :: Exp -> Exp -> Bool
needsSeparator prevE curE
  | isGreekIdentifierExp prevE && isIdentifierLike curE = True
  | isUnaryMinusSymbol prevE && isIdentifierLike curE    = True
  | isScripted prevE && not (isLargeOpScripted prevE) &&
      isIdentifierLike curE                             = True
  | isCloseLike prevE && isIdentifierLike curE          = True
  | isIdentifierLike prevE && isWideSpace curE          = True
  | isIdentifierLike prevE && isDelimited curE          = True
  | otherwise                                            = False

isGreekIdentifierExp :: Exp -> Bool
isGreekIdentifierExp e =
  case e of
    EIdentifier t -> greekName t /= Nothing
    _             -> False

isIdentifierLike :: Exp -> Bool
isIdentifierLike e =
  case e of
    EIdentifier{}   -> True
    ENumber{}       -> True
    EMathOperator{} -> True
    ESub{}          -> True
    ESuper{}        -> True
    ESubsup{}       -> True
    EStyled{}       -> True
    _               -> False

isDelimited :: Exp -> Bool
isDelimited e =
  case e of
    EDelimited{} -> True
    _            -> False

isWideSpace :: Exp -> Bool
isWideSpace e =
  case e of
    ESpace w -> w >= 1
    _        -> False

isCloseLike :: Exp -> Bool
isCloseLike e =
  case e of
    ESymbol Close _ -> True
    EDelimited{}    -> True
    _               -> False

isScripted :: Exp -> Bool
isScripted e =
  case e of
    ESub{}    -> True
    ESuper{}  -> True
    ESubsup{} -> True
    _         -> False

isUnaryMinusSymbol :: Exp -> Bool
isUnaryMinusSymbol e =
  case e of
    ESymbol t "-" -> t /= Bin
    ESymbol t "−" -> t /= Bin
    _             -> False

isLargeOpScripted :: Exp -> Bool
isLargeOpScripted e =
  case e of
    ESub base _      -> largeOpName base /= Nothing
    ESuper base _    -> largeOpName base /= Nothing
    ESubsup base _ _ -> largeOpName base /= Nothing
    _                -> False

startsWithOperatorNeedingLhs :: [Exp] -> Bool
startsWithOperatorNeedingLhs exps =
  case exps of
    (ESymbol _ s : _) -> s `elem` ["×", "⋅", "·"]
    _                 -> False

renderExpIn :: AlignContext -> Exp -> Maybe T.Text
renderExpIn ctx e =
  case e of
    ENumber t       -> Just t
    EIdentifier t   -> Just (renderIdentifier t)
    EMathOperator t -> Just ("func " <> t)
    ESymbol t s     -> Just (renderSymbol t s)
    EText _ t       -> Just (quoteText t)
    ESpace w        -> Just (renderSpace w)
    EGrouped xs     -> ("{" <>) . (<> "}") <$> renderExpsIn ctx xs
    EStyled sty xs  -> renderStyled ctx sty xs

    EFraction frac num den -> do
      num' <- renderExpIn AlignDefault num
      den' <- renderExpIn AlignDefault den
      let num'' = maybeCenterFractionArg ctx num'
      let den'' = maybeCenterFractionArg ctx den'
      pure $ case frac of
        NoLineFrac -> "{" <> num'' <> " / " <> den'' <> "}"
        _          -> "{" <> num'' <> " over " <> den'' <> "}"

    ESqrt x -> ("sqrt {" <>) . (<> "}") <$> renderExpIn ctx x
    ERoot idx rad -> do
      idx' <- renderExpIn ctx idx
      rad' <- renderExpIn ctx rad
      pure $ "nroot {" <> idx' <> "} {" <> rad' <> "}"

    EDelimited op cl xs -> do
      body <- renderDelimitedBody ctx xs
      let op' = delimToken DelimLeft op
      let cl' = delimToken DelimRight cl
      pure $ "left " <> op' <> " " <> body <> " right " <> cl'

    ESub base sub -> do
      case largeOpName base of
        Just op -> do
          sub' <- renderLimitArg ctx sub
          pure $ op <> " from " <> sub' <> " "
        Nothing -> do
          base' <- renderExpIn ctx base
          sub'  <- renderScriptArg ctx sub
          pure $ renderScriptBase base base' <> "_" <> sub'

    ESuper base sup -> do
      case largeOpName base of
        Just op -> do
          sup' <- renderLimitArg ctx sup
          pure $ op <> " to " <> sup' <> " "
        Nothing -> do
          base' <- renderExpIn ctx base
          sup'  <- renderScriptArg ctx sup
          pure $ renderScriptBase base base' <> "^" <> sup'

    ESubsup base sub sup -> do
      case largeOpName base of
        Just op -> do
          sub' <- renderLimitArg ctx sub
          sup' <- renderLimitArg ctx sup
          pure $ op <> " from " <> sub' <> " to " <> sup' <> " "
        Nothing -> do
          base' <- renderExpIn ctx base
          sub'  <- renderScriptArg ctx sub
          sup'  <- renderScriptArg ctx sup
          pure $ renderScriptBase base base' <> "_" <> sub' <> "^" <> sup'

    EOver _ base over
      | Just accent <- accentName over -> do
          base' <- renderExpIn ctx base
          pure $ accent <> " " <> renderAccentArg base base'
      | otherwise -> Nothing

    EUnder _ base under ->
      case largeOpName base of
        Just op -> do
          under' <- renderLimitArg ctx under
          pure $ op <> " from " <> under' <> " "
        Nothing -> Nothing
    EUnderover _ base under over ->
      case largeOpName base of
        Just op -> do
          under' <- renderLimitArg ctx under
          over'  <- renderLimitArg ctx over
          pure $ op <> " from " <> under' <> " to " <> over' <> " "
        Nothing -> Nothing
    EArray aligns rows -> renderMatrix aligns rows
    EPhantom{}   -> Nothing
    _            -> Nothing

renderDelimitedBody :: AlignContext -> [Either T.Text Exp] -> Maybe T.Text
renderDelimitedBody ctx xs = do
  chunks <- mapM (renderDelimitedChunk ctx) xs
  pure $ T.strip (mergeDelimitedChunks chunks)

data DelimitedChunk = DelimRaw T.Text | DelimExp Exp T.Text

renderDelimitedChunk :: AlignContext -> Either T.Text Exp -> Maybe DelimitedChunk
renderDelimitedChunk ctx p =
  case p of
    Left t  -> Just $ DelimRaw (" " <> delimToken DelimMiddle t <> " ")
    Right x -> DelimExp x <$> renderExpIn ctx x

mergeDelimitedChunks :: [DelimitedChunk] -> T.Text
mergeDelimitedChunks [] = ""
mergeDelimitedChunks (c0:cs) = snd $ foldl' step (chunkExp c0, chunkText c0) cs
 where
  step (prevExp, acc) cur
    | T.null curText = (prevExp, acc)
    | otherwise =
        case cur of
          DelimRaw _ -> (Nothing, acc <> curText)
          DelimExp curExp _ ->
            let sep = case prevExp of
                        Just pe -> if needsSeparator pe curExp then " " else ""
                        Nothing -> ""
            in (Just curExp, acc <> sep <> curText)
   where
    curText = chunkText cur

  chunkText c =
    case c of
      DelimRaw t    -> t
      DelimExp _ t  -> t

  chunkExp c =
    case c of
      DelimRaw _   -> Nothing
      DelimExp e _ -> Just e

renderMatrix :: [Alignment] -> [[[Exp]]] -> Maybe T.Text
renderMatrix aligns rows = do
  rows' <- mapM (renderMatrixRow aligns) rows
  pure $ "matrix { " <> T.intercalate " ## " rows' <> " }"

renderMatrixRow :: [Alignment] -> [[Exp]] -> Maybe T.Text
renderMatrixRow aligns cells = do
  cells' <- sequence
    [ renderMatrixCellWithAlign (columnAlign aligns i) c
    | (i, c) <- zip [(0 :: Int)..] cells
    ]
  pure $ T.intercalate " # " cells'

renderMatrixCell :: AlignContext -> [Exp] -> Maybe T.Text
renderMatrixCell _ [] = Just "{}"
renderMatrixCell ctx xs = do
  rendered <- renderExpsIn ctx xs
  let stripped = T.strip rendered
  pure $ if T.null stripped then "{}" else stripped

renderMatrixCellWithAlign :: Alignment -> [Exp] -> Maybe T.Text
renderMatrixCellWithAlign align xs = do
  cell <- renderMatrixCell (alignmentContext align) xs
  pure $ case align of
    AlignLeft  -> "alignl " <> cell
    AlignRight -> "alignr " <> cell
    _          -> cell

columnAlign :: [Alignment] -> Int -> Alignment
columnAlign aligns i =
  case drop i aligns of
    (a : _) -> a
    []      -> AlignCenter

renderStyled :: AlignContext -> TextType -> [Exp] -> Maybe T.Text
renderStyled ctx sty xs = do
  body <- renderExpsIn ctx xs
  pure $ case sty of
    TextItalic       -> "ital " <> styleArg body
    TextBold         -> "bold " <> styleArg body
    TextScript       -> "ital " <> styleArg body
    TextFraktur      -> "ital " <> styleArg body
    TextDoubleStruck -> "ital " <> styleArg body
    _                -> body
 where
  styleArg t
    | T.null t    = "{}"
    | T.length t == 1 = t
    | otherwise   = "{" <> t <> "}"

alignmentContext :: Alignment -> AlignContext
alignmentContext a =
  case a of
    AlignLeft  -> AlignLeftCtx
    AlignRight -> AlignRightCtx
    _          -> AlignDefault

maybeCenterFractionArg :: AlignContext -> T.Text -> T.Text
maybeCenterFractionArg ctx t
  | ctx == AlignLeftCtx || ctx == AlignRightCtx = "{alignc " <> asArg t <> "}"
  | otherwise = t
 where
  asArg x =
    let s = T.strip x
    in if T.null s
          then "{}"
          else if T.length s == 1
                  then s
                  else if T.head s == '{' && T.last s == '}'
                          then s
                  else "{" <> s <> "}"

renderSpace :: Rational -> T.Text
renderSpace w
  | w <= 0    = ""
  | w >= 2    = "~~ "
  | w >= 1    = "~ "
  | otherwise = " "

renderIdentifier :: T.Text -> T.Text
renderIdentifier ident =
  case greekName ident of
    Just name
      | shouldItalicizeGreek ident -> "%i" <> name
      | otherwise                  -> "%" <> name
    Nothing -> ident

-- Lowercase Greek identifiers are variables in TeX math and are usually italic.
shouldItalicizeGreek :: T.Text -> Bool
shouldItalicizeGreek ident =
  case ident of
    "α" -> True
    "β" -> True
    "γ" -> True
    "δ" -> True
    "ϵ" -> True
    "ε" -> True
    "ζ" -> True
    "η" -> True
    "θ" -> True
    "ϑ" -> True
    "ι" -> True
    "κ" -> True
    "λ" -> True
    "μ" -> True
    "ν" -> True
    "ξ" -> True
    "ο" -> True
    "π" -> True
    "ϖ" -> True
    "ρ" -> True
    "ϱ" -> True
    "𝜚" -> True
    "σ" -> True
    "ς" -> True
    "𝜍" -> True
    "τ" -> True
    "υ" -> True
    "ϕ" -> True
    "φ" -> True
    "χ" -> True
    "ψ" -> True
    "ω" -> True
    _   -> False

greekName :: T.Text -> Maybe T.Text
greekName ident =
  case ident of
    "α" -> Just "alpha"
    "β" -> Just "beta"
    "γ" -> Just "gamma"
    "δ" -> Just "delta"
    "ϵ" -> Just "varepsilon"
    "ε" -> Just "epsilon"
    "ζ" -> Just "zeta"
    "η" -> Just "eta"
    "θ" -> Just "theta"
    "ϑ" -> Just "vartheta"
    "ι" -> Just "iota"
    "κ" -> Just "kappa"
    "λ" -> Just "lambda"
    "μ" -> Just "mu"
    "ν" -> Just "nu"
    "ξ" -> Just "xi"
    "ο" -> Just "omicron"
    "π" -> Just "pi"
    "ϖ" -> Just "varpi"
    "ρ" -> Just "rho"
    "ϱ" -> Just "varrho"
    "𝜚" -> Just "varrho"
    "σ" -> Just "sigma"
    "ς" -> Just "varsigma"
    "𝜍" -> Just "varsigma"
    "τ" -> Just "tau"
    "υ" -> Just "upsilon"
    "ϕ" -> Just "phi"
    "φ" -> Just "varphi"
    "χ" -> Just "chi"
    "ψ" -> Just "psi"
    "ω" -> Just "omega"
    "Γ" -> Just "GAMMA"
    "Δ" -> Just "DELTA"
    "Θ" -> Just "THETA"
    "Λ" -> Just "LAMBDA"
    "Ξ" -> Just "XI"
    "Π" -> Just "PI"
    "Σ" -> Just "SIGMA"
    "Υ" -> Just "UPSILON"
    "Φ" -> Just "PHI"
    "Ψ" -> Just "PSI"
    "Ω" -> Just "OMEGA"
    _   -> Nothing

renderScriptBase :: Exp -> T.Text -> T.Text
renderScriptBase e rendered0 =
  let rendered = T.strip rendered0
  in
  if isAtomic e
     then rendered
     else "{" <> rendered <> "}"

renderScriptArg :: AlignContext -> Exp -> Maybe T.Text
renderScriptArg ctx e = do
  rendered0 <- renderExpIn ctx e
  let rendered = T.strip rendered0
  pure $ if isAtomic e
            then rendered
            else "{" <> rendered <> "}"

renderLimitArg :: AlignContext -> Exp -> Maybe T.Text
renderLimitArg ctx e =
  case e of
    EGrouped xs -> renderExpsIn ctx xs
    _           -> T.strip <$> renderExpIn ctx e

renderAccentArg :: Exp -> T.Text -> T.Text
renderAccentArg e rendered0 =
  let rendered = T.strip rendered0
  in
  if isAtomic e
     then rendered
     else "{" <> rendered <> "}"

isAtomic :: Exp -> Bool
isAtomic e =
  case e of
    ENumber{}       -> True
    EIdentifier{}   -> True
    EMathOperator{} -> True
    EText{}         -> True
    ESymbol{}       -> True
    _               -> False

accentName :: Exp -> Maybe T.Text
accentName e =
  case e of
    ESymbol Accent s -> accentFromChar s
    ESymbol _ s      -> accentFromChar s
    _                -> Nothing

accentFromChar :: T.Text -> Maybe T.Text
accentFromChar s =
  case s of
    "\775" -> Just "dot"   -- COMBINING DOT ABOVE
    "˙"    -> Just "dot"   -- DOT ABOVE
    "\776" -> Just "ddot"  -- COMBINING DIAERESIS
    "¨"    -> Just "ddot"  -- DIAERESIS
    "\770" -> Just "hat"   -- COMBINING CIRCUMFLEX ACCENT
    "ˆ"    -> Just "hat"   -- MODIFIER LETTER CIRCUMFLEX ACCENT
    "\780" -> Just "check" -- COMBINING CARON
    "ˇ"    -> Just "check" -- CARON
    "\771" -> Just "tilde" -- COMBINING TILDE
    "˜"    -> Just "tilde" -- SMALL TILDE
    "\772" -> Just "bar"   -- COMBINING MACRON
    "\8254" -> Just "bar"  -- OVERLINE
    "¯"    -> Just "bar"   -- MACRON
    "\8407" -> Just "vec"  -- COMBINING RIGHT ARROW ABOVE
    "→"    -> Just "vec"   -- RIGHTWARDS ARROW
    "\774" -> Just "breve" -- COMBINING BREVE
    "˘"    -> Just "breve" -- BREVE
    _      -> Nothing

data DelimSide = DelimLeft | DelimRight | DelimMiddle

delimToken :: DelimSide -> T.Text -> T.Text
delimToken side raw =
  case raw of
    ""  -> "none"
    "." -> "none"
    "(" -> "("
    ")" -> ")"
    "[" -> "["
    "]" -> "]"
    "{" -> case side of
      DelimLeft   -> "lbrace"
      DelimRight  -> "rbrace"
      DelimMiddle -> "{"
    "}" -> case side of
      DelimLeft   -> "lbrace"
      DelimRight  -> "rbrace"
      DelimMiddle -> "}"
    "|" -> case side of
      DelimLeft   -> "lline"
      DelimRight  -> "rline"
      DelimMiddle -> "mline"
    "∣" -> case side of
      DelimLeft   -> "lline"
      DelimRight  -> "rline"
      DelimMiddle -> "mline"
    "∥" -> case side of
      DelimLeft   -> "ldline"
      DelimRight  -> "rdline"
      DelimMiddle -> "mline"
    "⟨" -> "langle"
    "⟩" -> "rangle"
    "⌊" -> "lfloor"
    "⌋" -> "rfloor"
    "⌈" -> "lceil"
    "⌉" -> "rceil"
    "⟦" -> "ldbracket"
    "⟧" -> "rdbracket"
    _   -> raw

renderSymbol :: TeXSymbolType -> T.Text -> T.Text
renderSymbol t s =
  case s of
    "∫" -> "int "
    "∑" -> "sum "
    "←" -> " leftarrow "
    "→" -> " toward "
    "↔" -> " leftrightarrow "
    "⇐" -> " dlarrow "
    "⇒" -> " drarrow "
    "⇔" -> " dlrarrow "
    "↑" -> " uparrow "
    "↓" -> " downarrow "
    "≤" -> " <= "
    "≥" -> " >= "
    "≠" -> " <> "
    "≈" -> " approx "
    "∼" -> " sim "
    "≃" -> " simeq "
    "≡" -> " equiv "
    "∝" -> " prop "
    "∥" -> " parallel "
    "∣" -> " divides "
    "∤" -> " ndivides "
    "⊥" -> " ortho "
    "⟂" -> " ortho "
    "∈" -> " in "
    "∉" -> " notin "
    "∋" -> " owns "
    "⊂" -> " subset "
    "⊆" -> " subseteq "
    "⊃" -> " supset "
    "⊇" -> " supseteq "
    "⊄" -> " nsubset "
    "⊈" -> " nsubseteq "
    "⊅" -> " nsupset "
    "⊉" -> " nsupseteq "
    "∪" -> " union "
    "∩" -> " intersection "
    "\\" -> " setminus "
    "∧" -> " and "
    "∨" -> " or "
    "∀" -> "forall "
    "∃" -> " exists "
    "∄" -> " notexists "
    "∂" -> " partial "
    "∇" -> "nabla "
    "∞" -> "infinity"
    "∅" -> "emptyset"
    "+" -> " + "
    "-" | t == Bin  -> " - "
    "-"             -> "-"
    "−" | t == Bin  -> " - "
    "−"             -> "-"
    "=" -> " = "
    "," -> ", "
    ";" -> "; "
    ":" -> ": "
    "/" -> " / "
    "⋅" -> " cdot "
    "·" -> " cdot "
    "×" -> " times "
    _   -> s

largeOpName :: Exp -> Maybe T.Text
largeOpName e =
  case e of
    ESymbol _ "∫" -> Just "int"
    ESymbol _ "∑" -> Just "sum"
    _             -> Nothing

quoteText :: T.Text -> T.Text
quoteText = ("\"" <>) . (<> "\"") . T.concatMap go
 where
  go '"'  = "\\\""
  go '\\' = "\\\\"
  go c    = T.singleton c
