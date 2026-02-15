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
  , TeXSymbolType(..)
  )

-- | Render TeXMath expressions as StarMath syntax.
-- Falls back to TeX output for expressions that are not yet supported.
writeStarMath :: DisplayType -> [Exp] -> T.Text
writeStarMath _dt exps =
  case renderExps exps of
    Just rendered -> T.strip rendered
    Nothing       -> writeTeX exps

renderExps :: [Exp] -> Maybe T.Text
renderExps = fmap T.concat . mapM renderExp

renderExp :: Exp -> Maybe T.Text
renderExp e =
  case e of
    ENumber t       -> Just t
    EIdentifier t   -> Just (renderIdentifier t)
    EMathOperator t -> Just t
    ESymbol t s     -> Just (renderSymbol t s)
    EText _ t       -> Just (quoteText t)
    ESpace _        -> Just " "
    EGrouped xs     -> ("{" <>) . (<> "}") <$> renderExps xs
    EStyled _ xs    -> renderExps xs

    EFraction frac num den -> do
      num' <- renderExp num
      den' <- renderExp den
      pure $ case frac of
        NoLineFrac -> "{" <> num' <> " / " <> den' <> "}"
        _          -> "{" <> num' <> " over " <> den' <> "}"

    ESqrt x -> ("sqrt {" <>) . (<> "}") <$> renderExp x
    ERoot idx rad -> do
      idx' <- renderExp idx
      rad' <- renderExp rad
      pure $ "nroot {" <> idx' <> "} {" <> rad' <> "}"

    EDelimited op cl xs -> do
      body <- renderDelimitedBody xs
      let op' = delimToken DelimLeft op
      let cl' = delimToken DelimRight cl
      pure $ "left " <> op' <> " " <> body <> " right " <> cl'

    ESub base sub -> do
      case largeOpName base of
        Just op -> do
          sub' <- renderLimitArg sub
          pure $ op <> " from " <> sub' <> " "
        Nothing -> do
          base' <- renderExp base
          sub'  <- renderScriptArg sub
          pure $ renderScriptBase base base' <> "_" <> sub'

    ESuper base sup -> do
      case largeOpName base of
        Just op -> do
          sup' <- renderLimitArg sup
          pure $ op <> " to " <> sup' <> " "
        Nothing -> do
          base' <- renderExp base
          sup'  <- renderScriptArg sup
          pure $ renderScriptBase base base' <> "^" <> sup'

    ESubsup base sub sup -> do
      case largeOpName base of
        Just op -> do
          sub' <- renderLimitArg sub
          sup' <- renderLimitArg sup
          pure $ op <> " from " <> sub' <> " to " <> sup' <> " "
        Nothing -> do
          base' <- renderExp base
          sub'  <- renderScriptArg sub
          sup'  <- renderScriptArg sup
          pure $ renderScriptBase base base' <> "_" <> sub' <> "^" <> sup'

    EOver _ base over
      | Just accent <- accentName over -> do
          base' <- renderExp base
          pure $ accent <> " " <> renderAccentArg base base'
      | otherwise -> Nothing

    EUnder _ base under ->
      case largeOpName base of
        Just op -> do
          under' <- renderLimitArg under
          pure $ op <> " from " <> under' <> " "
        Nothing -> Nothing
    EUnderover _ base under over ->
      case largeOpName base of
        Just op -> do
          under' <- renderLimitArg under
          over'  <- renderLimitArg over
          pure $ op <> " from " <> under' <> " to " <> over' <> " "
        Nothing -> Nothing
    EArray aligns rows -> renderMatrix aligns rows
    EPhantom{}   -> Nothing
    _            -> Nothing

renderDelimitedPart :: Either T.Text Exp -> Maybe T.Text
renderDelimitedPart p =
  case p of
    Left t  -> Just $ " " <> delimToken DelimMiddle t <> " "
    Right x -> renderExp x

renderDelimitedBody :: [Either T.Text Exp] -> Maybe T.Text
renderDelimitedBody xs = T.strip <$> (fmap T.concat $ mapM renderDelimitedPart xs)

renderMatrix :: [Alignment] -> [[[Exp]]] -> Maybe T.Text
renderMatrix aligns rows
  | not (all (== AlignCenter) aligns) = Nothing
  | otherwise = do
      rows' <- mapM renderMatrixRow rows
      pure $ "matrix { " <> T.intercalate " ## " rows' <> " }"

renderMatrixRow :: [[Exp]] -> Maybe T.Text
renderMatrixRow cells = do
  cells' <- mapM renderMatrixCell cells
  pure $ T.intercalate " # " cells'

renderMatrixCell :: [Exp] -> Maybe T.Text
renderMatrixCell [] = Just "{}"
renderMatrixCell xs = do
  rendered <- renderExps xs
  let stripped = T.strip rendered
  pure $ if T.null stripped then "{}" else stripped

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

renderScriptArg :: Exp -> Maybe T.Text
renderScriptArg e = do
  rendered0 <- renderExp e
  let rendered = T.strip rendered0
  pure $ if isAtomic e
            then rendered
            else "{" <> rendered <> "}"

renderLimitArg :: Exp -> Maybe T.Text
renderLimitArg e =
  case e of
    EGrouped xs -> renderExps xs
    _           -> T.strip <$> renderExp e

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
renderSymbol _ s =
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
    "-" -> " - "
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
