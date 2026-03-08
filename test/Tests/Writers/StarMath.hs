{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.StarMath (tests) where

import Data.Text (Text, unpack)
import Test.Tasty
import Test.Tasty.HUnit
import Text.TeXMath (DisplayType(..), readTeX, writeTeX)
import Text.Pandoc.Writers.StarMath (writeStarMath)

tests :: [TestTree]
tests =
  [ testCase "dot + text subscript" $
      star "\\dot{Q}_{\\text{dem}}(t)=\\dot{Q}_{\\text{eb}}(t)+\\dot{Q}_{\\text{dis}}(t)+\\dot{Q}_{\\text{boil}}(t),"
      @?=
      "{dot Q}_\"dem\"(t) = {dot Q}_\"eb\"(t) + {dot Q}_\"dis\"(t) + {dot Q}_\"boil\"(t),"
  , testCase "common accents" $
      star "\\ddot{x}+\\hat{x}+\\tilde{x}+\\vec{x}+\\bar{x}"
      @?=
      "ddot x + hat x + tilde x + vec x + bar x"
  , testCase "fraction" $
      star "\\frac{a+b}{c}"
      @?=
      "{{a + b} over c}"
  , testCase "square root" $
      star "\\sqrt{x+1}"
      @?=
      "sqrt {{x + 1}}"
  , testCase "nth root" $
      star "\\sqrt[3]{x}"
      @?=
      "nroot {3} {x}"
  , testCase "subscript and superscript" $
      star "x_i^2"
      @?=
      "x_i^2"
  , testCase "superscript with grouped base" $
      star "(a+b)^2"
      @?=
      "(a + b)^2"
  , testCase "delimited fraction" $
      star "\\left(\\frac{a}{b}\\right)"
      @?=
      "left ( {a over b} right )"
  , testCase "delimited braces" $
      star "\\left\\{\\frac{a}{b}\\right\\}"
      @?=
      "left lbrace {a over b} right rbrace"
  , testCase "one-sided delimiter uses none" $
      star "\\left. x \\right|"
      @?=
      "left none x right rline"
  , testCase "middle delimiter uses mline" $
      star "\\left( x \\middle| y \\right)"
      @?=
      "left ( x mline y right )"
  , testCase "operator mapping cdot" $
      star "a\\cdot b"
      @?=
      "a cdot b"
  , testCase "binomial (NoLineFrac)" $
      star "\\binom{n}{k}"
      @?=
      "left ( {n / k} right )"
  , testCase "integral without limits" $
      star "\\int x\\,dx"
      @?=
      "int x dx"
  , testCase "integral with lower and upper limits" $
      star "\\int_0^1 x\\,dx"
      @?=
      "int from 0 to 1 x dx"
  , testCase "integral with infinite upper limit" $
      star "\\int_{0}^{\\infty} e^{-x}\\,dx"
      @?=
      "int from 0 to infinity e^{{- x}} dx"
  , testCase "sum with lower and upper limits" $
      star "\\sum_{i=1}^{n} i"
      @?=
      "sum from i = 1 to n i"
  , testCase "sum with symbolic term" $
      star "\\sum_{k=1}^{n} a_k"
      @?=
      "sum from k = 1 to n a_k"
  , testCase "greek letter mapping" $
      star "\\alpha + \\beta + \\Gamma + \\Omega"
      @?=
      "%ialpha + %ibeta + %GAMMA + %OMEGA"
  , testCase "greek variant mapping" $
      star "\\phi + \\varphi + \\epsilon + \\varepsilon + \\vartheta"
      @?=
      "%iphi + %ivarphi + %ivarepsilon + %iepsilon + %ivartheta"
  , testCase "arrow mapping" $
      star "x \\to y, x \\leftarrow y, x \\Rightarrow y, x \\Leftrightarrow y"
      @?=
      "x toward y, x leftarrow y, x drarrow y, x dlrarrow y"
  , testCase "set and relation symbol mapping" $
      star "A \\subseteq B, A \\cup B, x \\in A, x \\notin B"
      @?=
      "A subseteq B, A union B, x in A, x notin B"
  , testCase "logic and calculus symbol mapping" $
      star "\\forall x \\exists y, \\nabla f = 0, \\partial_t u"
      @?=
      "forall x exists y, nabla f = 0, partial_t u"
  , testCase "greek identifier spacing in products" $
      star "E_{k+1}=E_{k+\\frac12}-\\frac{\\dot{Q}_{\\text{dis},k}\\Delta t}{\\eta_{\\text{dis}}},"
      @?=
      "E_{{k + 1}} = E_{{k + {1 over 2}}} - {{{dot Q}_{{\"dis\", k}} %DELTA t} over %ieta_\"dis\"},"
  , testCase "math operators rendered as functions" $
      star "E_{k+1}\\leftarrow\\min\\!\\left(E^{\\text{cap}}_{\\text{s}},\\max(0,E_{k+1})\\right)."
      @?=
      "E_{{k + 1}} leftarrow func min left ( E_\"s\"^\"cap\", func max(0, E_{{k + 1}}) right )."
  , testCase "nested function with left delimiter spacing" $
      star "P_{\\text{ch}, k}=\\max\\!\\left(0,\\min\\!\\left(P'_{\\text{pv}, k},\\,P^{\\text{cap}}_{\\text{ch}},\\,P_{\\text{ch,head}, k}\\right)\\right)."
      @?=
      "P_{{\"ch\", k}} = func max left ( 0, func min left ( P_{{\"pv\", k}}^′,  P_\"ch\"^\"cap\",  P_{{\"ch,head\", k}} right ) right )."
  , testCase "quad spacing command" $
      star "a,\\quad b"
      @?=
      "a, ~ b"
  , testCase "qquad spacing command" $
      star "a,\\qquad b"
      @?=
      "a, ~~ b"
  , testCase "greek token separator before scripted identifier" $
      star "f_{\\text{eff}}=(1-\\lambda)f_{\\text{m}}+\\lambda f_{\\text{l}}."
      @?=
      "f_\"eff\" = (1 - %ilambda) f_\"m\" + %ilambda f_\"l\"."
  , testCase "mathcal styled token before left delimiter" $
      star "\\min\\ \\mathcal{J}\\left(P^{\\text{cap}}_{\\text{pv}},N_{\\text{u}},P^{\\text{cap}}_{\\text{eb}}\\right)"
      @?=
      "func min ital J left ( P_\"pv\"^\"cap\", N_\"u\", P_\"eb\"^\"cap\" right )"
  , testCase "leading binary operator gets neutral lhs" $
      star "\\times\\Delta t"
      @?=
      "{} times %DELTA t"
  , testCase "matrix environment" $
      star "\\begin{matrix}a&b\\\\c&d\\end{matrix}"
      @?=
      "matrix { a # b ## c # d }"
  , testCase "pmatrix environment" $
      star "\\begin{pmatrix}a&b\\\\c&d\\end{pmatrix}"
      @?=
      "left ( matrix { a # b ## c # d } right )"
  , testCase "bmatrix environment" $
      star "\\begin{bmatrix}a&b\\\\c&d\\end{bmatrix}"
      @?=
      "left [ matrix { a # b ## c # d } right ]"
  , testCase "vmatrix environment" $
      star "\\begin{vmatrix}a&b\\\\c&d\\end{vmatrix}"
      @?=
      "left lline matrix { a # b ## c # d } right rline"
  , testCase "Vmatrix environment" $
      star "\\begin{Vmatrix}a&b\\\\c&d\\end{Vmatrix}"
      @?=
      "left ldline matrix { a # b ## c # d } right rdline"
  , testCase "array with left/right alignment" $
      star "\\begin{array}{lr}a&b\\\\c&d\\end{array}"
      @?=
      "matrix { alignl a # alignr b ## alignl c # alignr d }"
  , testCase "aligned array keeps fractions centered" $
      star "\\begin{array}{l}\\frac{AAA}{B}\\end{array}"
      @?=
      "matrix { alignl {{alignc {AAA}} over {alignc B}} }"
  , testCase "cases environment" $
      star "\\begin{cases}a, & x>0\\\\ b, & x\\le 0\\end{cases}"
      @?=
      "left lbrace matrix { alignl a, # alignl x>0 ## alignl b, # alignl x <= 0 } right none"
  , testCase "cases with negative log fraction" $
      star "t_{\\text{pb,disc}}=\\begin{cases}\\dfrac{C_{\\text{tot}}}{B}, & r=0,\\\\\\dfrac{-\\ln\\!\\left(1-rC_{\\text{tot}}/B\\right)}{\\ln(1+r)}, & r>0\\text{ and }1-rC_{\\text{tot}}/B>0,\\\\\\infty, & \\text{otherwise}.\\end{cases}"
      @?=
      "t_\"pb,disc\" = left lbrace matrix { alignl {{alignc {C_\"tot\"}} over {alignc B}}, # alignl r = 0, ## alignl {{alignc {- func ln left ( 1 - rC_\"tot\" / B right )}} over {alignc {func ln(1 + r)}}}, # alignl r>0\" and \"1 - rC_\"tot\" / B>0, ## alignl infinity, # alignl \"otherwise\". } right none"
  , testCase "fallback to TeX for unsupported forms" $
      case readTeX "\\phantom{x}+1" of
        Left err -> assertFailure ("readTeX failed: " ++ unpack err)
        Right exps ->
          writeStarMath DisplayBlock exps @?= writeTeX exps
  , testCase "fallback to TeX for under/over constructs" $
      case readTeX "\\underbrace{x+y}_{z}+\\overbrace{x+y}^{z}" of
        Left err -> assertFailure ("readTeX failed: " ++ unpack err)
        Right exps ->
          writeStarMath DisplayBlock exps @?= writeTeX exps
  ]

star :: Text -> Text
star inp =
  case readTeX inp of
    Left err   -> error ("readTeX failed in test: " ++ unpack err)
    Right exps -> writeStarMath DisplayBlock exps
