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
      "int from 0 to infinity e^{{−x}} dx"
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
      "forall x exists y, nabla f = 0, partial_tu"
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
  , testCase "fallback to TeX for non-centered array alignment" $
      case readTeX "\\begin{array}{lr}a&b\\\\c&d\\end{array}" of
        Left err -> assertFailure ("readTeX failed: " ++ unpack err)
        Right exps ->
          writeStarMath DisplayBlock exps @?= writeTeX exps
  , testCase "fallback to TeX for unsupported forms" $
      case readTeX "\\begin{cases}a&b\\\\c&d\\end{cases}" of
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
