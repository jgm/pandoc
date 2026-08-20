{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{- |
   Module      : Text.Pandoc.App.Completion
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Generation of shell completion scripts for bash, zsh and fish.
The scripts are generated at runtime from pandoc's single list of
command-line options ('OptionSpec'), together with the completion
metadata that each option carries (its 'CompletionKind' and a short
description).  All completions are static: the lists of formats,
styles, engines and data files are embedded into the generated script,
so no call to pandoc is made while completing.
-}
module Text.Pandoc.App.Completion ( generateCompletion ) where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.List as L
import qualified Data.Text as T
import System.Console.GetOpt (ArgDescr (..))

import Text.Pandoc.App.Opt (CompletionShell (..), OptionSpec (..),
                            CompletionKind (..))

-- | Generate a completion script for the given shell.  The completion
-- behaviour and descriptions are taken from the per-option 'OptionSpec'
-- data, so the script cannot drift from the actual options.
generateCompletion :: CompletionShell
                   -> [OptionSpec]   -- ^ the option list
                   -> [Text]         -- ^ input formats
                   -> [Text]         -- ^ output formats
                   -> [Text]         -- ^ highlighting style names
                   -> [String]       -- ^ PDF engines
                   -> [String]       -- ^ data files
                   -> IO Text
generateCompletion Bash   = bashScript
generateCompletion Zsh    = zshScript
generateCompletion Fish   = fishScript

-- | The list of all option names (short and long), space separated.
allOptionNames :: [OptionSpec] -> String
allOptionNames opts =
  unwords [ name | OptionSpec shorts longs _ _ _ <- opts
                 , name <- map (\c -> '-' : [c]) shorts ++
                             map ("--" ++) longs ]

-- | The completion kind and description for an option.  This is taken
-- directly from the 'OptionSpec'; there is no separate specification to
-- keep in sync.
optionKindDesc :: OptionSpec -> (CompletionKind, Text)
optionKindDesc (OptionSpec _ _ _ k desc) = (k, desc)

placeholder :: ArgDescr a -> Maybe String
placeholder (ReqArg _ s) = Just s
placeholder (OptArg _ s) = Just s
placeholder _ = Nothing

-- | Whether an option needs an explicit @case "${prev}"@ arm in the bash
-- script.  Options that just take a file or are boolean flags fall
-- through to the default file completion, so they need no arm.
isCompletableKind :: CompletionKind -> Bool
isCompletableKind OptFlag = False
isCompletableKind Files   = False
isCompletableKind _       = True

-- | The argument passed to @compgen -W@ for an option of the given kind.
-- Dynamic kinds reference the shell variables that pandoc fills in;
-- fixed enumerations are listed verbatim.
prevSource :: CompletionKind  -- ^ completion kind
           -> String          -- ^ engine list (already space-joined)
           -> String
prevSource InputFormats    _ = "${informats}"
prevSource OutputFormats   _ = "${outformats}"
prevSource HighlightStyles _ = "${highlight_styles}"
prevSource DataFiles       _ = "${datafiles}"
prevSource Engines         e = e
prevSource (Fixed vs)      _ = unwords vs
prevSource OptFlag         _ = ""
prevSource Files           _ = ""

----------------------------------------------------------------------
-- bash
----------------------------------------------------------------------

-- | The bash completion script reproduces the historical script that
-- was previously generated from @data/bash_completion.tpl@.  The list
-- of options completed per value (the @case "${prev}"@ arms) is derived
-- from the option list, so it cannot drift from the actual options.
bashScript :: [OptionSpec] -> [Text] -> [Text] -> [Text] -> [String]
           -> [String] -> IO Text
bashScript opts informats outformats hstyles engines datafiles = do
  let optsStr   = allOptionNames opts
      infStr    = unwords (map T.unpack informats)
      outfStr   = unwords (map T.unpack outformats)
      hsStr     = unwords (map T.unpack hstyles)
      dfStr     = unwords datafiles
      engStr    = unwords engines
      caseBody  = concatMap armToLines (bashCaseArms opts engStr)
  return $ T.unlines $
    [ "# This script enables bash autocompletion for pandoc.  To enable"
    , "# bash completion, add this to your .bashrc:"
    , "# eval \"$(pandoc --completion=bash)\""
    , ""
    , "_pandoc()"
    , "{"
    , "    local cur prev opts informats outformats highlight_styles datafiles"
    , "    COMPREPLY=()"
    , "    cur=\"${COMP_WORDS[COMP_CWORD]}\""
    , "    prev=\"${COMP_WORDS[COMP_CWORD-1]}\""
    , ""
    , "    # These should be filled in by pandoc:"
    , T.pack $ "    opts=\"" ++ optsStr ++ "\""
    , T.pack $ "    informats=\"" ++ infStr ++ "\""
    , T.pack $ "    outformats=\"" ++ outfStr ++ "\""
    , T.pack $ "    highlight_styles=\"" ++ hsStr ++ "\""
    , T.pack $ "    datafiles=\"" ++ dfStr ++ "\""
    , ""
    , "    case \"${prev}\" in"
    ]
    ++ caseBody ++
    [ "         *)"
    , "             ;;"
    , "    esac"
    , ""
    , "    case \"${cur}\" in"
    , "         -*)"
    , "             COMPREPLY=( $(compgen -W \"${opts}\" -- ${cur}) )"
    , "             return 0"
    , "             ;;"
    , "         *)"
    , "             local IFS=$'\\n'"
    , "             COMPREPLY=( $(compgen -X '' -f \"${cur}\") )"
    , "             return 0"
    , "             ;;"
    , "    esac"
    , ""
    , "}"
    , ""
    , "complete -o filenames -o bashdefault -F _pandoc pandoc"
    ]

-- | The @case "${prev}"@ arms, one per distinct completion source,
-- merging all options that share the same source so that (for example)
-- @--from@ and @--read@ end up in a single arm.
bashCaseArms :: [OptionSpec] -> String -> [(String, [String])]
bashCaseArms opts engStr =
  let arms = [ (prevSource k engStr, names)
             | o@(OptionSpec shorts longs _ _ _) <- opts
             , let (k, _) = optionKindDesc o
             , isCompletableKind k
             , let names = map (\c -> '-' : [c]) shorts ++
                           map ("--" ++) longs ]
  in mergeArms arms

-- | Merge arms that share the same completion source, preserving the
-- order in which the sources first appear in the option list.
mergeArms :: [(String, [String])] -> [(String, [String])]
mergeArms = L.foldl' go []
  where go [] (src, ns) = [(src, ns)]
        go (x@(s, ns0) : xs) (src, ns)
          | s == src  = (s, ns0 ++ ns) : xs
          | otherwise = x : go xs (src, ns)

-- | Render one merged arm as the four lines of a bash @case@ body.
armToLines :: (String, [String]) -> [Text]
armToLines (src, names) =
  let pat = intercalate "|" names
  in [ T.pack ("         " ++ pat ++ ")")
     , T.pack ("             COMPREPLY=( $(compgen -W \"" ++ src ++
              "\" -- ${cur}) )")
     , "             return 0"
     , "             ;;" ]

----------------------------------------------------------------------
-- zsh
----------------------------------------------------------------------

zshScript :: [OptionSpec] -> [Text] -> [Text] -> [Text] -> [String]
          -> [String] -> IO Text
zshScript opts informats outformats hstyles engines datafiles = do
  let infStr  = unwords (map T.unpack informats)
      outfStr = unwords (map T.unpack outformats)
      hsStr   = unwords (map T.unpack hstyles)
      dfStr   = unwords datafiles
      engStr  = unwords engines
      action k mbP = T.pack $ zshAction k mbP infStr outfStr hsStr dfStr engStr
      optLines = concat
        [ zshOptionLine o action
        | o@(OptionSpec _shorts _longs _ad _ _) <- opts ]
  return $ T.unlines $
    [ "#compdef pandoc"
    , ""
    , "_pandoc() {"
    , "  local -a args"
    , "  args=("
    ]
    ++ optLines
    ++ [ "    '*:files:_files'"
       , "  )"
       , "  _arguments -s -S $args"
       , "}"
       , ""
       , "_pandoc \"$@\""
       ]

-- | Produce one or more @_arguments@ spec lines (one per name) for an
-- option.  The description and action are embedded in single quotes.
zshOptionLine :: OptionSpec
              -> (CompletionKind -> Maybe String -> Text)
              -> [Text]
zshOptionLine (OptionSpec shorts longs ad k desc) action =
  let desc' = escapeZshDesc desc
      act   = action k (placeholder ad)
      line name = T.pack ("    '" ++ name ++ "[") <> desc' <>
                  T.pack ("]") <> act <> T.pack "'"
  in map line (map (\c -> '-' : [c]) shorts ++ map ("--" ++) longs)

-- | The zsh completion action for a given kind.  All lists are embedded
-- statically.
zshAction :: CompletionKind -> Maybe String -> String -> String -> String
          -> String -> String -> String
zshAction OptFlag _ _ _ _ _ _ = ""
zshAction Files mbP _ _ _ _ _ =
  ":" ++ maybe "FILE" id mbP ++ ":_files"
zshAction (Fixed vs) mbP _ _ _ _ _ =
  ":" ++ maybe "VALUE" id mbP ++ ":(" ++ unwords vs ++ ")"
zshAction InputFormats _ inf _ _ _ _ = ":FORMAT:(" ++ inf ++ ")"
zshAction OutputFormats _ _ outf _ _ _ = ":FORMAT:(" ++ outf ++ ")"
zshAction HighlightStyles _ _ _ hs _ _ = ":STYLE:(" ++ hs ++ ")"
zshAction DataFiles _ _ _ _ df _ = ":FILE:(" ++ df ++ ")"
zshAction Engines _ _ _ _ _ eng = ":PROGRAM:(" ++ eng ++ ")"

-- | Escape a description for embedding inside a single-quoted zsh
-- @_arguments@ spec.  Single quotes are the only character that needs
-- special treatment; the descriptions are kept free of colons and
-- square brackets.
escapeZshDesc :: Text -> Text
escapeZshDesc = T.replace "'" "'\\''"

----------------------------------------------------------------------
-- fish
----------------------------------------------------------------------

fishScript :: [OptionSpec] -> [Text] -> [Text] -> [Text] -> [String]
           -> [String] -> IO Text
fishScript opts informats outformats hstyles engines datafiles = do
  let infStr  = unwords (map T.unpack informats)
      outfStr = unwords (map T.unpack outformats)
      hsStr   = unwords (map T.unpack hstyles)
      dfStr   = unwords datafiles
      engStr  = unwords engines
      argPart k mbP = T.pack $ fishArg k mbP infStr outfStr hsStr dfStr engStr
      optLines = concat
        [ fishOptionLine o argPart
        | o@(OptionSpec _shorts _longs _ad _ _) <- opts ]
  return $ T.unlines optLines

fishOptionLine :: OptionSpec
               -> (CompletionKind -> Maybe String -> Text)
               -> [Text]
fishOptionLine (OptionSpec shorts longs ad k desc) argPart =
  let shortPart = case shorts of
                   [c] -> T.pack (" -s " ++ [c])
                   _   -> ""
      descPart = if T.null desc
                   then ""
                   else T.pack " -d \"" <> escapeFishDesc desc <> T.pack "\""
  in [ T.pack "complete -c pandoc" <> shortPart <>
       T.pack (" -l " ++ l) <> descPart <>
       argPart k (placeholder ad)
     | l <- take 1 longs ]

fishArg :: CompletionKind -> Maybe String -> String -> String -> String
        -> String -> String -> String
fishArg OptFlag _ _ _ _ _ _ = ""
fishArg Files _ _ _ _ _ _ = " -r"
fishArg (Fixed vs) _ _ _ _ _ _ = " -r -a \"" ++ unwords vs ++ "\""
fishArg InputFormats _ inf _ _ _ _ = " -r -a \"" ++ inf ++ "\""
fishArg OutputFormats _ _ outf _ _ _ = " -r -a \"" ++ outf ++ "\""
fishArg HighlightStyles _ _ _ hs _ _ = " -r -a \"" ++ hs ++ "\""
fishArg DataFiles _ _ _ _ df _ = " -r -a \"" ++ df ++ "\""
fishArg Engines _ _ _ _ _ eng = " -r -a \"" ++ eng ++ "\""

-- | Escape a description for a fish completion @-d@ argument, which is
-- wrapped in double quotes.
escapeFishDesc :: Text -> Text
escapeFishDesc = T.replace "\\" "\\\\"
               . T.replace "\"" "\\\""
               . T.replace "$" "\\$"
