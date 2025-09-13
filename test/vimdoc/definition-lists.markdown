---
vimdoc-prefix: pandoc
abstract: "A short description"
filename: "definition-lists.txt"
author: Author
title: Title
---

## Basic

Term 1
:   Definition I

Term 2
:   Definition II

Term 3
:   Definition III

## Code

<!-- Source: <https://github.com/ggandor/leap.nvim/blob/c4a215acef90749851d85ddba08bc282867b50eb/doc/leap.txt#L283-L294> -->

`leap.opts.keys.next_target`
: Jump to the next available target (use the previous search pattern if no input
  has been given). `:h leap-repeat`

`leap.opts.keys.prev_target`
: Jump to the previous target (revert `next_target`).

`leap.opts.keys.next_group`
: Shift to the next group of labeled targets.

`leap.opts.keys.prev_group`
: Shift to the previous group of labeled targets (revert `next_group`).

## Span

[Important concept]{#important-concept}
: Definition

[I am too long to fit on the same line as a reference, so reference is put above me!]{#i-am-too-long}
: Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
  tempor incididunt ut labore et dolore magna aliqua.

## Commands

In markdown vim commands are represented as inline code starting with colon (ie.
`:MyCommand`), but writer strips the backticks

`:FnlCompileBuffer`{#:FnlCompileBuffer}

:   Compiles current active fennel buffer

`:FnlCompile[!]`{#:FnlCompile}

:   Diff compiles all indexed fennel files
    If bang! is present then forcefully compiles all `source` files

`:[range]Fnl {expr}`{#:Fnl}

:    Evaluates {expr} or range

     ```
     :'<,'>Fnl

     :Fnl (print "Hello World")

     :Fnl (values some_var)
     ```
