% Using the pandoc API
% John MacFarlane

intro - pandoc can be used as a library, to write
your own custom conversion tools, or to power
a web application.

# Basic usage

a simple example

# The Pandoc structure

blocks/inlines

# Readers and writers

getReader, getWriter

# Options

inc extensions

# Builder

Inlines vs Inline, etc.
monoid

example: report from CSV data

# The PandocMonad class

motivations
Class.
advanced: custom PandocMonad instances

# Templates and other data files

# Handling errors and warnings

# Generic transformations

Walk and syb for AST transformations

# Filters

just the basic idea of toJSONFilter
the rest can be left to filters.md

# Self-contained


# PDF

# Creating a front-end

Text.Pandoc.App

