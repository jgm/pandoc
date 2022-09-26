---
title: pandoc-lua
section: 1
date: September 22, 2022
---

# SYNOPSIS

`pandoc-lua` [*options*] [*script* [*args*]]

# DESCRIPTION

`pandoc-lua` is a standalone Lua interpreter with behavior similar
to that of the standard `lua` executable, but exposing all of
pandoc's Lua libraries. All `pandoc.*` packages, as well as the
packages `re` and `lpeg`, are available via global variables.
Furthermore, the globals `PANDOC_VERSION`, `PANDOC_STATE`, and
`PANDOC_API_VERSION` are set at startup.

If no script argument is given, then the script is assumed to be
passed in via *stdin*. Interactive mode is not supported at this
time.

When called without the option `-E`, the interpreter checks for an
environment variable `LUA_INIT` before running any argument. If
the variable content has the format *`@filename`*, then
`pandoc-lua` executes the file. Otherwise, `pandoc-lua` executes
the string itself.

# OPTIONS

`-e stat`
:   Execute statement `stat`.

`-l mod`
:   If mod has the pattern `g=m`, then require library `m` into
    global `g`; otherwise require library `mod` into global
    `mod`.

`-v`
:   Show version information.

`-i`
:   Not supported yet; print a warning to that effect.

`-E`
:   Ignore environment variables. This is not fully implemented
    yet and only ignores the `LUA_INIT` variable. Other variables
    like `LUA_PATH` and `LUA_CPATH` are **not** ignored.

`-W`
:   Turn warnings on.

# AUTHORS

Copyright 2022 John MacFarlane (jgm@berkeley.edu) and
contributors. Released under the [GPL], version 2 or later. This
software carries no warranty of any kind. (See COPYRIGHT for full
copyright and warranty notices.)

Lua: Copyright 1994-2022 Lua.org, PUC-Rio.

[GPL]: https://www.gnu.org/copyleft/gpl.html "GNU General Public License"

