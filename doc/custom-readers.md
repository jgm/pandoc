---
author:
- John MacFarlane
date: 'November 18, 2021'
title: Creating Custom Pandoc Readers in Lua
---

# Introduction

If you need to parse a format not already handled by pandoc,
you can create a custom reader using the [Lua] language.
Pandoc has a built-in Lua interpreter, so you needn't
install any additional software to do this.

[Lua]: https://www.lua.org

A custom writer is a Lua file that defines a function
called `Reader`, which takes two arguments:

- a string, the raw input to be parsed
- optionally, a table of reader options, e.g.
  `{ columns = 62, standalone = true }`.

The `Reader` function should return a `Pandoc` AST.
This can be created using functions in the [`pandoc` module],
which is automatically in scope.  (Indeed, all of the utility
functions that are available for [Lua filters] are available
in custom readers, too.)

[Lua filters]: https://pandoc.org/lua-filters.html
[`pandoc` module]: https://pandoc.org/lua-filters.html#module-pandoc

A minimal example would be

```lua
function Reader(input)
  return pandoc.Pandoc({ pandoc.CodeBlock(input) })
end
```

This just returns a document containing a big code block with
all of the input.

In a nontrivial reader, you'll want to parse the input.
You can do this using standard Lua library functions
(for example, the [patterns] library), or with the powerful
and fast [lpeg] parsing library, which is automatically in scope.
You can also use external Lua libraries (for example,
an XML parser).

[patterns]: http://lua-users.org/wiki/PatternsTutorial
[lpeg]: http://www.inf.puc-rio.br/~roberto/lpeg/

# Example: plain text reader

This is a simple example using [lpeg] to parse the input
into space-separated strings and blankline-separated paragraphs.

```lua
-- A sample custom reader that just parses text into blankline-separated
-- paragraphs with space-separated words.

-- For better performance we put these functions in local variables:
local P, S, R, Cf, Cc, Ct, V, Cs, Cg, Cb, B, C, Cmt =
  lpeg.P, lpeg.S, lpeg.R, lpeg.Cf, lpeg.Cc, lpeg.Ct, lpeg.V,
  lpeg.Cs, lpeg.Cg, lpeg.Cb, lpeg.B, lpeg.C, lpeg.Cmt

local whitespacechar = S(" \t\r\n")
local wordchar = (1 - whitespacechar)
local spacechar = S(" \t")
local newline = P"\r"^-1 * P"\n"
local blanklines = newline * (spacechar^0 * newline)^1
local endline = newline - blanklines

-- Grammar
G = P{ "Pandoc",
  Pandoc = Ct(V"Block"^0) / pandoc.Pandoc;
  Block = blanklines^0 * V"Para" ;
  Para = Ct(V"Inline"^1) / pandoc.Para;
  Inline = V"Str" + V"Space" + V"SoftBreak" ;
  Str = wordchar^1 / pandoc.Str;
  Space = spacechar^1 / pandoc.Space;
  SoftBreak = endline / pandoc.SoftBreak;
}

function Reader(input)
  return lpeg.match(G, input)
end
```

Example of use:

```
% pandoc -f plain.lua -t native
*Hello there*, this is plain text with no formatting
except paragraph breaks.

- Like this one.
^D
[ Para
    [ Str "*Hello"
    , Space
    , Str "there*,"
    , Space
    , Str "this"
    , Space
    , Str "is"
    , Space
    , Str "plain"
    , Space
    , Str "text"
    , Space
    , Str "with"
    , Space
    , Str "no"
    , Space
    , Str "formatting"
    , SoftBreak
    , Str "except"
    , Space
    , Str "paragraph"
    , Space
    , Str "breaks."
    ]
, Para
    [ Str "-"
    , Space
    , Str "Like"
    , Space
    , Str "this"
    , Space
    , Str "one."
    ]
]
```

# Example: a RIS bibliography reader

This is a parser for [RIS bibliography] files.  It can be used
to convert them to CSL JSON or YAML, BibTeX, or BibLaTeX.

[RIS bibliography]: https://en.wikipedia.org/wiki/RIS_(file_format)

```lua
-- A sample custom reader for RIS bibliography format
-- https://en.wikipedia.org/wiki/RIS_(file_format)
-- The references are converted to inline pandoc/CSL YAML
-- references in the metadata.

local inspect = require"inspect"

local types =
  { ABST = "article",
    ADVS = "motion-picture",
    AGGR = "dataset",
    ANCIENT = "book",
    ART = "graphic",
    BILL = "bill",
    BLOG = "post-weblog",
    BOOK = "book",
    CASE = "legal_case",
    CHAP = "chapter",
    CHART = "graphic",
    CLSWK = "book",
    COMP = "program",
    CONF = "paper-conference",
    CPAPER = "paper-conference",
    CTLG = "catalog",
    DATA = "dataset",
    DBASE = "dataset",
    DICT = "book",
    EBOOK = "book",
    ECHAP = "chapter",
    EDBOOK = "book",
    EJOUR = "article",
    WEB = "webpage",
    ENCYC = "entry-encyclopedia",
    EQUA = "figure",
    FIGURE = "figure",
    GEN = "entry",
    GOVDOC = "report",
    GRANT = "report",
    HEAR = "report",
    ICOMM = "personal_communication",
    INPR = "article-journal",
    JFULL = "article-journal",
    JOUR = "article-journal",
    LEGAL = "legal_case",
    MANSCPT = "manuscript",
    MAP = "map",
    MGZN = "article-magazine",
    MPCT = "motion-picture",
    MULTI = "webpage",
    MUSIC = "musical_score",
    NEWS = "article-newspaper",
    PAMP = "pamphlet",
    PAT = "patent",
    PCOMM = "personal_communication",
    RPRT = "report",
    SER = "article",
    SLIDE = "graphic",
    SOUND = "musical_score",
    STAND = "report",
    STAT = "legislation",
    THES = "thesis",
    UNBILL = "bill",
    UNPB = "unpublished",
    VIDEO = "graphic"
  }

local function inlines(s)
  local ils = {}
  for t in string.gmatch(s, "%S+") do
    if #ils == 0 then
      ils = {pandoc.Str(t)}
    else
      table.insert(ils, pandoc.Space())
      table.insert(ils, pandoc.Str(t))
    end
  end
  return pandoc.MetaInlines(ils)
end

local function clean(refpairs)
  local ref = {}
  for i = 1, #refpairs do
    local k,v = table.unpack(refpairs[i])
    if k == "TY" then
      ref["type"] = types[v]
    elseif k == "VL" then
      ref.volume = v
    elseif k == "KW" then
      ref.keyword = v
    elseif k == "PB" then
      ref.publisher = v
    elseif k == "CY" or k == "PP" then
      ref["publisher-place"] = v
    elseif k == "SP" then
      if ref.page then
        ref.page = v .. ref.page
      else
        ref.page = v
      end
    elseif k == "EP" then
      if ref.page then
        ref.page = ref.page .. "-" .. v
      else
        ref.page = "-" .. v
      end
    elseif k == "AU" or k == "A1" or k == "A2" or k == "A3" then
      if ref.author then
        table.insert(ref.author, inlines(v))
      else
        ref.author = {inlines(v)}
      end
    elseif k == "TI" or k == "T1" or k == "CT" or
            (k == "BT" and ref.type == "book") then
      ref.title = inlines(v)
    elseif k == "ET" then
      ref.edition = inlines(v)
    elseif k == "NV" then
      ref["number-of-volumes"] = inlines(v)
    elseif k == "AB" then
      ref.abstract = inlines(v)
    elseif k == "ED" then
      if ref.editor then
        table.insert(ref.editor, inlines(v))
      else
        ref.editor = {inlines(v)}
      end
    elseif k == "JO" or k == "JF" or k == "T2" or
             (k == "BT" and ref.type ~= "book") then
      ref["container-title"] = inlines(v)
    elseif k == "PY" or k == "Y1" then
      ref.issued = v
    elseif k == "IS" then
      ref.issue = v
    elseif k == "SN" then
      ref.ISSN = v
    elseif k == "L" then
      ref.lang = v
    elseif k == "UR" or k == "LK" then
      ref.URL = v
    end
  end
  return ref
end

function Reader(input, reader_options)
  local refs = {}
  local thisref = {}
  local ids = {}
  for line in string.gmatch(input, "[^\n]*") do
    key, val = string.match(line, "([A-Z][A-Z0-9])  %- (.*)")
    if key == "ER" then
      -- clean up fields
      local newref = clean(thisref)
      -- ensure we have an id and if not, create a sensible one
      if not newref.id then
        newref.id = ""
        for _,x in ipairs(newref.author) do
          newref.id = newref.id .. string.match(pandoc.utils.stringify(x), "%a+")
        end
        if newref.issued then
          newref.id = newref.id .. string.match(newref.issued, "%d+")
        end
        if ids[newref.id] then -- add disambiguator if needed
          newref.id = newref.id .. "-" .. #ids
        end
      end
      table.insert(ids, newref.id)
      table.insert(refs, newref)
      thisref = {}
    elseif key then
      table.insert(thisref, {key, val})
    end
  end
  return pandoc.Pandoc({}, pandoc.Meta { references = refs } )
end
```

Example of use:

```
% pandoc -f ris.lua -t bibtex
TY  - JOUR
AU  - Shannon, Claude E.
PY  - 1948
DA  - July
TI  - A Mathematical Theory of Communication
T2  - Bell System Technical Journal
SP  - 379
EP  - 423
VL  - 27
ER  - 
TY  - JOUR
T1  - On computable numbers, with an application to the Entscheidungsproblem
A1  - Turing, Alan Mathison
JO  - Proc. of London Mathematical Society
VL  - 47
IS  - 1
SP  - 230
EP  - 265
Y1  - 1937
ER  - 
^D
@article{Shannon1948,
  author = {Shannon, Claude E.},
  title = {A {Mathematical} {Theory} of {Communication}},
  journal = {Bell System Technical Journal},
  volume = {27},
  pages = {379-423},
  year = {1948}
}
@article{Turing1937,
  author = {Turing, Alan Mathison},
  title = {On Computable Numbers, with an Application to the
    {Entscheidungsproblem}},
  journal = {Proc. of London Mathematical Society},
  volume = {47},
  number = {1},
  pages = {230-265},
  year = {1937}
}
```

# Example: a wiki Creole reader

This is a parser for [Creole common wiki markup].
It uses an [lpeg] grammar.  Fun fact: this custom reader is faster than
pandoc's built-in creole reader! This shows that high-performance
readers can be designed in this way.

[Creole common wiki markup]: http://www.wikicreole.org/wiki/CheatSheet


```lua
-- A sample custom reader for Creole 1.0 (common wiki markup)
-- http://www.wikicreole.org/wiki/CheatSheet

-- For better performance we put these functions in local variables:
local P, S, R, Cf, Cc, Ct, V, Cs, Cg, Cb, B, C, Cmt =
  lpeg.P, lpeg.S, lpeg.R, lpeg.Cf, lpeg.Cc, lpeg.Ct, lpeg.V,
  lpeg.Cs, lpeg.Cg, lpeg.Cb, lpeg.B, lpeg.C, lpeg.Cmt

local whitespacechar = S(" \t\r\n")
local specialchar = S("/*~[]\\{}|")
local wordchar = (1 - (whitespacechar + specialchar))
local spacechar = S(" \t")
local newline = P"\r"^-1 * P"\n"
local blankline = spacechar^0 * newline
local endline = newline * #-blankline
local endequals = spacechar^0 * P"="^0 * spacechar^0 * newline
local cellsep = spacechar^0 * P"|"

local function trim(s)
   return (s:gsub("^%s*(.-)%s*$", "%1"))
end

local function ListItem(lev, ch)
  local start
  if ch == nil then
    start = S"*#"
  else
    start = P(ch)
  end
  local subitem = function(c)
    if lev < 6 then
      return ListItem(lev + 1, c)
    else
      return (1 - 1) -- fails
    end
  end
  local parser = spacechar^0
               * start^lev
               * #(- start)
               * spacechar^0
               * Ct((V"Inline" - (newline * spacechar^0 * S"*#"))^0)
               * newline
               * (Ct(subitem("*")^1) / pandoc.BulletList
                  +
                  Ct(subitem("#")^1) / pandoc.OrderedList
                  +
                  Cc(nil))
               / function (ils, sublist)
                   return { pandoc.Plain(ils), sublist }
                 end
  return parser
end

-- Grammar
G = P{ "Doc",
  Doc = Ct(V"Block"^0)
      / pandoc.Pandoc ;
  Block = blankline^0
        * ( V"Header"
          + V"HorizontalRule"
          + V"CodeBlock"
          + V"List"
          + V"Table"
          + V"Para") ;
  Para = Ct(V"Inline"^1)
       * newline
       / pandoc.Para ;
  HorizontalRule = spacechar^0
                 * P"----"
                 * spacechar^0
                 * newline
                 / pandoc.HorizontalRule;
  Header = (P("=")^1 / string.len)
         * spacechar^1
         * Ct((V"Inline" - endequals)^1)
         * endequals
         / pandoc.Header;
  CodeBlock = P"{{{"
            * blankline
            * C((1 - (newline * P"}}}"))^0)
            * newline
            * P"}}}"
            / pandoc.CodeBlock;
  Placeholder = P"<<<"
              * C(P(1) - P">>>")^0
              * P">>>"
              / function() return pandoc.Div({}) end;
  List = V"BulletList"
       + V"OrderedList" ;
  BulletList = Ct(ListItem(1,'*')^1)
             / pandoc.BulletList ;
  OrderedList = Ct(ListItem(1,'#')^1)
             / pandoc.OrderedList ;
  Table = (V"TableHeader" + Cc{})
        * Ct(V"TableRow"^1)
        / function(headrow, bodyrows)
            local numcolumns = #(bodyrows[1])
            local aligns = {}
            local widths = {}
            for i = 1,numcolumns do
              aligns[i] = pandoc.AlignDefault
              widths[i] = 0
            end
            return pandoc.utils.from_simple_table(
              pandoc.SimpleTable({}, aligns, widths, headrow, bodyrows))
          end ;
  TableHeader = Ct(V"HeaderCell"^1)
              * cellsep^-1
              * spacechar^0
              * newline ;
  TableRow   = Ct(V"BodyCell"^1)
             * cellsep^-1
             * spacechar^0
             * newline ;
  HeaderCell = cellsep
             * P"="
             * spacechar^0
             * Ct((V"Inline" - (newline + cellsep))^0)
             / function(ils) return { pandoc.Plain(ils) } end ;
  BodyCell   = cellsep
             * spacechar^0
             * Ct((V"Inline" - (newline + cellsep))^0)
             / function(ils) return { pandoc.Plain(ils) } end ;
  Inline = V"Emph"
         + V"Strong"
         + V"LineBreak"
         + V"Link"
         + V"URL"
         + V"Image"
         + V"Str"
         + V"Space"
         + V"SoftBreak"
         + V"Escaped"
         + V"Placeholder"
         + V"Code"
         + V"Special" ;
  Str = wordchar^1
      / pandoc.Str;
  Escaped = P"~"
          * C(P(1))
          / pandoc.Str ;
  Special = specialchar
          / pandoc.Str;
  Space = spacechar^1
        / pandoc.Space ;
  SoftBreak = endline
            * # -(V"HorizontalRule" + V"CodeBlock")
            / pandoc.SoftBreak ;
  LineBreak = P"\\\\"
            / pandoc.LineBreak ;
  Code = P"{{{"
       * C((1 - P"}}}")^0)
       * P"}}}"
       / trim / pandoc.Code ;
  Link = P"[["
       * C((1 - (P"]]" + P"|"))^0)
       * (P"|" * Ct((V"Inline" - P"]]")^1))^-1 * P"]]"
       / function(url, desc)
           local txt = desc or {pandoc.Str(url)}
           return pandoc.Link(txt, url)
         end ;
  Image = P"{{"
        * #-P"{"
        * C((1 - (S"}"))^0)
        * (P"|" * Ct((V"Inline" - P"}}")^1))^-1
        * P"}}"
        / function(url, desc)
            local txt = desc or ""
            return pandoc.Image(txt, url)
          end ;
  URL = P"http"
      * P"s"^-1
      * P":"
      * (1 - (whitespacechar + (S",.?!:;\"'" * #whitespacechar)))^1
      / function(url)
          return pandoc.Link(pandoc.Str(url), url)
        end ;
  Emph = P"//"
       * Ct((V"Inline" - P"//")^1)
       * P"//"
       / pandoc.Emph ;
  Strong = P"**"
         * Ct((V"Inline" -P"**")^1)
         * P"**"
         / pandoc.Strong ;
}

function Reader(input, reader_options)
  return lpeg.match(G, input)
end
```

Example of use:

```
% pandoc -f creole.lua -t markdown
== Wiki Creole

You can make things **bold** or //italic// or **//both//** or //**both**//.

Character formatting extends across line breaks: **bold,
this is still bold. This line deliberately does not end in star-star.

Not bold. Character formatting does not cross paragraph boundaries.

You can use [[internal links]] or [[http://www.wikicreole.org|external links]],
give the link a [[internal links|different]] name.
^D
## Wiki Creole

You can make things **bold** or *italic* or ***both*** or ***both***.

Character formatting extends across line breaks: \*\*bold, this is still
bold. This line deliberately does not end in star-star.

Not bold. Character formatting does not cross paragraph boundaries.

You can use [internal links](internal links) or [external
links](http://www.wikicreole.org), give the link a
[different](internal links) name.
```

