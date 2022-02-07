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

A custom reader is a Lua file that defines a function
called `Reader`, which takes two arguments:

- the raw input to be parsed, as a list of sources
- optionally, a table of reader options, e.g.
  `{ columns = 62, standalone = true }`.

The `Reader` function should return a `Pandoc` AST.
This can be created using functions in the [`pandoc` module],
which is automatically in scope.  (Indeed, all of the utility
functions that are available for [Lua filters] are available
in custom readers, too.)

Each source item corresponds to a file or stream passed to pandoc
containing its text and name. E.g., if a single file `input.txt`
is passed to pandoc, then the list of sources will contain just a
single element `s`, where `s.name == 'input.txt'` and `s.text`
contains the file contents as a string.

The sources list, as well as each of its elements, can be
converted to a string via the Lua standard library function
`tostring`.

[Lua filters]: https://pandoc.org/lua-filters.html
[`pandoc` module]: https://pandoc.org/lua-filters.html#module-pandoc

A minimal example would be

```lua
function Reader(input)
  return pandoc.Pandoc({ pandoc.CodeBlock(tostring(input)) })
end
```

This just returns a document containing a big code block with all
of the input. Or, to create a separate code block for each input
file, one might write

``` lua
function Reader(input)
  return pandoc.Pandoc(input:map(
    function (s) return pandoc.CodeBlock(s.text) end))
end
```

In a nontrivial reader, you'll want to parse the input.
You can do this using standard Lua library functions
(for example, the [patterns] library), or with the powerful
and fast [lpeg] parsing library, which is automatically in scope.
You can also use external Lua libraries (for example,
an XML parser).

A previous pandoc version passed a raw string instead of a list
of sources to the Reader function. Reader functions that rely on
this are obsolete, but still supported: Pandoc analyzes any
script error, detecting when code assumed the old behavior. The
code is rerun with raw string input in this case, thereby
ensuring backwards compatibility.

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
  return lpeg.match(G, tostring(input))
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
  return lpeg.match(G, tostring(input))
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

# Example: parsing JSON from an API

This custom reader consumes the JSON output of
<https://www.reddit.com/r/haskell.json> and produces
a document containing the current top articles on the
Haskell subreddit.

It assumes that the `luajson` library is available.  (It can be
installed using `luarocks install luajson`---but be sure you are
installing it for Lua 5.3, which is the version packaged with
pandoc.)


```lua
-- consumes the output of https://www.reddit.com/r/haskell.json

local json = require'json'  -- luajson must be available

local function read_inlines(raw)
  local doc = pandoc.read(raw, "commonmark")
  return pandoc.utils.blocks_to_inlines(doc.blocks)
end

local function read_blocks(raw)
  local doc = pandoc.read(raw, "commonmark")
  return doc.blocks
end

function Reader(input)

  local parsed = json.decode(tostring(input))
  local blocks = {}

  for _,entry in ipairs(parsed.data.children) do
    local d = entry.data
    table.insert(blocks, pandoc.Header(2,
                  pandoc.Link(read_inlines(d.title), d.url)))
    for _,block in ipairs(read_blocks(d.selftext)) do
      table.insert(blocks, block)
    end
  end

  return pandoc.Pandoc(blocks)

end
```

Similar code can be used to consume JSON output from other APIs.

Note that the content of the text fields is markdown, so we
convert it using `pandoc.read()`.


# Example: syntax-highlighted code files

This is a reader that puts the content of each input file into a
code block, sets the file's extension as the block's class to
enable code highlighting, and places the filename as a header
above each code block.

``` lua
function to_code_block (source)
  local _, lang = pandoc.path.split_extension(source.name)
  return pandoc.Div{
    pandoc.Header(1, source.name == '' and '<stdin>' or source.name),
    pandoc.CodeBlock(source.text, {class=lang}),
  }
end

function Reader (input, opts)
  return pandoc.Pandoc(input:map(to_code_block))
end
```

# Example: extracting the content from web pages

This reader uses the command-line program `readable`
(install via `npm install -g readability-cli`)
to clean out parts of HTML input that have to do with
navigation, leaving only the content.

``` lua
-- Custom reader that extracts the content from HTML documents,
-- ignoring navigation and layout elements. This preprocesses input
-- through the 'readable' program (which can be installed using
-- 'npm install -g readability-cli') and then calls the HTML reader.
-- In addition, Divs that seem to have only a layout function are removed
-- to avoid clutter.

function make_readable(source)
  local result
  if not pcall(function ()
      local name = source.name
      if not name:match("http") then
        name = "file:///" .. name
      end
      result = pandoc.pipe("readable",
                 {"--keep-classes","--base",name},
                 source.text)
    end) then
      io.stderr:write("Error running 'readable': do you have it installed?\n")
      io.stderr:write("npm install -g readability-cli\n")
      os.exit(1)
  end
  return result
end

local boring_classes =
        { row = true,
          page = true,
          container = true
        }

local boring_attributes = { "role" }

local function is_boring_class(cl)
  return boring_classes[cl] or cl:match("col%-") or cl:match("pull%-")
end

local function handle_div(el)
  for i,class in ipairs(el.classes) do
    if is_boring_class(class) then
      el.classes[i] = nil
    end
  end
  for i,k in ipairs(boring_attributes) do
    el.attributes[k] = nil
  end
  if el.identifier:match("readability%-") then
    el.identifier = ""
  end
  if #el.classes == 0 and #el.attributes == 0 and #el.identifier == 0 then
    return el.content
  else
    return el
  end
end

function Reader(sources)
  local readable = ''
  for _,source in ipairs(sources) do
    readable = readable .. make_readable(source)
  end
  local doc = pandoc.read(readable, "html", PANDOC_READER_OPTIONS)
  -- Now remove Divs used only for layout
  return doc:walk{ Div = handle_div }
end
```

Example of use:

```
pandoc -f readable.lua -t markdown https://pandoc.org
```
and compare the output to
```
pandoc -f html -t markdown https://pandoc.org
```

