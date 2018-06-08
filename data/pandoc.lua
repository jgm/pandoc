--[[
pandoc.lua

Copyright © 2017–2018 Albert Krewinkel

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
]]

---
-- Lua functions for pandoc scripts.
--
-- @author Albert Krewinkel
-- @copyright © 2017–2018 Albert Krewinkel
-- @license MIT
local M = {}

local List = require 'pandoc.List'

------------------------------------------------------------------------
-- Accessor objects
--
-- Create metatables which allow to access numerical indices via accessor
-- methods.
-- @section
-- @local

--- Create a new indexing function.
-- @param template function template
-- @param indices list of indices, starting with the most deeply nested
-- @return newly created function
-- @local
function make_indexing_function(template, indices)
  local loadstring = loadstring or load
  local bracketed = {}
  for i = 1, #indices do
    bracketed[i] = string.format('[%d]', indices[#indices - i + 1])
  end
  local fnstr = string.format('return ' .. template, table.concat(bracketed))
  return assert(loadstring(fnstr))()
end

--- Create accessor functions using a function template.
-- @param fn_template function template in which '%s' is replacd with indices
-- @param accessors list of accessors
-- @return mapping from accessor names to accessor functions
-- @local
local function create_accessor_functions (fn_template, accessors)
  local res = {}
  function add_accessors(acc, ...)
    if type(acc) == 'string' then
      res[acc] = make_indexing_function(fn_template, {...})
    elseif type(acc) == 'table' and #acc == 0 and next(acc) then
      local name, substructure = next(acc)
      res[name] = make_indexing_function(fn_template, {...})
      add_accessors(substructure, ...)
    else
      for i = 1, #(acc or {}) do
        add_accessors(acc[i], i, ...)
      end
    end
  end
  add_accessors(accessors)
  return res
end

--- Create a new table which allows to access numerical indices via accessor
-- functions.
-- @local
local function create_accessor_behavior (tag, accessors)
  local behavior = {tag = tag}
  behavior.getters = create_accessor_functions(
    'function (x) return x.c%s end',
    accessors
  )
  behavior.setters = create_accessor_functions(
    'function (x, v) x.c%s = v end',
    accessors
  )
  behavior.__index = function(t, k)
    if getmetatable(t).getters[k] then
      return getmetatable(t).getters[k](t)
    elseif k == "t" then
      return getmetatable(t)["tag"]
    else
      return getmetatable(t)[k]
    end
  end
  behavior.__newindex = function(t, k, v)
    if getmetatable(t).setters[k] then
      getmetatable(t).setters[k](t, v)
    else
      rawset(t, k, v)
    end
  end
  return behavior
end


------------------------------------------------------------------------
-- The base class for types
-- @type Type
-- @local
local Type = {}
Type.name = 'Type'
Type.__index = Type
Type.behavior = {
  __type = Type,
  new = function (obj)
    obj = obj or {}
    setmetatable(obj, self)
    return obj
  end
}
Type.behavior.__index = Type.behavior

--- Set a new behavior for the type, inheriting that of the parent type if none
--- is specified explicitly
-- @param behavior the behavior object for this type.
-- @local
function Type:set_behavior (behavior)
  behavior = behavior or {}
  behavior.__index = rawget(behavior, '__index') or behavior
  behavior.__type = self
  if not getmetatable(behavior) and getmetatable(self) then
    setmetatable(behavior, getmetatable(self).behavior)
  end
  self.behavior = behavior
end

--- Create a new subtype, using the given table as base.
-- @param name name of the new type
-- @param[opt] behavior behavioral object for the new type.
-- @return a new type
-- @local
function Type:make_subtype(name, behavior)
  local newtype = setmetatable({}, self)
  newtype.name = name
  newtype.__index = newtype
  newtype:set_behavior(behavior)
  return newtype
end


------------------------------------------------------------------------
-- The base class for pandoc's AST elements.
-- @type AstElement
-- @local
local AstElement = Type:make_subtype 'AstElement'
AstElement.__call = function(t, ...)
  local success, ret = pcall(t.new, t, ...)
  if success then
    return setmetatable(ret, t.behavior)
  else
    error(string.format('Constructor for %s failed: %s\n', t.name, ret))
  end
end

--- Make a new subtype which constructs a new value when called.
-- @local
function AstElement:make_subtype(...)
  local newtype = Type.make_subtype(self, ...)
  newtype.__call = self.__call
  return newtype
end

--- Create a new constructor
-- @local
-- @param tag Tag used to identify the constructor
-- @param fn Function to be called when constructing a new element
-- @param accessors names to use as accessors for numerical fields
-- @return function that constructs a new element
function AstElement:create_constructor(tag, fn, accessors)
  local constr = self:make_subtype(tag, create_accessor_behavior(tag, accessors))
  function constr:new(...)
    return setmetatable(fn(...), self.behavior)
  end
  self.constructor = self.constructor or {}
  self.constructor[tag] = constr
  return constr
end

--- Convert AstElement input into a list if necessary.
-- @local
local function ensureList (x)
  if x.tag then
    -- Lists are not tagged, but all elements are
    return List:new{x}
  else
    return List:new(x)
  end
end

--- Ensure a given object is an Inline element, or convert it into one.
-- @local
local function ensureInlineList (x)
  if type(x) == 'string' then
    return List:new{M.Str(x)}
  else
    return ensureList(x)
  end
end

------------------------------------------------------------------------
--- Pandoc Document
-- @section document

--- A complete pandoc document
-- @function Pandoc
-- @tparam      {Block,...} blocks      document content
-- @tparam[opt] Meta        meta        document meta data
M.Pandoc = AstElement:make_subtype'Pandoc'
function M.Pandoc:new (blocks, meta)
  return {
    blocks = ensureList(blocks),
    meta = meta or {},
  }
end

-- DEPRECATED synonym:
M.Doc = M.Pandoc

------------------------------------------------------------------------
-- Meta
-- @section Meta

--- Create a new Meta object. It sets the metatable of the given table to
--- `Meta`.
-- @function Meta
-- @tparam meta table table containing document meta information
M.Meta = AstElement:make_subtype'Meta'
function M.Meta:new (meta) return meta end


------------------------------------------------------------------------
-- MetaValue
-- @section MetaValue
M.MetaValue = AstElement:make_subtype('MetaValue')

--- Meta blocks
-- @function MetaBlocks
-- @tparam {Block,...} blocks blocks
M.MetaBlocks = M.MetaValue:create_constructor(
  'MetaBlocks',
  function (content) return ensureList(content) end
)

--- Meta inlines
-- @function MetaInlines
-- @tparam {Inline,...} inlines inlines
M.MetaInlines = M.MetaValue:create_constructor(
  'MetaInlines',
  function (content) return ensureInlineList(content) end
)

--- Meta list
-- @function MetaList
-- @tparam {MetaValue,...} meta_values list of meta values
M.MetaList = M.MetaValue:create_constructor(
  'MetaList',
  function (content) return ensureList(content) end
)

--- Meta map
-- @function MetaMap
-- @tparam table key_value_map a string-indexed map of meta values
M.MetaMap = M.MetaValue:create_constructor(
  "MetaMap",
  function (mm) return mm end
)

--- Creates string to be used in meta data.
-- Does nothing, lua strings are meta strings.
-- @function MetaString
-- @tparam string str string value
function M.MetaString(str)
  return str
end

--- Creates boolean to be used in meta data.
-- Does nothing, lua booleans are meta booleans.
-- @function MetaBool
-- @tparam boolean bool boolean value
function M.MetaBool(bool)
  return bool
end

------------------------------------------------------------------------
-- Blocks
-- @section Block

--- Block elements
M.Block = AstElement:make_subtype'Block'

--- Creates a block quote element
-- @function BlockQuote
-- @tparam      {Block,...} content     block content
-- @treturn     Block                   block quote element
M.BlockQuote = M.Block:create_constructor(
  "BlockQuote",
  function(content) return {c = ensureList(content)} end,
  "content"
)

--- Creates a bullet (i.e. unordered) list.
-- @function BulletList
-- @tparam      {{Block,...},...} content     list of items
-- @treturn     Block                         bullet list element
M.BulletList = M.Block:create_constructor(
  "BulletList",
  function(content) return {c = ensureList(content)} end,
  "content"
)

--- Creates a code block element
-- @function CodeBlock
-- @tparam      string      text        code string
-- @tparam[opt] Attr        attr element attributes
-- @treturn     Block                   code block element
M.CodeBlock = M.Block:create_constructor(
  "CodeBlock",
  function(text, attr) return {c = {attr or M.Attr(), text}} end,
  {{attr = {"identifier", "classes", "attributes"}}, "text"}
)

--- Creates a definition list, containing terms and their explanation.
-- @function DefinitionList
-- @tparam      {{{Inline,...},{Block,...}},...} content     list of items
-- @treturn     Block                  definition list element
M.DefinitionList = M.Block:create_constructor(
  "DefinitionList",
  function(content) return {c = ensureList(content)} end,
  "content"
)

--- Creates a div element
-- @function Div
-- @tparam      {Block,...} content     block content
-- @tparam[opt] Attr        attr  element attributes
-- @treturn     Block                   div element
M.Div = M.Block:create_constructor(
  "Div",
  function(content, attr)
    return {c = {attr or M.Attr(), ensureList(content)}}
  end,
  {{attr = {"identifier", "classes", "attributes"}}, "content"}
)

--- Creates a header element.
-- @function Header
-- @tparam      int          level       header level
-- @tparam      {Inline,...} content     inline content
-- @tparam[opt] Attr         attr element attributes
-- @treturn     Block                    header element
M.Header = M.Block:create_constructor(
  "Header",
  function(level, content, attr)
    return {c = {level, attr or M.Attr(), ensureInlineList(content)}}
  end,
  {"level", {attr = {"identifier", "classes", "attributes"}}, "content"}
)

--- Creates a horizontal rule.
-- @function HorizontalRule
-- @treturn     Block                   horizontal rule
M.HorizontalRule = M.Block:create_constructor(
  "HorizontalRule",
  function() return {} end
)

--- Creates a line block element.
-- @function LineBlock
-- @tparam      {{Inline,...},...} content    inline content
-- @treturn     Block                   line block element
M.LineBlock = M.Block:create_constructor(
  "LineBlock",
  function(content) return {c = ensureList(content)} end,
  "content"
)

--- Creates a null element.
-- @function Null
-- @treturn     Block                   null element
M.Null = M.Block:create_constructor(
  "Null",
  function() return {} end
)

--- Creates an ordered list.
-- @function OrderedList
-- @tparam      {{Block,...},...} items list items
-- @param[opt]  listAttributes list parameters
-- @treturn     Block  ordered list element
M.OrderedList = M.Block:create_constructor(
  "OrderedList",
  function(items, listAttributes)
    listAttributes = listAttributes or {1, M.DefaultStyle, M.DefaultDelim}
    return {c = {listAttributes, ensureList(items)}}
  end,
  {{listAttributes = {"start", "style", "delimiter"}}, "content"}
)

--- Creates a para element.
-- @function Para
-- @tparam      {Inline,...} content    inline content
-- @treturn     Block                   paragraph element
M.Para = M.Block:create_constructor(
  "Para",
  function(content) return {c = ensureInlineList(content)} end,
  "content"
)

--- Creates a plain element.
-- @function Plain
-- @tparam      {Inline,...} content    inline content
-- @treturn     Block                   plain element
M.Plain = M.Block:create_constructor(
  "Plain",
  function(content) return {c = ensureInlineList(content)} end,
  "content"
)

--- Creates a raw content block of the specified format.
-- @function RawBlock
-- @tparam      string      format      format of content
-- @tparam      string      text        string content
-- @treturn     Block                   raw block element
M.RawBlock = M.Block:create_constructor(
  "RawBlock",
  function(format, text) return {c = {format, text}} end,
  {"format", "text"}
)

--- Creates a table element.
-- @function Table
-- @tparam      {Inline,...} caption    table caption
-- @tparam      {AlignDefault|AlignLeft|AlignRight|AlignCenter,...} aligns alignments
-- @tparam      {int,...}    widths     column widths
-- @tparam      {Block,...}  headers    header row
-- @tparam      {{Block,...}} rows      table rows
-- @treturn     Block                   table element
M.Table = M.Block:create_constructor(
  "Table",
  function(caption, aligns, widths, headers, rows)
    return {
      c = {
        ensureInlineList(caption),
        List:new(aligns),
        List:new(widths),
        List:new(headers),
        List:new(rows)
      }
    }
  end,
  {"caption", "aligns", "widths", "headers", "rows"}
)


------------------------------------------------------------------------
-- Inline
-- @section Inline

--- Inline element class
M.Inline = AstElement:make_subtype'Inline'

--- Creates a Cite inline element
-- @function Cite
-- @tparam {Inline,...}   content       List of inlines
-- @tparam {Citation,...} citations     List of citations
-- @treturn Inline citations element
M.Cite = M.Inline:create_constructor(
  "Cite",
  function(content, citations)
    return {c = {ensureList(citations), ensureInlineList(content)}}
  end,
  {"citations", "content"}
)

--- Creates a Code inline element
-- @function Code
-- @tparam      string      text        brief image description
-- @tparam[opt] Attr        attr  additional attributes
-- @treturn Inline code element
M.Code = M.Inline:create_constructor(
  "Code",
  function(text, attr) return {c = {attr or M.Attr(), text}} end,
  {{attr = {"identifier", "classes", "attributes"}}, "text"}
)

--- Creates an inline element representing emphasised text.
-- @function Emph
-- @tparam      {Inline,..} content     inline content
-- @treturn Inline emphasis element
M.Emph = M.Inline:create_constructor(
  "Emph",
  function(content) return {c = ensureInlineList(content)} end,
  "content"
)

--- Creates a Image inline element
-- @function Image
-- @tparam      {Inline,..} caption     text used to describe the image
-- @tparam      string      src         path to the image file
-- @tparam[opt] string      title       brief image description
-- @tparam[opt] Attr        attr additional attributes
-- @treturn Inline image element
M.Image = M.Inline:create_constructor(
  "Image",
  function(caption, src, title, attr)
    title = title or ""
    attr = attr or M.Attr()
    return {c = {attr, ensureInlineList(caption), {src, title}}}
  end,
  {{attr = {"identifier", "classes", "attributes"}}, "caption", {"src", "title"}}
)

--- Create a LineBreak inline element
-- @function LineBreak
-- @treturn Inline linebreak element
M.LineBreak = M.Inline:create_constructor(
  "LineBreak",
  function() return {} end
)

--- Creates a link inline element, usually a hyperlink.
-- @function Link
-- @tparam      {Inline,..} content     text for this link
-- @tparam      string      target      the link target
-- @tparam[opt] string      title       brief link description
-- @tparam[opt] Attr        attr additional attributes
-- @treturn Inline image element
M.Link = M.Inline:create_constructor(
  "Link",
  function(content, target, title, attr)
    title = title or ""
    attr = attr or M.Attr()
    return {c = {attr, ensureInlineList(content), {target, title}}}
  end,
  {{attr = {"identifier", "classes", "attributes"}}, "content", {"target", "title"}}
)

--- Creates a Math element, either inline or displayed.
-- @function Math
-- @tparam      "InlineMath"|"DisplayMath" mathtype rendering specifier
-- @tparam      string      text        Math content
-- @treturn     Inline                  Math element
M.Math = M.Inline:create_constructor(
  "Math",
  function(mathtype, text)
    return {c = {mathtype, text}}
  end,
  {"mathtype", "text"}
)
--- Creates a DisplayMath element (DEPRECATED).
-- @function DisplayMath
-- @tparam      string      text        Math content
-- @treturn     Inline                  Math element
M.DisplayMath = M.Inline:create_constructor(
  "DisplayMath",
  function(text) return M.Math("DisplayMath", text) end,
  {"mathtype", "text"}
)
--- Creates an InlineMath inline element (DEPRECATED).
-- @function InlineMath
-- @tparam      string      text        Math content
-- @treturn     Inline                  Math element
M.InlineMath = M.Inline:create_constructor(
  "InlineMath",
  function(text) return M.Math("InlineMath", text) end,
  {"mathtype", "text"}
)

--- Creates a Note inline element
-- @function Note
-- @tparam      {Block,...} content     footnote block content
M.Note = M.Inline:create_constructor(
  "Note",
  function(content) return {c = ensureList(content)} end,
  "content"
)

--- Creates a Quoted inline element given the quote type and quoted content.
-- @function Quoted
-- @tparam      "DoubleQuote"|"SingleQuote" quotetype type of quotes to be used
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  quoted element
M.Quoted = M.Inline:create_constructor(
  "Quoted",
  function(quotetype, content) return {c = {quotetype, ensureInlineList(content)}} end,
  {"quotetype", "content"}
)
--- Creates a single-quoted inline element (DEPRECATED).
-- @function SingleQuoted
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  quoted element
-- @see Quoted
M.SingleQuoted = M.Inline:create_constructor(
  "SingleQuoted",
  function(content) return M.Quoted(M.SingleQuote, content) end,
  {"quotetype", "content"}
)
--- Creates a single-quoted inline element (DEPRECATED).
-- @function DoubleQuoted
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  quoted element
-- @see Quoted
M.DoubleQuoted = M.Inline:create_constructor(
  "DoubleQuoted",
  function(content) return M.Quoted("DoubleQuote", content) end,
  {"quotetype", "content"}
)

--- Creates a RawInline inline element
-- @function RawInline
-- @tparam      string      format      format of the contents
-- @tparam      string      text        string content
-- @treturn     Inline                  raw inline element
M.RawInline = M.Inline:create_constructor(
  "RawInline",
  function(format, text) return {c = {format, text}} end,
  {"format", "text"}
)

--- Creates text rendered in small caps
-- @function SmallCaps
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  smallcaps element
M.SmallCaps = M.Inline:create_constructor(
  "SmallCaps",
  function(content) return {c = ensureInlineList(content)} end,
  "content"
)

--- Creates a SoftBreak inline element.
-- @function SoftBreak
-- @treturn     Inline                  softbreak element
M.SoftBreak = M.Inline:create_constructor(
  "SoftBreak",
  function() return {} end
)

--- Create a Space inline element
-- @function Space
-- @treturn Inline space element
M.Space = M.Inline:create_constructor(
  "Space",
  function() return {} end
)

--- Creates a Span inline element
-- @function Span
-- @tparam      {Inline,..} content     inline content
-- @tparam[opt] Attr        attr  additional attributes
-- @treturn Inline span element
M.Span = M.Inline:create_constructor(
  "Span",
  function(content, attr)
    return {c = {attr or M.Attr(), ensureInlineList(content)}}
  end,
  {{attr = {"identifier", "classes", "attributes"}}, "content"}
)

--- Creates a Str inline element
-- @function Str
-- @tparam      string      text        content
-- @treturn     Inline                  string element
M.Str = M.Inline:create_constructor(
  "Str",
  function(text) return {c = text} end,
  "text"
)

--- Creates text which is striked out.
-- @function Strikeout
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  strikeout element
M.Strikeout = M.Inline:create_constructor(
  "Strikeout",
  function(content) return {c = ensureInlineList(content)} end,
  "content"
)

--- Creates a Strong element, whose text is usually displayed in a bold font.
-- @function Strong
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  strong element
M.Strong = M.Inline:create_constructor(
  "Strong",
  function(content) return {c = ensureInlineList(content)} end,
  "content"
)

--- Creates a Subscript inline element
-- @function Subscript
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  subscript element
M.Subscript = M.Inline:create_constructor(
  "Subscript",
  function(content) return {c = ensureInlineList(content)} end,
  "content"
)

--- Creates a Superscript inline element
-- @function Superscript
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  strong element
M.Superscript = M.Inline:create_constructor(
  "Superscript",
  function(content) return {c = ensureInlineList(content)} end,
  "content"
)


------------------------------------------------------------------------
-- Element components
-- @section components

--- Check if the first element of a pair matches the given value.
-- @param x  key value to be checked
-- @return function returning true iff first element of its argument matches x
-- @local
local function assoc_key_equals (x)
  return function (y) return y[1] == x end
end

--- Lookup a value in an associative list
-- @function lookup
-- @local
-- @tparam {{key, value},...} alist associative list
-- @param key key for which the associated value is to be looked up
local function lookup(alist, key)
  return (List.find_if(alist, assoc_key_equals(key)) or {})[2]
end

--- Return an iterator which returns key-value pairs of an associative list.
-- @function apairs
-- @local
-- @tparam {{key, value},...} alist associative list
local apairs = function (alist)
  local i = 1
  local cur
  function nxt ()
    cur = rawget(alist, i)
    if cur then
      i = i + 1
      return cur[1], cur[2]
    end
    return nil
  end
  return nxt, nil, nil
end

--- AttributeList, a metatable to allow table-like access to attribute lists
-- represented by associative lists.
-- @local
local AttributeList = {
  __index = function (t, k)
    if type(k) == "number" then
      return rawget(t, k)
    else
      return lookup(t, k)
    end
  end,

  __newindex = function (t, k, v)
    local cur, idx = List.find_if(t, assoc_key_equals(k))
    if v == nil then
      table.remove(t, idx)
    elseif cur then
      cur[2] = v
    elseif type(k) == "number" then
      rawset(t, k, v)
    else
      rawset(t, #t + 1, {k, v})
    end
  end,

  __pairs = apairs
}

--- Convert a table to an associative list. The order of key-value pairs in the
-- alist is undefined. The table should either contain no numeric keys or
-- already be an associative list.
-- @local
-- @tparam table tbl associative list or table without numeric keys.
-- @treturn table associative list
local to_alist = function (tbl)
  if #tbl ~= 0 or next(tbl) == nil then
    -- probably already an alist
    return tbl
  end
  local alist = {}
  local i = 1
  for k, v in pairs(tbl) do
    alist[i] = {k, v}
    i = i + 1
  end
  return alist
end

-- Attr

--- Create a new set of attributes (Attr).
-- @function Attr
-- @tparam[opt] string       identifier element identifier
-- @tparam[opt] {string,...} classes    element classes
-- @tparam[opt] table        attributes table containing string keys and values
-- @return element attributes
M.Attr = AstElement:make_subtype'Attr'
function M.Attr:new (identifier, classes, attributes)
  identifier = identifier or ''
  classes = ensureList(classes or {})
  attributes = setmetatable(to_alist(attributes or {}), AttributeList)
  return {identifier, classes, attributes}
end
M.Attr.behavior._field_names = {identifier = 1, classes = 2, attributes = 3}
M.Attr.behavior.__index = function(t, k)
  return rawget(t, getmetatable(t)._field_names[k]) or
    getmetatable(t)[k]
end
M.Attr.behavior.__newindex = function(t, k, v)
  if getmetatable(t)._field_names[k] then
    rawset(t, getmetatable(t)._field_names[k], v)
  else
    rawset(t, k, v)
  end
end

-- Citation
M.Citation = AstElement:make_subtype'Citation'

--- Creates a single citation.
-- @function Citation
-- @tparam      string       id       citation identifier (like a bibtex key)
-- @tparam      AuthorInText|SuppressAuthor|NormalCitation mode citation mode
-- @tparam[opt] {Inline,...} prefix   citation prefix
-- @tparam[opt] {Inline,...} suffix   citation suffix
-- @tparam[opt] int          note_num note number
-- @tparam[opt] int          hash  hash number
function M.Citation:new (id, mode, prefix, suffix, note_num, hash)
  return {
    id = id,
    mode = mode,
    prefix = ensureList(prefix or {}),
    suffix = ensureList(suffix or {}),
    note_num = note_num or 0,
    hash = hash or 0,
  }
end


------------------------------------------------------------------------
-- Constants
-- @section constants

--- Author name is mentioned in the text.
-- @see Citation
-- @see Cite
M.AuthorInText = "AuthorInText"

--- Author name is suppressed.
-- @see Citation
-- @see Cite
M.SuppressAuthor = "SuppressAuthor"

--- Default citation style is used.
-- @see Citation
-- @see Cite
M.NormalCitation = "NormalCitation"

--- Table cells aligned left.
-- @see Table
M.AlignLeft = "AlignLeft"

--- Table cells right-aligned.
-- @see Table
M.AlignRight = "AlignRight"

--- Table cell content is centered.
-- @see Table
M.AlignCenter = "AlignCenter"

--- Table cells are alignment is unaltered.
-- @see Table
M.AlignDefault = "AlignDefault"

--- Default list number delimiters are used.
-- @see OrderedList
M.DefaultDelim = "DefaultDelim"

--- List numbers are delimited by a period.
-- @see OrderedList
M.Period = "Period"

--- List numbers are delimited by a single parenthesis.
-- @see OrderedList
M.OneParen = "OneParen"

--- List numbers are delimited by a double parentheses.
-- @see OrderedList
M.TwoParens = "TwoParens"

--- List are numbered in the default style
-- @see OrderedList
M.DefaultStyle = "DefaultStyle"

--- List items are numbered as examples.
-- @see OrderedList
M.Example = "Example"

--- List are numbered using decimal integers.
-- @see OrderedList
M.Decimal = "Decimal"

--- List are numbered using lower-case roman numerals.
-- @see OrderedList
M.LowerRoman = "LowerRoman"

--- List are numbered using upper-case roman numerals
-- @see OrderedList
M.UpperRoman = "UpperRoman"

--- List are numbered using lower-case alphabetic characters.
-- @see OrderedList
M.LowerAlpha = "LowerAlpha"

--- List are numbered using upper-case alphabetic characters.
-- @see OrderedList
M.UpperAlpha = "UpperAlpha"

------------------------------------------------------------------------
-- Functions which have moved to different modules
local utils = require 'pandoc.utils'
M.sha1 = utils.sha1

return M
