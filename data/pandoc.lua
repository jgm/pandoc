--[[
pandoc.lua

Copyright © 2017–2019 Albert Krewinkel

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
-- @copyright © 2017–2019 Albert Krewinkel
-- @license MIT
local M = {}

-- Re-export bundled modules
M.List = require 'pandoc.List'
M.mediabag = require 'pandoc.mediabag'
M.system = require 'pandoc.system'
M.types = require 'pandoc.types'
M.utils = require 'pandoc.utils'
M.text = require 'text'

-- Local names for modules which this module depends on.
local List = M.List
local utils = M.utils


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
function make_indexing_function(template, ...)
  local indices = {...}
  local loadstring = loadstring or load
  local bracketed = {}
  for i = 1, #indices do
    local idx = indices[#indices - i + 1]
    bracketed[i] = type(idx) == 'number'
      and string.format('[%d]', idx)
      or string.format('.%s', idx)
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
      res[acc] = make_indexing_function(fn_template, ...)
    elseif type(acc) == 'table' and #acc == 0 and next(acc) then
      -- Named substructure: the given names are accessed via the substructure,
      -- but the accessors are also added to the result table, enabling direct
      -- access from the parent element. Mainly used for `attr`.
      local name, substructure = next(acc)
      res[name] = make_indexing_function(fn_template, ...)
      for _, subname in ipairs(substructure) do
        res[subname] = make_indexing_function(fn_template, subname, ...)
      end
    else
      for i = 1, #(acc or {}) do
        add_accessors(acc[i], i, ...)
      end
    end
  end
  add_accessors(accessors)
  return res
end

--- Get list of top-level fields from field descriptor table.
-- E.g.: `top_level_fields{'foo', {bar='baz'}, {'qux', 'quux'}}`
-- gives {'foo, 'bar', 'qux', 'quux'}
-- @local
local function top_level_fields (fields)
  local result = List:new{}
  for _, v in ipairs(fields) do
    if type(v) == 'string' then
      table.insert(result, v)
    elseif type(v) == 'table' and #v == 0 and next(v) then
      table.insert(result, (next(v)))
    else
      result:extend(top_level_fields(v))
    end
  end
  return result
end

--- Creates a function which behaves like next, but respects field names.
-- @local
local function make_next_function (fields)
  local field_indices = {}
  for i, f in ipairs(fields) do
    field_indices[f] = i
  end

  return function (t, field)
    local raw_idx = field == nil and 0 or field_indices[field]
    local next_field = fields[raw_idx + 1]
    return next_field, t[next_field]
  end
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
  behavior.__eq = utils.equals
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
  behavior.__pairs = function (t)
    if accessors == nil then
      return next, t
    end
    local iterable_fields = type(accessors) == 'string'
      and {accessors}
      or top_level_fields(accessors)
    return make_next_function(iterable_fields), t
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

--- Ensure that the given object is a definition pair, convert if necessary.
-- @local
local function ensureDefinitionPairs (pair)
  local inlines = ensureInlineList(pair[1] or {})
  local blocks = ensureList(pair[2] or {}):map(ensureList)
  return {inlines, blocks}
end

--- Split a string into it's words, using whitespace as separators.
local function words (str)
  local ws = {}
  for w in str:gmatch("([^%s]+)") do ws[#ws + 1] = w end
  return ws
end

--- Try hard to turn the arguments into an Attr object.
local function ensureAttr(attr)
  if type(attr) == 'table' then
    if #attr > 0 then return M.Attr(table.unpack(attr)) end

    -- assume HTML-like key-value pairs
    local ident = attr.id or ''
    local classes = words(attr.class or '')
    local attributes = attr
    attributes.id = nil
    attributes.class = nil
    return M.Attr(ident, classes, attributes)
  elseif attr == nil then
    return M.Attr()
  elseif type(attr) == 'string' then
    -- treat argument as ID
    return M.Attr(attr)
  end
  -- print(arg, ...)
  error('Could not convert to Attr')
end

------------------------------------------------------------------------
--- Pandoc Document
-- @section document

--- A complete pandoc document
-- @function Pandoc
-- @tparam      {Block,...} blocks      document content
-- @tparam[opt] Meta        meta        document meta data
M.Pandoc = AstElement:make_subtype'Pandoc'
M.Pandoc.behavior.clone = M.types.clone.Pandoc
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
M.Meta.behavior.clone = M.types.clone.Meta
function M.Meta:new (meta) return meta end


------------------------------------------------------------------------
-- MetaValue
-- @section MetaValue
M.MetaValue = AstElement:make_subtype('MetaValue')
M.MetaValue.behavior.clone = M.types.clone.MetaValue

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
  function (content)
    if content.tag == 'MetaList' then
      return content
    end
    return ensureList(content)
  end
)
for k, v in pairs(List) do
  M.MetaList.behavior[k] = v
end

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
M.Block.behavior.clone = M.types.clone.Block

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
  function(content) return {c = ensureList(content):map(ensureList)} end,
  "content"
)

--- Creates a code block element
-- @function CodeBlock
-- @tparam      string      text        code string
-- @tparam[opt] Attr        attr element attributes
-- @treturn     Block                   code block element
M.CodeBlock = M.Block:create_constructor(
  "CodeBlock",
  function(text, attr) return {c = {ensureAttr(attr), text}} end,
  {{attr = {"identifier", "classes", "attributes"}}, "text"}
)

--- Creates a definition list, containing terms and their explanation.
-- @function DefinitionList
-- @tparam      {{{Inline,...},{{Block,...}}},...} content     list of items
-- @treturn     Block                  definition list element
M.DefinitionList = M.Block:create_constructor(
  "DefinitionList",
  function(content)
    return {c = ensureList(content):map(ensureDefinitionPairs)}
  end,
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
    return {c = {ensureAttr(attr), ensureList(content)}}
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
    return {c = {level, ensureAttr(attr), ensureInlineList(content)}}
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
  function(content) return {c = ensureList(content):map(ensureInlineList)} end,
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
    listAttributes = listAttributes or M.ListAttributes()
    return {c = {listAttributes, ensureList(items):map(ensureList)}}
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
-- @tparam      Caption      caption    table caption
-- @tparam      {ColSpec,...} colspecs  column alignments and widths
-- @tparam      TableHead    head       table head
-- @tparam      {TableBody,..} bodies   table bodies
-- @treturn     TableFoot    foot       table foot
-- @tparam[opt] Attr         attr       attributes
M.Table = M.Block:create_constructor(
  "Table",
  function(caption, colspecs, head, bodies, foot, attr)
    return {
      c = {
        ensureAttr(attr),
        caption,
        List:new(colspecs),
        head,
        List:new(bodies),
        foot
      }
    }
  end,
  {"attr", "caption", "colspecs", "head", "bodies", "foot"}
)


------------------------------------------------------------------------
-- Inline
-- @section Inline

--- Inline element class
M.Inline = AstElement:make_subtype'Inline'
M.Inline.behavior.clone = M.types.clone.Inline

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
-- @tparam      string      text  code string
-- @tparam[opt] Attr        attr  additional attributes
-- @treturn Inline code element
M.Code = M.Inline:create_constructor(
  "Code",
  function(text, attr) return {c = {ensureAttr(attr), text}} end,
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
    return {c = {ensureAttr(attr), ensureInlineList(caption), {src, title}}}
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
    attr = ensureAttr(attr)
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
  function(quotetype, content)
    return {c = {quotetype, ensureInlineList(content)}}
  end,
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
    return {c = {ensureAttr(attr), ensureInlineList(content)}}
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
-- @treturn     Inline                  superscript element
M.Superscript = M.Inline:create_constructor(
  "Superscript",
  function(content) return {c = ensureInlineList(content)} end,
  "content"
)

--- Creates an Underline inline element
-- @function Underline
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  underline element
M.Underline = M.Inline:create_constructor(
  "Underline",
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
    if v == nil and not cur then
      -- deleted key does not exists in list
      return
    elseif v == nil then
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
  return setmetatable({identifier, classes, attributes}, self.behavior)
end
M.Attr.behavior.clone = M.types.clone.Attr
M.Attr.behavior.tag = 'Attr'
M.Attr.behavior._field_names = {identifier = 1, classes = 2, attributes = 3}
M.Attr.behavior.__eq = utils.equals
M.Attr.behavior.__index = function(t, k)
  return (k == 't' and t.tag) or
    rawget(t, getmetatable(t)._field_names[k]) or
    getmetatable(t)[k]
end
M.Attr.behavior.__newindex = function(t, k, v)
  if k == 'attributes' then
    rawset(t, 3, setmetatable(to_alist(v or {}), AttributeList))
  elseif getmetatable(t)._field_names[k] then
    rawset(t, getmetatable(t)._field_names[k], v)
  else
    rawset(t, k, v)
  end
end
M.Attr.behavior.__pairs = function(t)
  local field_names = M.Attr.behavior._field_names
  local fields = {}
  for name, i in pairs(field_names) do
    fields[i] = name
  end
  return make_next_function(fields), t, nil
end

-- Monkey-patch setters for `attr` fields to be more forgiving in the input that
-- results in a valid Attr value.
function augment_attr_setter (setters)
  if setters.attr then
    local orig = setters.attr
    setters.attr = function(k, v)
      orig(k, ensureAttr(v))
    end
  end
end
for _, blk in pairs(M.Block.constructor) do
  augment_attr_setter(blk.behavior.setters)
end
for _, inln in pairs(M.Inline.constructor) do
  augment_attr_setter(inln.behavior.setters)
end


-- Citation
M.Citation = AstElement:make_subtype'Citation'
M.Citation.behavior.clone = M.types.clone.Citation

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

-- ListAttributes
M.ListAttributes = AstElement:make_subtype 'ListAttributes'
M.ListAttributes.behavior.clone = M.types.clone.ListAttributes

--- Creates a set of list attributes.
-- @function ListAttributes
-- @tparam[opt] integer start number of the first list item
-- @tparam[opt] string  style style used for list numbering
-- @tparam[opt] DefaultDelim|Period|OneParen|TwoParens delimiter delimiter of list numbers
-- @treturn table list attributes table
function M.ListAttributes:new (start, style, delimiter)
  start = start or 1
  style = style or 'DefaultStyle'
  delimiter = delimiter or 'DefaultDelim'
  return {start, style, delimiter}
end
M.ListAttributes.behavior._field_names = {start = 1, style = 2, delimiter = 3}
M.ListAttributes.behavior.__eq = utils.equals
M.ListAttributes.behavior.__index = function (t, k)
  return rawget(t, getmetatable(t)._field_names[k]) or
    getmetatable(t)[k]
end
M.ListAttributes.behavior.__newindex = function (t, k, v)
  if getmetatable(t)._field_names[k] then
    rawset(t, getmetatable(t)._field_names[k], v)
  else
    rawset(t, k, v)
  end
end
M.ListAttributes.behavior.__pairs = function(t)
  local field_names = M.ListAttributes.behavior._field_names
  local fields = {}
  for name, i in pairs(field_names) do
    fields[i] = name
  end
  return make_next_function(fields), t, nil
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
M.sha1 = utils.sha1

return M
