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

--- Try hard to turn the arguments into an Attr object.
local function ensureAttr(attr)
  if type(attr) == 'userdata' then
    return attr
  end
  return M.Attr(attr)
end

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
-- Element components
-- @section components

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

--
-- Legacy and compatibility types
--

--- Creates a simple (old style) table element.
-- @function SimpleTable
-- @tparam      {Inline,...} caption    table caption
-- @tparam      {AlignDefault|AlignLeft|AlignRight|AlignCenter,...} aligns alignments
-- @tparam      {int,...}    widths     column widths
-- @tparam      {Block,...}  headers    header row
-- @tparam      {{Block,...}} rows      table rows
-- @treturn     Block                   table element
M.SimpleTable = function(caption, aligns, widths, headers, rows)
  return {
    caption = ensureInlineList(caption),
    aligns = List:new(aligns),
    widths = List:new(widths),
    headers = List:new(headers),
    rows = List:new(rows),
    tag = "SimpleTable",
    t = "SimpleTable",
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
M.sha1 = utils.sha1

return M
