--[[
pandoc.lua

Copyright © 2017 Albert Krewinkel

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
-- @copyright © 2017 Albert Krewinkel
-- @license MIT
local M = {
  _VERSION = "0.2.0"
}

--- Attributes
-- @type Attributes
M.Attributes = {}
setmetatable(M.Attributes, M.Attributes)

M.Attributes.__index = function(t, k)
  if k == "id" then
    return t[1]
  elseif k == "class" then
    return table.concat(t[2], ' ')
  else
    return t.kv[k]
  end
end

--- Create a new set of attributes (Attr).
-- @function Attributes
M.Attributes.__call = function(t, key_values, id, classes)
  local kv = {}
  for i = 1, #key_values do
    kv[key_values[i][1]] = key_values[i][2]
  end
  id = id or ''
  classes = classes or {}
  local attr = {id, classes, key_values, kv = kv}
  setmetatable(attr, t)
  return attr
end
M.Attributes.empty = M.Attributes('', {}, {})


--- Creates a single citation.
-- @function Citation
-- @tparam      string       id       citation identifier (like a bibtex key)
-- @tparam      AuthorInText|SuppressAuthor|NormalCitation mode citation mode
-- @tparam[opt] {Inline,...} prefix   citation prefix
-- @tparam[opt] {Inline,...} suffix   citation suffix
-- @tparam[opt] int          note_num note number
-- @tparam[opt] int          note_num hash number
M.Citation = function(id, mode, prefix, suffix, note_num, hash)
  prefix = prefix or {}
  suffix = suffix or {}
  note_num = note_num or 0
  hash = hash or 0
  return {
    citationId = id,
    citationPrefix = prefix,
    citationSuffix = suffix,
    citationMode = mode,
    citationNoteNum = note_num,
    citationHash = hash,
  }
end

------------------------------------------------------------------------
-- The base class for pandoc's AST elements.
-- @type Element
-- @local
local Element = {}

--- Create a new element subtype
-- @local
function Element:make_subtype(o)
  o = o or {}
  setmetatable(o, self)
  self.__index = self
  return o
end

--- Create a new element given its tag and arguments
-- @local
function Element:new(tag, ...)
  local element = { t = tag }
  local content = {...}
  -- special case for unary constructors
  if #content == 1 then
    element.c = content[1]
  -- Don't set 'c' field if no further arguments were given. This is important
  -- for nullary constructors like `Space` and `HorizontalRule`.
  elseif #content > 0 then
    element.c = content
  end
  setmetatable(element, self)
  self.__index = self
  return element
end

--- Create a new constructor
-- @local
-- @param tag Tag used to identify the constructor
-- @param fn Function to be called when constructing a new element
-- @return function that constructs a new element
function Element:create_constructor(tag, fn)
  local constr = self:make_subtype({tag = tag})
  function constr:new(...)
    local obj = fn(...)
    setmetatable(obj, self)
    self.__index = function(t, k)
      if k == "c" then
        return t["content"]
      elseif k == "t" then
        return getmetatable(t)["tag"]
      else
        return getmetatable(t)[k]
      end
    end
    return obj
  end
  self.constructor = self.constructor or {}
  self.constructor[tag] = constr
  return constr
end

--- Calls the constructor, creating a new element.
-- @local
function Element.__call(t, ...)
  return t:new(...)
end

------------------------------------------------------------------------
--- Pandoc Document
-- @section document

--- A complete pandoc document
-- @function Doc
-- @tparam      {Block,...} blocks      document content
-- @tparam[opt] Meta        meta        document meta data
function M.Doc(blocks, meta)
  meta = meta or {}
  return {
    ["blocks"] = blocks,
    ["meta"] = meta,
    ["pandoc-api-version"] = {1,17,0,5},
  }
end


------------------------------------------------------------------------
-- MetaValue
-- @section MetaValue
M.MetaValue = Element:make_subtype{}
M.MetaValue.__call = function(t, ...)
  return t:new(...)
end
--- Meta blocks
-- @function MetaBlocks
-- @tparam {Block,...} blocks blocks
--- Meta inlines
-- @function MetaInlines
-- @tparam {Inline,...} inlines inlines
--- Meta list
-- @function MetaList
-- @tparam {MetaValue,...} meta_values list of meta values
--- Meta boolean
-- @function MetaBool
-- @tparam boolean bool boolean value
--- Meta map
-- @function MetaMap
-- @tparam table a string-index map of meta values
--- Meta string
-- @function MetaString
-- @tparam string str string value
M.meta_value_types = {
  "MetaBlocks",
  "MetaBool",
  "MetaInlines",
  "MetaList",
  "MetaMap",
  "MetaString"
}
for i = 1, #M.meta_value_types do
  M[M.meta_value_types[i]] = M.MetaValue:create_constructor(
    M.meta_value_types[i],
    function(content)
      return {c = content}
    end
  )
end

--- Inline element class
-- @type Inline
M.Inline = Element:make_subtype{}
M.Inline.__call = function (t, ...)
  return t:new(...)
end

------------------------------------------------------------------------
-- Inline
-- @section Inline

--- Creates a Cite inline element
-- @function Cite
-- @tparam {Inline,...}   content       List of inlines
-- @tparam {Citation,...} citations     List of citations
-- @treturn Inline citations element
M.Cite = M.Inline:create_constructor(
  "Cite",
  function(content, citations) return {c = {citations, content}} end
)

--- Creates a Code inline element
-- @function Code
-- @tparam      string      code        brief image description
-- @tparam[opt] Attributes  attributes  additional attributes
-- @treturn Inline code element
M.Code = M.Inline:create_constructor(
  "Code",
  function(code, attributes) return {c = {attributes, code}} end
)

--- Creates an inline element representing emphasised text.
-- @function Emph
-- @tparam      {Inline,..} content     inline content
-- @treturn Inline emphasis element
M.Emph = M.Inline:create_constructor(
  "Emph",
  function(content) return {c = content} end
)

--- Creates a Image inline element
-- @function Image
-- @tparam      {Inline,..} caption     text used to describe the image
-- @tparam      string      src         path to the image file
-- @tparam[opt] string      title       brief image description
-- @tparam[opt] Attributes  attributes  additional attributes
-- @treturn Inline image element
M.Image = M.Inline:create_constructor(
  "Image",
  function(caption, src, title, attributes)
    title = title or ""
    attributes = attributes or Attribute.empty
    return {c = {attributes, caption, {src, title}}}
  end
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
-- @tparam[opt] Attributes  attributes  additional attributes
-- @treturn Inline image element
M.Link = M.Inline:create_constructor(
  "Link",
  function(content, target, title, attributes)
    title = title or ""
    attributes = attributes or Attribute.empty
    return {c = {attributes, content, {target, title}}}
  end
)

--- Creates a Math inline element
-- @function Math
-- @tparam      InlineMath|DisplayMath mathtype Display specifier
-- @tparam      string      text        Math content
-- @treturn     Inline                  Math element
M.Math = M.Inline:create_constructor(
  "Math",
  function(mathtype, text)
    return {c = {mathtype, text}}
  end
)

--- Creates a Note inline element
-- @function Note
-- @tparam      {Block,...} content     footnote block content
M.Note = M.Inline:create_constructor(
  "Note",
  function(contents) return {c = contents} end
)

--- Creates a Quoted inline element
-- @function Quoted
-- @tparam      DoubleQuote|SingleQuote quotetype type of quotes to be used
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  quoted element
M.Quoted = M.Inline:create_constructor(
  "Quoted",
  function(quotetype, content) return {c = {quotetype, content}} end
)
--- Creates a RawInline inline element
-- @function RawInline
-- @tparam      string      format      format of the contents
-- @tparam      string      text        string content
-- @treturn     Inline                  raw inline element
M.RawInline = M.Inline:create_constructor(
  "RawInline",
  function(format, text) return {c = {format, text}} end
)

--- Creates text rendered in small caps
-- @function SmallCaps
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  smallcaps element
M.SmallCaps = M.Inline:create_constructor(
  "SmallCaps",
  function(content) return {c = content} end
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
-- @tparam[opt] Attributes  attributes  additional attributes
-- @treturn Inline span element
M.Span = M.Inline:create_constructor(
  "Span",
  function(content, attributes) return {c = {attributes, content}} end
)

--- Creates a Str inline element
-- @function Str
-- @tparam      string      text        content
-- @treturn     Inline                  string element
M.Str = M.Inline:create_constructor(
  "Str",
  function(text) return {c = text} end
)

--- Creates text which is striked out.
-- @function Strikeout
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  strikeout element
M.Strikeout = M.Inline:create_constructor(
  "Strikeout",
  function(content) return {c = content} end
)

--- Creates a Strong element, whose text is usually displayed in a bold font.
-- @function Strong
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  strong element
M.Strong = M.Inline:create_constructor(
  "Strong",
  function(content) return {c = content} end
)

--- Creates a Subscript inline element
-- @function Subscript
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  subscript element
M.Subscript = M.Inline:create_constructor(
  "Subscript",
  function(content) return {c = content} end
)

--- Creates a Superscript inline element
-- @function Superscript
-- @tparam      {Inline,..} content     inline content
-- @treturn     Inline                  strong element
M.Superscript = M.Inline:create_constructor(
  "Superscript",
  function(content) return {c = content} end
)


------------------------------------------------------------------------
-- Block elements
-- @type Block
M.Block = Element:make_subtype{}

--- Block constructors
M.Block.constructors = {
  BlockQuote = true,
  BulletList = true,
  CodeBlock = true,
  DefinitionList = true,
  Div = true,
  Header = true,
  HorizontalRule = true,
  HorizontalRule = true,
  LineBlock = true,
  Null = true,
  OrderedList = true,
  Para = true,
  Plain = true,
  RawBlock = true,
  Table = true,
}

local set_of_inline_types = {}
for k, _ in pairs(M.Inline.constructor) do
  set_of_inline_types[k] = true
end

for block_type, _ in pairs(M.Block.constructors) do
  M[block_type] = function(...)
    return M.Block:new(block_type, ...)
  end
end


------------------------------------------------------------------------
-- Constants
-- @section constants

--- Math content is to be displayed on a separate line.
-- @see Math
M.DisplayMath = {}
M.DisplayMath.t = "DisplayMath"
--- Math content is to be displayed inline within the paragraph
-- @see Math
M.InlineMath = {}
M.InlineMath.t = "InlineMath"

--- Double quoted content.
-- @see Quoted
M.DoubleQuote = {}
M.DoubleQuote.t = "DoubleQuote"

--- Single quoted content.
-- @see Quoted
M.SingleQuote = {}
M.SingleQuote.t = "SingleQuote"

--- Author name is mentioned in the text.
-- @see Citation
-- @see Cite
M.AuthorInText = {}
M.AuthorInText.t = "AuthorInText"

--- Author name is suppressed.
-- @see Citation
-- @see Cite
M.SuppressAuthor = {}
M.SuppressAuthor.t = "SuppressAuthor"

--- Default citation style is used.
-- @see Citation
-- @see Cite
M.NormalCitation = {}
M.NormalCitation.t = "NormalCitation"


------------------------------------------------------------------------
-- Helper Functions
-- @section helpers

--- Use functions defined in the global namespace to create a pandoc filter.
-- All globally defined functions which have names of pandoc elements are
-- collected into a new table.
-- @return A list of filter functions
-- @usage
-- -- within a file defining a pandoc filter:
-- function Str(text)
--   return pandoc.Str(utf8.upper(text))
-- end
--
-- return {pandoc.global_filter()}
-- -- the above is equivallent to
-- -- return {{Str = Str}}
function M.global_filter()
  local res = {}
  for k, v in pairs(_G) do
    if M.Inline.constructor[k] or M.Block.constructors[k] or k == "Doc" then
      res[k] = v
    end
  end
  return res
end

return M
