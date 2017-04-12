--[[
pandoc.lua

Copyright (c) 2017 Albert Krewinkel

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

--- The module
local M = {
  _version = "0.1.0"
}

--- Create a new set of attributes (Attr).
function M.Attributes(id, classes, key_values)
  return {id, classes, key_values}
end

------------------------------------------------------------------------
--- Document AST elements
local Element = {}

--- Create a new element subtype
function Element:make_subtype(o)
  o = o or {}
  setmetatable(o, self)
  self.__index = self
  return o
end

--- Create a new element given its tag and arguments
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
  return constr
end

function Element.__call(t, ...)
  return t:new(...)
end

------------------------------------------------------------------------
-- Document
local function Doc(blocks, meta)
  return {
    ["blocks"] = blocks,
    ["meta"] = meta,
    ["pandoc-api-version"] = {1,17,0,5},
  }
end
local Inline = Element:make_subtype{}
function Inline.__call(t, ...)
  return t:new(...)
end

local Block = Element:make_subtype{}

------------------------------------------------------------------------
-- Inline element constructors
-- @section

--- Create a Cite inline element
-- @function Inline.Cite
Inline.Cite = Inline:create_constructor(
  "Cite",
  function(lst, cs) return {c = {cs, lst}} end
)
--- Create a Code inline element
-- @function Inline.Code
Inline.Code = Inline:create_constructor(
  "Code",
  function(code, attr) return {c = {attr, code}} end
)
--- Create a Emph inline element
-- @function Inline.Emph
Inline.Emph = Inline:create_constructor(
  "Emph",
  function(xs) return {c = xs} end
)
--- Create a Image inline element
-- @function Inline.Image
Inline.Image = Inline:create_constructor(
  "Image",
  function(capt, src, tit, attr) return {c = {attr, capt, {src, tit}}} end
)
--- Create a LineBreak inline element
-- @function Inline.LineBreak
Inline.LineBreak = Inline:create_constructor(
  "LineBreak",
  function() return {} end
)
--- Create a Link inline element
-- @function Inline.Link
Inline.Link = Inline:create_constructor(
  "Link",
  function(txt, src, tit, attr) return {c = {attr, txt, {src, tit}}} end
)
--- Create a Math inline element
-- @function Inline.Math
Inline.Math = Inline:create_constructor(
  "Math",
  function(m, str) return {c = {m, str}} end
)
--- Create a Note inline element
-- @function Inline.Note
Inline.Note = Inline:create_constructor(
  "Note",
  function(contents) return {c = contents} end
)
--- Create a Quoted inline element
-- @function Inline.Quoted
Inline.Quoted = Inline:create_constructor(
  "Quoted",
  function(qt, lst) return {c = {qt, lst}} end
)
--- Create a RawInline inline element
-- @function Inline.RawInline
Inline.RawInline = Inline:create_constructor(
  "RawInline",
  function(f, xs) return {c = {f, xs}} end
)
--- Create a SmallCaps inline element
-- @function Inline.SmallCaps
Inline.SmallCaps = Inline:create_constructor(
  "SmallCaps",
  function(xs) return {c = xs} end
)
--- Create a SoftBreak inline element
-- @function Inline.SoftBreak
Inline.SoftBreak = Inline:create_constructor(
  "SoftBreak",
  function() return {} end
)
--- Create a Space inline element
-- @function Inline.Space
Inline.Space = Inline:create_constructor(
  "Space",
  function() return {} end
)
--- Create a Span inline element
-- @function Inline.Span
Inline.Span = Inline:create_constructor(
  "Span",
  function(ls, attr) return {c = {attr, xs}} end
)
--- Create a Str inline element
-- @function Inline.Str
Inline.Str = Inline:create_constructor(
  "Str",
  function(str) return {c = str} end
)
--- Create a Strikeout inline element
-- @function Inline.Strikeout
Inline.Strikeout = Inline:create_constructor(
  "Strikeout",
  function(xs) return {c = xs} end
)
--- Create a Strong inline element
-- @function Inline.Strong
Inline.Strong = Inline:create_constructor(
  "Strong",
  function(xs) return {c = xs} end
)
--- Create a Subscript inline element
-- @function Inline.Subscript
Inline.Subscript = Inline:create_constructor(
  "Subscript",
  function(xs) return {c = xs} end
)
--- Create a Superscript inline element
-- @function Inline.Superscript
Inline.Superscript = Inline:create_constructor(
  "Superscript",
  function(xs) return {c = xs} end
)

M.block_types = {
  "BlockQuote",
  "BulletList",
  "CodeBlock",
  "DefinitionList",
  "Div",
  "Header",
  "HorizontalRule",
  "HorizontalRule",
  "LineBlock",
  "Null",
  "OrderedList",
  "Para",
  "Plain",
  "RawBlock",
  "Table",
}

M.inline_types = {
  "Cite",
  "Code",
  "Emph",
  "Image",
  "LineBreak",
  "Link",
  "Math",
  "Note",
  "Quoted",
  "RawInline",
  "SmallCaps",
  "SoftBreak",
  "Space",
  "Span",
  "Str",
  "Strikeout",
  "Strong",
  "Subscript",
  "Superscript"
}


for _, block_type in pairs(M.block_types) do
  M[block_type] = function(...)
    return Block:new(block_type, ...)
  end
end

for _, inline_type in pairs(M.inline_types) do
  M[inline_type] = Inline[inline_type]
end

--- Arrays to provide fast lookup of element types
local set_of_inline_types = {}
local set_of_block_types = {}

for i = 1, #M.inline_types do
  set_of_inline_types[M.inline_types[i]] = true
end
for i = 1, #M.block_types do
  set_of_block_types[M.block_types[i]] = true
end

function M.global_filter()
  local res = {}
  for k, v in pairs(_G) do
    if set_of_inline_types[k] or set_of_block_types[k] or k == "Doc" then
      res[k] = v
    end
  end
  return res
end

M["Doc"] = Doc
M["Inline"] = Inline
M["Block"] = Block

return M
