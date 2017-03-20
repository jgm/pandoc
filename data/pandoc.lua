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

local function Doc(blocks, meta)
  return {
    ["blocks"] = blocks,
    ["meta"] = meta,
    ["pandoc-api-version"] = {1,17,0,5},
  }
end

local Inline = Element:make_subtype{}
local Block = Element:make_subtype{}

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
  "DisplayMath",
  "DoubleQuoted",
  "Emph",
  "Image",
  "InlineMath",
  "LineBreak",
  "Link",
  "Math",
  "Note",
  "Quoted",
  "RawInline",
  "SingleQuoted",
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
  M[inline_type] = function(...)
    return Inline:new(inline_type, ...)
  end
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

return M
