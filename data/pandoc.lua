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
M.path = require 'pandoc.path'
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

--- Create a new table which allows to access numerical indices via accessor
-- functions.
-- @local
local function create_accessor_behavior (tag)
  local behavior = {tag = tag}
  behavior.__eq = utils.equals
  behavior.__index = function(t, k)
    if k == "t" then
      return getmetatable(t)["tag"]
    end
    return getmetatable(t)[k]
  end
  behavior.__pairs = function (t)
    return next, t
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
function AstElement:create_constructor(tag, fn)
  local constr = self:make_subtype(tag, create_accessor_behavior(tag))
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

return M
