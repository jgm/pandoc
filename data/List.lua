--[[
List.lua

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
  _VERSION = "0.1.0"
}

------------------------------------------------------------------------
-- Metatable for lists
-- @type List
local List = {}

function List:new (o)
  o = o or {}
  setmetatable(o, self)
  self.__index = self
  return o
end

function List:__call (o)
  return self:new(o)
end

--- Concatenates two lists.
function List:__concat (list)
  local res = List.clone(self)
  List.extend(res, list)
  return res
end

--- Returns a (shallow) copy of the list.
function List:clone ()
  local lst = setmetatable({}, getmetatable(self))
  List.extend(lst, self)
  return lst
end

--- Appends the given list to the end of this list.
function List:includes (needle)
  for i = 1, #self do
    if self[i] == needle then
      return true
    end
  end
  return false
end

--- Returns the value and index of the first occurrence of the given item.
-- @param needle item to search for
-- @return first item equal to the needle, or nil if no such item exists.
-- @return index of that element
function List:find (needle, init)
  return List.find_if(self, function(x) return x == needle end, init)
end

--- Returns the value and index of the first element for which test returns true.
-- @param test the test function
-- @param init index at which the search is started
-- @return first item for which `test` succeeds, or nil if no such item exists.
-- @return index of that element
function List:find_if (test, init)
  init = (init == nil and 1) or (init < 0 and #self - init) or init
  for i = init, #self do
    if test(self[i], i) then
      return self[i], i
    end
  end
  return nil
end

--- Add the given list to the end of this list.
-- @param list list to appended
function List:extend (list)
  for i = 1, #list do
    self[#self + 1] = list[i]
  end
end

-- Returns a copy of the current list by applying the given function to all
-- elements.
function List:map (fn)
  local res = setmetatable({}, getmetatable(self))
  for i = 1, #self do
    res[i] = fn(self[i])
  end
  return res
end

return List
