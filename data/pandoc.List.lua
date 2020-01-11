--[[
List.lua

Copyright © 2017–2020 Albert Krewinkel

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. ]]

--- Pandoc's List type and helper methods
-- @classmod pandoc.List
-- @author Albert Krewinkel
-- @copyright © 2017–2020 Albert Krewinkel
-- @license MIT
local List = {
  _VERSION = "1.0.0"
}

--- Create a new list.
-- @param[opt] o table that should be altered into a list (default: `{}`)
-- @return the altered input table
function List:new (o)
  o = o or {}
  setmetatable(o, self)
  self.__index = self
  return o
end

--- Concatenates two lists.
-- @param list second list concatenated to the first
-- @return a new list containing all elements from list1 and list2
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

--- Adds the given list to the end of this list.
-- @param list list to appended
function List:extend (list)
  for i = 1, #list do
    self[#self + 1] = list[i]
  end
end

--- Returns a new list containing all items satisfying a given condition.
-- @param pred condition items must satisfy.
-- @return a new list containing all items for which `test` was true.
function List:filter (pred)
  local res = setmetatable({}, getmetatable(self))
  for i = 1, #self do
    if pred(self[i], i) then
      res[#res + 1] = self[i]
    end
  end
  return res
end

--- Returns the value and index of the first occurrence of the given item.
-- @param needle item to search for
-- @param[opt] init index at which the search is started (default: 1)
-- @return first item equal to the needle, or nil if no such item exists.
-- @return index of that element
function List:find (needle, init)
  return List.find_if(self, function(x) return x == needle end, init)
end

--- Returns the value and index of the first element for which the predicate
--- holds true.
-- @param pred the predicate function
-- @param[opt] init index at which the search is started (default: 1)
-- @return first item for which `test` succeeds, or nil if no such item exists.
-- @return index of that element
function List:find_if (pred, init)
  init = (init == nil and 1) or (init < 0 and #self - init) or init
  for i = init, #self do
    if pred(self[i], i) then
      return self[i], i
    end
  end
  return nil
end

--- Checks if the list has an item equal to the given needle.
-- @param needle item to search for
-- @param[opt] init index at which the search is started; defaults to 1.
-- @return true if a list item is equal to the needle, false otherwise
function List:includes (needle, init)
  return not (List.find(self, needle, init) == nil)
end

--- Insert an element into the list. Alias for `table.insert`.
-- @param list list
-- @param[opt] pos position at which the new element is to be inserted
-- @param value value to insert
List.insert = table.insert

--- Returns a copy of the current list by applying the given function to
-- all elements.
-- @param fn function which is applied to all list items.
function List:map (fn)
  local res = setmetatable({}, getmetatable(self))
  for i = 1, #self do
    res[i] = fn(self[i], i)
  end
  return res
end

--- Remove element from list (alias for `table.remove`)
-- @param list list
-- @param[opt] pos position of the element to be removed (default: #list)
-- @return the removed element
List.remove = table.remove

--- Sort list in-place (alias for `table.sort`)
-- @param list list
-- @param[opt] comp comparison function; default to `<` operator.
List.sort = table.sort

-- Set metatable with __call metamethod. This allows the use of `List`
-- as a constructor function.
local ListMT = {
  __call = List.new
}
setmetatable(List, ListMT)

return List
