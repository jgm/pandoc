local tasty = require 'tasty'
local path = require 'pandoc.path'

local assert = tasty.assert
local test = tasty.test_case
local group = tasty.test_group

return {
  group 'path separator' {
    test('is string', function ()
      assert.are_same(type(path.separator), 'string')
    end),
    test('is slash or backslash', function ()
      assert.is_truthy(path.separator:match '^[/\\]$')
    end),
  },
  group 'search path separator' {
    test('is string', function ()
      assert.are_same(type(path.search_path_separator), 'string')
    end),
    test('is colon or semicolon', function ()
      assert.is_truthy(path.search_path_separator:match '^[:;]$')
    end)
  },
  group 'module' {
    test('check function existence', function ()
      local functions = {
        'directory',
        'filename',
        'is_absolute',
        'is_relative',
        'join',
        'make_relative',
        'normalize',
        'split',
        'split_extension',
        'split_search_path',
      }
      for _, f in ipairs(functions) do
        assert.are_equal(type(path[f]), 'function')
      end
    end)
  }
}
