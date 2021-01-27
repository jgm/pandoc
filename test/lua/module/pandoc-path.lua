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
}
