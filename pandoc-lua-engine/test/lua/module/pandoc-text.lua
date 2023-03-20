--
-- Tests for the pandoc.text module
--
local text = require 'pandoc.text'
local tasty = require 'tasty'

local group = tasty.test_group
local test = tasty.test_case
local assert = tasty.assert

assert.is_function = function (x)
  assert.are_equal(type(x), 'function')
end
-- We rely mostly on the tests in the `hslua-module-text` module. The
-- only thing we need to test is whether `pandoc.text` is available,
-- whether all functions are defined, and whether `require 'text'` works
-- (for backwards compatibility).
return {
  group 'module' {
    test('is table', function ()
      assert.are_equal(type(text), 'table')
    end),
    test('can be required as "text"', function ()
      assert.are_equal(require 'text', require 'pandoc.text')
    end)
  },

  group 'functions' {
    test('fromencoding', function ()
      assert.is_function(text.fromencoding)
    end),
    test('len', function ()
      assert.is_function(text.len)
    end),
    test('lower', function ()
      assert.is_function(text.lower)
    end),
    test('reverse', function ()
      assert.is_function(text.reverse)
    end),
    test('sub', function ()
      assert.is_function(text.sub)
    end),
    test('toencoding', function ()
      assert.is_function(text.toencoding)
    end),
    test('upper', function ()
      assert.is_function(text.upper)
    end),
  },
}
