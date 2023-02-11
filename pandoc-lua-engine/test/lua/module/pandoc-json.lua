--
-- Tests for the system module
--
local json = require 'pandoc.json'
local tasty = require 'tasty'

local group = tasty.test_group
local test = tasty.test_case
local assert = tasty.assert

return {
  -- Check existence of static fields
  group 'static fields' {
    test('null', function ()
      assert.are_equal(type(json.null), 'userdata')
    end),
  },

  group 'encode' {
    test('string', function ()
      assert.are_equal(json.encode 'one\ntwo', '"one\\ntwo"')
    end),
    test('null', function ()
      assert.are_equal(json.encode(json.null), 'null')
    end),
    test('number', function ()
      assert.are_equal(json.encode(42), '42')
    end),
    test('object', function ()
      assert.are_same(json.encode{a = 5}, '{"a":5}')
    end),
    test('object with metamethod', function ()
      local obj = setmetatable(
        {title = 23},
        {
          __tojson = function (obj)
            return '"Nichts ist so wie es scheint"'
          end
        }
      )
      assert.are_same(json.encode(obj), [["Nichts ist so wie es scheint"]])
    end),
    test('Inline (Space)', function ()
      assert.are_equal(
        json.encode(pandoc.Space()),
        '{"t":"Space"}'
      )
    end),
    test('Block (HorizontalRule)', function ()
      assert.are_equal(
        json.encode(pandoc.HorizontalRule()),
        '{"t":"HorizontalRule"}'
      )
    end),
    test('Inlines list', function ()
      assert.are_equal(
        json.encode(pandoc.Inlines{pandoc.Space()}),
        '[{"t":"Space"}]'
      )
    end),
    test('Pandoc', function ()
      assert.are_equal(
        type(json.encode(pandoc.Pandoc{'Hello from Lua!'})),
        'string'
      )
    end),
  },

  group 'decode' {
    test('string', function ()
      assert.are_equal(json.decode '"one\\ntwo"', 'one\ntwo')
    end),
    test('null', function ()
      assert.are_equal(json.decode 'null', json.null)
    end),
    test('number', function ()
      assert.are_equal(json.decode '42', 42)
    end),
    test('object', function ()
      assert.are_same(json.decode '{"a":5}', {a = 5})
    end),
    test('Inline (Space)', function ()
      assert.are_equal(json.decode '{"t":"Space"}', pandoc.Space())
    end),
    test('Inline (Str)', function ()
      assert.are_equal(json.decode '{"t":"Str", "c":"a"}', pandoc.Str 'a')
    end),
    test('disabled AST check', function ()
      assert.are_same(
        json.decode('{"t":"Str", "c":"a"}', false),
        {t = 'Str', c = 'a'}
      )
    end),
    test('Inlines list', function ()
      assert.are_equal(
        json.decode '[{"t":"Space"}]',
        pandoc.Inlines{pandoc.Space()}
      )
    end)
  },
}
