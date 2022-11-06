local tasty = require 'tasty'
local template = require 'pandoc.template'

local assert = tasty.assert
local test = tasty.test_case
local group = tasty.test_group

return {
  test('is table', function ()
    assert.are_equal(type(template), 'table')
  end),
  group 'default' {
    test('is function', function ()
      assert.are_equal(type(template.default), 'function')
    end),
    test('returns a string for known format', function ()
      assert.are_equal(
        pandoc.utils.type(template.default 'json'),
        'string'
      )
      assert.are_equal(
        pandoc.utils.type(template.default 'markdown'),
        'string'
      )
    end),
    test('fails on unknown format', function ()
      local success, msg = pcall(function ()
          return pandoc.utils.type(template.default 'nosuchformat')
      end)
      assert.is_falsy(success)
    end),
  },
  group 'compile' {
    test('is function', function ()
      assert.are_equal(type(template.compile), 'function')
    end),
    test('returns a Template', function ()
      assert.are_equal(
        pandoc.utils.type(template.compile('$title$')),
        'pandoc Template'
      )
    end),
    test('returns a Template', function ()
      local templ_path = pandoc.path.join{'lua', 'module', 'default.test'}
      assert.are_equal(
        pandoc.utils.type(template.compile('${ partial() }', templ_path)),
        'pandoc Template'
      )
    end),
    test('fails if template has non-existing partial', function ()
      assert.error_matches(
        function () return template.compile('${ nosuchpartial() }') end,
        'Could not find data file'
      )
    end),
    test('works with default template that uses partials', function ()
      local jats_template = template.default 'jats'
      assert.are_equal(type(jats_template), 'string')
      assert.are_equal(
        pandoc.utils.type(template.compile(jats_template)),
        'pandoc Template'
      )
    end),
  },
  group 'apply' {
    test('is function', function ()
      assert.are_equal(type(template.apply), 'function')
    end),
    test('returns a Doc value', function ()
      local tmpl = template.compile('placeholder')
      assert.are_equal(
        pandoc.utils.type(template.apply(tmpl, {})),
        'Doc'
      )
    end),
    test('applies the given context', function ()
      local tmpl = template.compile('song: $title$')
      local context = {title = 'Along Comes Mary'}
      assert.are_equal(
        template.apply(tmpl, context):render(),
        'song: Along Comes Mary'
      )
    end),
    test('accepts string as template', function ()
      local context = {number = '2'}
      assert.are_equal(
        template.apply('Song $number$', context):render(),
        'Song 2'
      )
    end)
  },
}
