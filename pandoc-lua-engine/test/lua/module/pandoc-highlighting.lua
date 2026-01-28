local tasty = require 'tasty'

local test = tasty.test_case
local group = tasty.test_group
local assert = tasty.assert

local pandoc = require 'pandoc'
local highlighting = require 'pandoc.highlighting'

return {
  group 'styles' {
    test('is a table', function ()
      assert.are_equal('table', type(highlighting.styles))
    end),
  },

  group 'definitions' {
    test('returns a string', function ()
      local defs = highlighting.definitions('espresso', 'css')
      assert.are_equal('string', type(defs))
    end),
    test('errors when presented with an unknown style name', function ()
      assert.error_matches(
        function ()
          highlighting.definitions('unknown-style', 'css')
        end,
        'Unknown highlight%-style'
      )
    end),
    test('errors when asked to converto to an unsupported format', function ()
      local kate = highlighting.style('kate')
      assert.error_matches(
        function ()
          highlighting.definitions(kate, 'markdown')
        end,
        'Unsupported format'
      )
    end),
  },

  group 'highlight' {
    test('produces highlighted code', function ()
      local espresso = highlighting.style 'espresso'
      local codeblock = pandoc.CodeBlock('print(42, "answer")', {class='lua'})
      local highlighted = highlighting.highlight(codeblock, 'html')
      assert.are_equal('string', type(highlighted))
    end)
  },

  group 'style' {
    test('returns a table for a default style', function ()
      local style = highlighting.style('espresso')
      assert.are_equal('table', type(style))
    end),
    test('errors when presented with an unknown style name', function ()
      assert.error_matches(
        function ()
          highlighting.style('unknown-style')
        end,
        'Unknown highlight%-style unknown%-style'
      )
    end)
  },
}
