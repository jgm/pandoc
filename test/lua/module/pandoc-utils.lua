local tasty = require 'tasty'
local utils = require 'pandoc.utils'

local assert = tasty.assert
local test = tasty.test_case
local group = tasty.test_group

return {
  group 'blocks_to_inlines' {
    test('default separator', function ()
      local blocks = {
        pandoc.Para { pandoc.Str 'Paragraph1' },
        pandoc.Para { pandoc.Emph { pandoc.Str 'Paragraph2' } }
      }
      local expected = {
        pandoc.Str 'Paragraph1',
        pandoc.Space(), pandoc.Str '¶', pandoc.Space(),
        pandoc.Emph { pandoc.Str 'Paragraph2' }
      }
      assert.are_same(
        expected,
        utils.blocks_to_inlines(blocks)
      )
    end),
    test('custom separator', function ()
      local blocks = {
        pandoc.Para{ pandoc.Str 'Paragraph1' },
        pandoc.Para{ pandoc.Emph 'Paragraph2' }
      }
      local expected = {
        pandoc.Str 'Paragraph1',
        pandoc.LineBreak(),
        pandoc.Emph { pandoc.Str 'Paragraph2' }
      }
      assert.are_same(
        expected,
        utils.blocks_to_inlines(blocks, { pandoc.LineBreak() })
      )
    end)
  },

  group 'make_sections' {
    test('sanity check', function ()
      local blks = {
        pandoc.Header(1, {pandoc.Str 'First'}),
        pandoc.Header(2, {pandoc.Str 'Second'}),
        pandoc.Header(2, {pandoc.Str 'Third'}),
      }
      local hblks = utils.make_sections(true, 1, blks)
      assert.are_equal('Div', hblks[1].t)
      assert.are_equal('Header', hblks[1].content[1].t)
      assert.are_equal('1', hblks[1].content[1].attributes['number'])
    end)
  },

  group 'normalize_date' {
    test('09 Nov 1989', function ()
      assert.are_equal('1989-11-09', utils.normalize_date '09 Nov 1989')
    end),
    test('12/31/2017', function ()
      assert.are_equal('2017-12-31', utils.normalize_date '12/31/2017')
    end),
  },

  group 'sha1' {
    test('hashing', function ()
      local ref_hash = '0a0a9f2a6772942557ab5355d76af442f8f65e01'
      assert.are_equal(ref_hash, utils.sha1 'Hello, World!')
    end)
  },

  group 'stringify' {
    test('inlines', function ()
      local inline = pandoc.Emph{
        pandoc.Str 'Cogito',
        pandoc.Space(),
        pandoc.Str 'ergo',
        pandoc.Space(),
        pandoc.Str 'sum.',
      }
      assert.are_equal('Cogito ergo sum.', utils.stringify(inline))
    end)
  },

  group 'to_roman_numeral' {
    test('convertes number', function ()
      assert.are_equal('MDCCCLXXXVIII', utils.to_roman_numeral(1888))
    end),
    test('fails on non-convertible argument', function ()
      assert.is_falsy(pcall(utils.to_roman_numeral, 'not a number'))
    end)
  },
}
