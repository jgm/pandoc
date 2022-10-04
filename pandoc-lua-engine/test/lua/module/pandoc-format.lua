local tasty = require 'tasty'

local test = tasty.test_case
local group = tasty.test_group
local assert = tasty.assert

local format = require 'pandoc.format'

return {
  group 'default_extensions' {
    test('docx', function ()
      local docx_default_exts = {
        'auto_identifiers',
      }
      assert.are_same(format.default_extensions('docx'), docx_default_exts)
    end),
  },

  group 'all_extensions' {
    test('docx', function ()
      local docx_default_exts = {
        'ascii_identifiers',
        'auto_identifiers',
        'citations',
        'east_asian_line_breaks',
        'empty_paragraphs',
        'gfm_auto_identifiers',
        'native_numbering',
        'styles',
      }
      assert.are_same(format.all_extensions('docx'), docx_default_exts)
    end),
  },
}
