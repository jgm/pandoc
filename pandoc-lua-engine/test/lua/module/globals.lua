local tasty = require 'tasty'

local test = tasty.test_case
local group = tasty.test_group
local assert = tasty.assert

-- These tests exist mainly to catch changes to the JSON representation of
-- WriterOptions and its components. UPDATE THE DOCS if anything changes.
return {
  group 'PANDOC_WRITER_OPTIONS' {
    test('cite_method', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.cite_method), 'string')
    end),
    test('columns', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.columns), 'number')
    end),
    test('dpi', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.dpi), 'number')
    end),
    test('email_obfuscation', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.email_obfuscation), 'string')
    end),
    test('epub_chapter_level', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.epub_chapter_level), 'number')
    end),
    test('epub_fonts', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.epub_fonts), 'table')
    end),
    test('epub_metadata', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.epub_metadata), 'nil')
    end),
    test('epub_subdirectory', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.epub_subdirectory), 'string')
    end),
    test('extensions', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.extensions), 'table')
      for _, v in ipairs(PANDOC_WRITER_OPTIONS.extensions) do
        assert.are_equal(type(v), 'string')
      end
    end),
    test('highlight_style', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.highlight_style), 'table')
    end),
    test('html_math_method', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.html_math_method), 'string')
    end),
    test('html_q_tags', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.html_q_tags), 'boolean')
    end),
    test('identifier_prefix', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.identifier_prefix), 'string')
    end),
    test('incremental', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.incremental), 'boolean')
    end),
    test('listings', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.listings), 'boolean')
    end),
    test('number_offset', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.number_offset), 'table')
      for _, v in ipairs(PANDOC_WRITER_OPTIONS.number_offset) do
        assert.are_equal(type(v), 'number')
      end
    end),
    test('number_sections', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.number_sections), 'boolean')
    end),
    test('prefer_ascii', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.prefer_ascii), 'boolean')
    end),
    test('reference_doc', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.reference_doc), 'nil')
    end),
    test('reference_links', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.reference_links), 'boolean')
    end),
    test('reference_location', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.reference_location), 'string')
    end),
    test('section_divs', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.section_divs), 'boolean')
    end),
    test('setext_headers', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.setext_headers), 'boolean')
    end),
    test('slide_level', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.slide_level), 'nil')
    end),
    test('tab_stop', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.tab_stop), 'number')
    end),
    test('table_of_contents', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.table_of_contents), 'boolean')
    end),
    test('toc_depth', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.toc_depth), 'number')
    end),
    test('top_level_division', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.top_level_division), 'string')
    end),
    test('variables', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.variables), 'table')
    end),
    test('wrap_text', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.wrap_text), 'string')
    end),
  }
}
