local tasty = require 'tasty'

local test = tasty.test_case
local group = tasty.test_group
local assert = tasty.assert

-- These tests exist mainly to catch changes to the JSON representation of
-- WriterOptions and its components. UPDATE THE DOCS if anything changes.
return {
  group 'PANDOC_WRITER_OPTIONS' {
    test('chunk_template', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.chunk_template), 'string')
    end),
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
    test('split_level', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.split_level), 'number')
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
    test('highlight_method', function ()
      assert.are_equal(type(PANDOC_WRITER_OPTIONS.highlight_method), 'string')
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
  },

  group 'PANDOC_STATE' {
    test('is a table object', function ()
      assert.are_equal(type(PANDOC_STATE), 'table')
    end),
    test('has property "input_files"', function ()
      assert.are_equal(type(PANDOC_STATE.input_files), 'table')
    end),
    test('has optional property "output_file"', function ()
      -- property may be nil
      if PANDOC_STATE.output_file then
        assert.are_equal(type(PANDOC_STATE.output_file), 'string')
      end
    end),
    test('has property "log"', function ()
      assert.are_equal(type(PANDOC_STATE.log), 'table')
    end),
    test('has property "request_headers"', function ()
      assert.are_equal(type(PANDOC_STATE.request_headers), 'table')
    end),
    test('has property "resource_path"', function ()
      assert.are_equal(type(PANDOC_STATE.resource_path), 'table')
    end),
    test('has optional property "source_url"', function ()
      if PANDOC_STATE.source_url then
        assert.are_equal(type(PANDOC_STATE.source_url), 'string')
      end
    end),
    test('has property "trace"', function ()
      assert.are_equal(type(PANDOC_STATE.trace), 'boolean')
    end),
    test('has optional property "user_data_dir"', function ()
      if PANDOC_STATE.user_data_dir then
        assert.are_equal(type(PANDOC_STATE.user_data_dir), 'string')
      end
    end),
    test('has property "verbosity"', function ()
      assert.are_equal(type(PANDOC_STATE.verbosity), 'string')
    end),
    test('can be deleted without breaking PandocLua monad functions', function()
      local state = PANDOC_STATE
      PANDOC_STATE = nil
      assert.is_nil(pandoc.mediabag.lookup('does-not-exist'))
      PANDOC_STATE = state
    end),
  },
}
