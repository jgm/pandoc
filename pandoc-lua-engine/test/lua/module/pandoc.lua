local tasty = require 'tasty'

local test = tasty.test_case
local group = tasty.test_group
local assert = tasty.assert

function os_is_windows ()
  return package.config:sub(1,1) == '\\'
end

-- Constructor behavior is tested in the hslua-pandoc-types module, so
-- we just make sure the functions are present.
return {
  group 'Constructors' {
    group 'Misc' {
      test('pandoc.Attr is a function', function ()
        assert.are_equal(type(pandoc.Attr), 'function')
      end),
      test('pandoc.AttributeList is a function', function ()
        assert.are_equal(type(pandoc.AttributeList), 'function')
      end),
      test('pandoc.Blocks is a function', function ()
        assert.are_equal(type(pandoc.Blocks), 'function')
      end),
      test('pandoc.Citation is a function', function ()
        assert.are_equal(type(pandoc.Citation), 'function')
      end),
      test('pandoc.Inlines is a function', function ()
        assert.are_equal(type(pandoc.Inlines), 'function')
      end),
      test('pandoc.SimpleTable is a function', function ()
        assert.are_equal(type(pandoc.SimpleTable), 'function')
      end),
      test('pandoc.Meta is a function', function ()
        assert.are_equal(type(pandoc.Meta), 'function')
      end),
      test('pandoc.Pandoc is a function', function ()
        assert.are_equal(type(pandoc.Pandoc), 'function')
      end),
    },
    group "Inline elements" {
      test('pandoc.AttributeList is a function', function ()
        assert.are_equal(type(pandoc.Cite), 'function')
      end),
      test('pandoc.AttributeList is a function', function ()
        assert.are_equal(type(pandoc.Code), 'function')
      end),
      test('pandoc.Emph is a function', function ()
        assert.are_equal(type(pandoc.Emph), 'function')
      end),
      test('pandoc.Image is a function', function ()
        assert.are_equal(type(pandoc.Image), 'function')
      end),
      test('pandoc.Link is a function', function ()
        assert.are_equal(type(pandoc.Link), 'function')
      end),
      test('pandoc.Math is a function', function ()
        assert.are_equal(type(pandoc.Math), 'function')
      end),
      test('pandoc.Note is a function', function ()
        assert.are_equal(type(pandoc.Note), 'function')
      end),
      test('pandoc.Quoted is a function', function ()
        assert.are_equal(type(pandoc.Quoted), 'function')
      end),
      test('pandoc.SmallCaps is a function', function ()
        assert.are_equal(type(pandoc.SmallCaps), 'function')
      end),
      test('pandoc.SoftBreak is a function', function ()
        assert.are_equal(type(pandoc.SoftBreak), 'function')
      end),
      test('pandoc.Span is a function', function ()
        assert.are_equal(type(pandoc.Span), 'function')
      end),
      test('pandoc.Str is a function', function ()
        assert.are_equal(type(pandoc.Str), 'function')
      end),
      test('pandoc.Strikeout is a function', function ()
      assert.are_equal(type(pandoc.Strikeout), 'function')
      end),
      test('pandoc.Strong is a function', function ()
        assert.are_equal(type(pandoc.Strong), 'function')
      end),
      test('pandoc.Subscript is a function', function ()
        assert.are_equal(type(pandoc.Subscript), 'function')
      end),
      test('pandoc.Superscript is a function', function ()
        assert.are_equal(type(pandoc.Superscript), 'function')
      end),
      test('pandoc.Underline is a function', function ()
        assert.are_equal(type(pandoc.Underline), 'function')
      end),
    },
    group "Block elements" {
      test('pandoc.BlockQuote is a function', function ()
        assert.are_equal(type(pandoc.BlockQuote), 'function')
      end),
      test('pandoc.BulletList is a function', function ()
        assert.are_equal(type(pandoc.BulletList), 'function')
      end),
      test('pandoc.CodeBlock is a function', function ()
        assert.are_equal(type(pandoc.CodeBlock), 'function')
      end),
      test('pandoc.DefinitionList is a function', function ()
        assert.are_equal(type(pandoc.DefinitionList), 'function')
      end),
      test('pandoc.Div is a function', function ()
        assert.are_equal(type(pandoc.Div), 'function')
      end),
      test('pandoc.Header is a function', function ()
        assert.are_equal(type(pandoc.Header), 'function')
      end),
      test('pandoc.LineBlock is a function', function ()
        assert.are_equal(type(pandoc.LineBlock), 'function')
      end),
      test('pandoc.Figure is a function', function ()
        assert.are_equal(type(pandoc.Figure), 'function')
      end),
      test('pandoc.OrderedList is a function', function ()
        assert.are_equal(type(pandoc.OrderedList), 'function')
      end),
      test('pandoc.Para is a function', function ()
        assert.are_equal(type(pandoc.Para), 'function')
      end),
      test('pandoc.Plain is a function', function ()
        assert.are_equal(type(pandoc.Plain), 'function')
      end),
      test('pandoc.RawBlock is a function', function ()
        assert.are_equal(type(pandoc.Plain), 'function')
      end),
      test('pandoc.Table is a function', function ()
        assert.are_equal(type(pandoc.Table), 'function')
      end),
    }
  },
  group 'MetaValue elements' {
    test('MetaList elements behave like lists', function ()
      local metalist = pandoc.MetaList{}
      assert.are_equal(type(metalist.insert), 'function')
      assert.are_equal(type(metalist.remove), 'function')
    end),
    test('`tag` is an alias for `t``', function ()
      assert.are_equal((pandoc.MetaList{}).tag, (pandoc.MetaList{}).t)
      assert.are_equal((pandoc.MetaMap{}).tag, (pandoc.MetaMap{}).t)
      assert.are_equal((pandoc.MetaInlines{}).tag, (pandoc.MetaInlines{}).t)
      assert.are_equal((pandoc.MetaBlocks{}).tag, (pandoc.MetaBlocks{}).t)
    end),
  },
  group 'Meta' {
    test('inline list is treated as MetaInlines', function ()
      local meta = pandoc.Pandoc({}, {test = {pandoc.Emph 'check'}}).meta
      assert.are_same(meta.test, {pandoc.Emph{pandoc.Str 'check'}})
    end),
    test('inline element is treated as MetaInlines singleton', function ()
      local meta = pandoc.Pandoc({}, {test = pandoc.Emph 'check'}).meta
      assert.are_same(meta.test, {pandoc.Emph{pandoc.Str 'check'}})
    end),
    test('block list is treated as MetaBlocks', function ()
      local meta = pandoc.Pandoc({}, {test = {pandoc.Plain 'check'}}).meta
      assert.are_same(meta.test, {pandoc.Plain{pandoc.Str 'check'}})
    end),
    test('block element is treated as MetaBlocks singleton', function ()
      local meta = pandoc.Pandoc({}, {test = pandoc.Plain 'check'}).meta
      assert.are_same(meta.test, {pandoc.Plain{pandoc.Str 'check'}})
    end),
  },
  group 'Pandoc' {
    test('normalize', function ()
      local doc = pandoc.Pandoc({{'a', pandoc.Space(), pandoc.Space(), 'b'}})
      local normalized = pandoc.Pandoc({{'a', pandoc.Space(), 'b'}})
      assert.are_equal(normalized, doc:normalize())
    end),
  },
  group 'Other types' {
    group 'ReaderOptions' {
      test('returns a userdata value', function ()
        local opts = pandoc.ReaderOptions {}
        assert.are_equal(type(opts), 'userdata')
      end),
      test('can construct from table', function ()
        local opts = pandoc.ReaderOptions {columns = 66}
        assert.are_equal(opts.columns, 66)
      end),
      test('can construct from other ReaderOptions value', function ()
        local orig = pandoc.ReaderOptions{columns = 65}
        local copy = pandoc.ReaderOptions(orig)
        for k, v in pairs(orig) do
          assert.are_same(copy[k], v)
        end
        assert.are_equal(copy.columns, 65)
      end),
    },
  },

  group 'clone' {
    test('clones Attr', function ()
      local attr = pandoc.Attr('test', {'my-class'}, {foo = 'bar'})
      local cloned = attr:clone()
      attr.identifier = ''
      attr.classes = {}
      attr.attributes = {}
      assert.are_same(cloned.identifier, 'test')
      assert.are_same(cloned.classes, {'my-class'})
      assert.are_same(cloned.attributes.foo, 'bar')
    end),
    test('clones ListAttributes', function ()
      local la = pandoc.ListAttributes(2, pandoc.DefaultStyle, pandoc.Period)
      local cloned = la:clone()
      la.start = 9
      assert.are_same(cloned.start, 2)
    end),
    test('clones Para', function ()
      local para = pandoc.Para {pandoc.Str 'Hello'}
      local cloned = para:clone()
      para.content[1].text = 'bye'
      assert.are_same(cloned, pandoc.Para {pandoc.Str 'Hello'})
    end),
    test('clones Str', function ()
      local str = pandoc.Str 'Hello'
      local cloned = str:clone()
      str.text = 'bye'
      assert.are_same(cloned.text, 'Hello')
    end),
    test('clones Citation', function ()
      local cite = pandoc.Citation('leibniz', pandoc.AuthorInText)
      local cloned = cite:clone()
      cite.id = 'newton'
      assert.are_same(cloned.id, 'leibniz')
      assert.are_same(cite.id, 'newton')
      assert.are_same(cite.mode, cloned.mode)
    end),
  },

  group 'pipe' {
    test('external string processing', function ()
      if os_is_windows() then
        local pipe_result = pandoc.pipe('find', {'hi'}, 'hi')
        assert.are_equal('hi', pipe_result:match '%a+')
      else
        local pipe_result = pandoc.pipe('tr', {'a', 'b'}, 'abc')
        assert.are_equal('bbc', pipe_result:match '%a+')
      end
    end),
    test('failing pipe', function ()
      if os_is_windows() then
        local success, err = pcall(pandoc.pipe, 'find', {'/a'}, 'hi')
        assert.is_falsy(success)
        assert.are_equal('find', err.command)
        assert.is_truthy(err.error_code ~= 0)
      else
        local success, err = pcall(pandoc.pipe, 'false', {}, 'abc')
        assert.is_falsy(success)
        assert.are_equal('false', err.command)
        assert.are_equal(1, err.error_code)
        assert.are_equal('', err.output)
      end
    end)
  },

  group 'read' {
    test('Markdown', function ()
      local valid_markdown = '*Hello*, World!\n'
      local expected = pandoc.Pandoc({
          pandoc.Para {
            pandoc.Emph { pandoc.Str 'Hello' },
            pandoc.Str ',',
            pandoc.Space(),
            pandoc.Str 'World!'
          }
      })
      assert.are_same(expected, pandoc.read(valid_markdown))
    end),
    test('unsupported extension', function ()
      assert.error_matches(
        function () pandoc.read('foo', 'gfm+empty_paragraphs') end,
        'The extension empty_paragraphs is not supported for gfm'
      )
    end),
    test('read with other indented code classes', function()
      local indented_code = '    return true'
      local expected = pandoc.Pandoc({
          pandoc.CodeBlock('return true', {class='foo'})
      })
      assert.are_same(
        expected,
        pandoc.read(indented_code, 'markdown', {indented_code_classes={'foo'}})
      )
    end),
    test('can read epub', function ()
      local epub = io.open('lua/module/tiny.epub', 'rb')
      local blocks = pandoc.read(epub:read'a', 'epub').blocks
      assert.are_equal(
        blocks[#blocks],
        pandoc.Para { pandoc.Emph 'EPUB' }
      )
    end),
    test('failing read', function ()
      assert.error_matches(
        function () pandoc.read('foo', 'nosuchreader') end,
        'Unknown input format nosuchreader'
      )
    end),
    group 'read_env' {
      test('images are added to the mediabag', function ()
        local epub = io.open('lua/module/sample.epub', 'rb'):read('a')
        local _ = pandoc.read(epub, 'epub')
        assert.are_equal(#pandoc.mediabag.list(), 1)
      end),
      test('images from EPUB are added when using the sandbox', function ()
        local epub = io.open('lua/module/sample.epub', 'rb'):read('a')
        local _ = pandoc.read(epub, 'epub', nil, {})
        assert.are_equal(#pandoc.mediabag.list(), 1)
      end),
      test('includes work in global env', function ()
        local tex = '\\include{lua/module/include.tex}'
        local doc = pandoc.read(tex, 'latex')
        assert.are_equal(
          doc.blocks,
          pandoc.Blocks{pandoc.Para 'included'}
        )
      end),
      test('sandbox disallows access to the filesystem', function ()
        local tex = '\\include{lua/module/include.tex}'
        local doc = pandoc.read(tex, 'latex', nil, {})
        assert.are_equal(doc.blocks, pandoc.Blocks{})
      end),
      test('files can be added to the sandbox', function ()
        local tex = '\\include{lua/module/include.tex}'
        local doc = pandoc.read(tex, 'latex', nil, {'lua/module/include.tex'})
        assert.are_equal(
          doc.blocks,
          pandoc.Blocks{pandoc.Para 'included'}
        )
      end),
      test('sandbox files can be given as key-value pairs', function ()
        local tex = '\\include{lua/module/include.tex}'
        local files = {
          ['lua/module/include.tex'] = 'Hello'
        }
        local doc = pandoc.read(tex, 'latex', nil, files)
        assert.are_equal(
          doc.blocks,
          pandoc.Blocks{pandoc.Para 'Hello'}
        )
      end),
      test('kv-pairs override contents read from file system', function ()
        local tex = '\\include{lua/module/include.tex}'
        local files = {
          'lua/module/include.tex',
          ['lua/module/include.tex'] = 'Hello'
        }
        local doc = pandoc.read(tex, 'latex', nil, files)
        assert.are_equal(
          doc.blocks,
          pandoc.Blocks{pandoc.Para 'Hello'}
        )
      end),
    },
    group 'extensions' {
      test('string spec', function ()
        local doc = pandoc.read('"vice versa"', 'markdown-smart')
        assert.are_equal(doc, pandoc.Pandoc{pandoc.Para '"vice versa"'})
      end),
      test('unsupported extension', function ()
        assert.error_matches(
          function () pandoc.read('foo', 'gfm+empty_paragraphs') end,
          'The extension empty_paragraphs is not supported for gfm'
        )
      end),
      test('unknown extension', function ()
        local format_spec = { format = 'markdown', extensions = {'nope'}}
        assert.error_matches(
          function () pandoc.read('x', format_spec) end,
          'The extension nope is not supported for markdown'
        )
      end),
      test('fails on invalid extension', function ()
        local format_spec = { format = 'markdown', extensions = {'nope'}}
        assert.error_matches(
          function () pandoc.read('nu-uh', format_spec) end,
          'The extension nope is not supported for markdown'
        )
      end),
    },
  },

  group 'walk_block' {
    test('block walking order', function ()
     local acc = {}
     local nested_nums = pandoc.Div {
       pandoc.Para{pandoc.Str'1'},
       pandoc.Div{
         pandoc.Para{pandoc.Str'2'},
         pandoc.Para{pandoc.Str'3'}
       },
       pandoc.Para{pandoc.Str'4'}
     }
     pandoc.walk_block(
       nested_nums,
       {Para = function (p) table.insert(acc, p.content[1].text) end}
     )
     assert.are_equal('1234', table.concat(acc))
    end)
  },

  group 'walk_inline' {
    test('inline walking order', function ()
      local acc = {}
      local nested_nums = pandoc.Span {
        pandoc.Str'1',
        pandoc.Emph {
          pandoc.Str'2',
          pandoc.Str'3'
        },
        pandoc.Str'4'
      }
      pandoc.walk_inline(
        nested_nums,
        {Str = function (s) table.insert(acc, s.text) end}
      )
      assert.are_equal('1234', table.concat(acc))
    end)
  },

  group 'write' {
    test('string spec', function ()
      local doc = pandoc.Pandoc{pandoc.Quoted('DoubleQuote', 'vice versa')}
      local plain = pandoc.write(doc, 'plain+smart')
      assert.are_equal(plain, '"vice versa"\n')
    end),
    test('table format spec with extensions list', function ()
      local doc = pandoc.Pandoc{pandoc.Quoted('DoubleQuote', 'vice versa')}
      local format_spec = { format = 'plain', extensions = {'smart'}}
      local plain = pandoc.write(doc, format_spec)
      assert.are_equal(plain, '"vice versa"\n')
    end),
    test('table format spec with `enable`/`disable` diff', function ()
      local diff = {
        enable = {'smart'}
      }
      local doc = pandoc.Pandoc{pandoc.Quoted('DoubleQuote', 'vice versa')}
      local format_spec = { format = 'plain', extensions = diff}
      local plain = pandoc.write(doc, format_spec)
      assert.are_equal(plain, '"vice versa"\n')
    end),
    test('table format spec with set-like diff', function ()
      local diff = {
        smart = true,
        auto_identifiers = false
      }
      local doc = pandoc.Pandoc{pandoc.Quoted('DoubleQuote', 'vice versa')}
      local format_spec = { format = 'plain', extensions = diff}
      local plain = pandoc.write(doc, format_spec)
      assert.are_equal(plain, '"vice versa"\n')
    end),
    test('fails on invalid extension', function ()
      local doc = pandoc.Pandoc{'nope'}
      local format_spec = { format = 'plain', extensions = {'nope'}}
      assert.error_matches(
        function () pandoc.write(doc, format_spec) end,
        'The extension nope is not supported for plain'
      )
    end),
  },

  group 'with_state' {
    test('request_headers can be modified', function ()
      local headers = {
        {"Authorization", "Basic my-secret"}
      }
      pandoc.with_state({request_headers = headers}, function ()
        assert.are_same(PANDOC_STATE.request_headers, headers)
      end)
    end),
    test('resource_path can be modified', function ()
      local paths = {'.', '/test/resource/path' }
      pandoc.with_state({resource_path = paths}, function ()
        assert.are_same(PANDOC_STATE.resource_path, paths)
      end)
    end),
    test('user_data_dir can be modified', function ()
      local opts = {user_data_dir = '/my/test/path'}
      pandoc.with_state(opts, function ()
        assert.are_equal(PANDOC_STATE.user_data_dir, '/my/test/path')
      end)
    end),
    test('original value is restored afterwards', function ()
      local orig_user_data_dir = PANDOC_STATE.user_data_dir
      local opts = {user_data_dir = '/my/test/path'}
      pandoc.with_state(opts, function () end)
      assert.are_equal(PANDOC_STATE.user_data_dir, orig_user_data_dir)
    end),
    test('unsupported options trigger an error', function ()
      local orig_log = PANDOC_STATE.log
      local opts = {log = 'nonsense'}
      assert.error_matches(
        function ()
          pandoc.with_state(opts, function ()
            assert.are_same(PANDOC_STATE.log, orig_log)
          end)
        end,
        "Unknown or unsupported"
      )
      assert.are_same(PANDOC_STATE.log, orig_log)
    end),
  },

  group 'Marshal' {
    group 'Inlines' {
      test('Strings are broken into words', function ()
        assert.are_equal(
          pandoc.Emph 'Nice, init?',
          pandoc.Emph{pandoc.Str 'Nice,', pandoc.Space(), pandoc.Str 'init?'}
        )
      end)
    },
    group 'Blocks' {
      test('Strings are broken into words and wrapped in Plain', function ()
        assert.are_equal(
          pandoc.Div{
            pandoc.Plain{pandoc.Str 'Nice,', pandoc.Space(), pandoc.Str 'init?'}
          },
          pandoc.Div{'Nice, init?'}
        )
      end)
    }
  }
}
