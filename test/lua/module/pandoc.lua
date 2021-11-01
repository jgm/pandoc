local tasty = require 'tasty'

local test = tasty.test_case
local group = tasty.test_group
local assert = tasty.assert

function os_is_windows ()
  return package.config:sub(1,1) == '\\'
end

return {
  group 'Attr' {
    group 'Constructor' {
      test('pandoc.Attr is a function', function ()
        assert.are_equal(type(pandoc.Attr), 'function')
      end),
      test('returns null-Attr if no arguments are given', function ()
        local attr = pandoc.Attr()
        assert.are_equal(attr.identifier, '')
        assert.are_same(attr.classes, {})
        assert.are_same(#attr.attributes, 0)
      end),
      test(
        'accepts string-indexed table or list of pairs as attributes',
        function ()
          local attributes_list = {{'one', '1'}, {'two', '2'}}
          local attr_from_list = pandoc.Attr('', {}, attributes_list)

          assert.are_equal(attr_from_list.attributes.one, '1')
          assert.are_equal(attr_from_list.attributes.two, '2')

          local attributes_table = {one = '1', two = '2'}
          local attr_from_table = pandoc.Attr('', {}, attributes_table)
          assert.are_equal(
            attr_from_table.attributes,
            pandoc.AttributeList(attributes_table)
          )
          assert.are_equal(attr_from_table.attributes.one, '1')
          assert.are_equal(attr_from_table.attributes.two, '2')
        end
      )
    },
    group 'Properties' {
      test('has t and tag property', function ()
        local attr = pandoc.Attr('')
        assert.are_equal(attr.t, 'Attr')
        assert.are_equal(attr.tag, 'Attr')
      end)
    },
    group 'AttributeList' {
      test('allows access via fields', function ()
        local attributes = pandoc.Attr('', {}, {{'a', '1'}, {'b', '2'}}).attributes
        assert.are_equal(attributes.a, '1')
        assert.are_equal(attributes.b, '2')
      end),
      test('allows access to pairs via numerical indexing', function ()
        local attributes = pandoc.Attr('', {}, {{'a', '1'}, {'b', '2'}}).attributes
        assert.are_same(attributes[1], {'a', '1'})
        assert.are_same(attributes[2], {'b', '2'})
      end),
      test('allows replacing a pair', function ()
        local attributes = pandoc.AttributeList{{'a', '1'}, {'b', '2'}}
        attributes[1] = {'t','five'}
        assert.are_same(attributes[1], {'t', 'five'})
        assert.are_same(attributes[2], {'b', '2'})
      end),
      test('allows to remove a pair', function ()
         local attributes = pandoc.AttributeList{{'a', '1'}, {'b', '2'}}
         attributes[1] = nil
         assert.are_equal(#attributes, 1)
      end),
      test('adds entries by field name', function ()
        local attributes = pandoc.Attr('',{}, {{'c', '1'}, {'d', '2'}}).attributes
        attributes.e = '3'
        assert.are_same(
          attributes,
          -- checking the full AttributeList would "duplicate" entries
          pandoc.AttributeList{{'c', '1'}, {'d', '2'}, {'e', '3'}}
        )
      end),
      test('deletes entries by field name', function ()
        local attributes = pandoc.Attr('',{}, {a = '1', b = '2'}).attributes
        attributes.a = nil
        assert.is_nil(attributes.a)
        assert.are_same(attributes, pandoc.AttributeList{{'b', '2'}})
      end),
      test('remains unchanged if deleted key did not exist', function ()
        local assoc_list = pandoc.List:new {{'alpha', 'x'}, {'beta', 'y'}}
        local attributes = pandoc.Attr('', {}, assoc_list).attributes
        attributes.a = nil
        local new_assoc_list = pandoc.List()
        for k, v in pairs(attributes) do
          new_assoc_list:insert({k, v})
        end
        assert.are_same(new_assoc_list, assoc_list)
      end),
      test('gives key-value pairs when iterated-over', function ()
        local attributes = {width = '11', height = '22', name = 'test'}
        local attr = pandoc.Attr('', {}, attributes)
        local count = 0
        for k, v in pairs(attr.attributes) do
          assert.are_equal(attributes[k], v)
          count = count + 1
        end
        assert.are_equal(count, 3)
      end)
    },
    group 'HTML-like attribute tables' {
      test('in element constructor', function ()
        local html_attributes = {
          id = 'the-id',
          class = 'class1 class2',
          width = '11',
          height = '12'
        }
        local attr = pandoc.Span('test', html_attributes).attr
        assert.are_equal(attr.identifier, 'the-id')
        assert.are_equal(attr.classes[1], 'class1')
        assert.are_equal(attr.classes[2], 'class2')
        assert.are_equal(attr.attributes.width, '11')
        assert.are_equal(attr.attributes.height, '12')
      end),
      test('element attr setter', function ()
        local html_attributes = {
          id = 'the-id',
          class = 'class1 class2',
          width = "11",
          height = "12"
        }
        local span = pandoc.Span 'test'
        span.attr = html_attributes
        span = span:clone() -- normalize
        assert.are_equal(span.attr.identifier, 'the-id')
        assert.are_equal(span.attr.classes[1], 'class1')
        assert.are_equal(span.attr.classes[2], 'class2')
        assert.are_equal(span.attr.attributes.width, '11')
        assert.are_equal(span.attr.attributes.height, '12')
      end),
      test('element attrbutes setter', function ()
        local attributes = {
          width = "11",
          height = "12"
        }
        local span = pandoc.Span 'test'
        span.attributes = attributes
        assert.are_equal(span.attr.attributes.width, '11')
        assert.are_equal(span.attr.attributes.height, '12')
      end)
    }
  },
  group "Inline elements" {
    test('Link has property `content`', function ()
      local link = pandoc.Link('example', 'https://example.org')
      assert.are_same(link.content, {pandoc.Str 'example'})

      link.content = 'commercial'
      link.target = 'https://example.com'
      assert.are_equal(link, pandoc.Link('commercial', 'https://example.com'))
    end)
  },
  group "Block elements" {
    group "BulletList" {
      test('access items via property `content`', function ()
        local para = pandoc.Para 'one'
        local blist = pandoc.BulletList{{para}}
        assert.are_same({{para}}, blist.content)
      end),
      test('property `content` uses fuzzy marshalling', function ()
        local old = pandoc.Plain 'old'
        local new = pandoc.Plain 'new'
        local blist = pandoc.BulletList{{old}}
        blist.content = {{new}}
        assert.are_same({{new}}, blist:clone().content)
        blist.content = new
        assert.are_same({{new}}, blist:clone().content)
      end),
    },
    group "OrderedList" {
      test('access items via property `content`', function ()
        local para = pandoc.Plain 'one'
        local olist = pandoc.OrderedList{{para}}
        assert.are_same({{para}}, olist.content)
      end),
      test('forgiving constructor', function ()
        local plain = pandoc.Plain 'old'
        local olist = pandoc.OrderedList({plain}, {3, 'Example', 'Period'})
        local listAttribs = pandoc.ListAttributes(3, 'Example', 'Period')
        assert.are_same(olist.listAttributes, listAttribs)
      end),
      test('has list attribute aliases', function ()
        local olist = pandoc.OrderedList({}, {4, 'Decimal', 'OneParen'})
        assert.are_equal(olist.start, 4)
        assert.are_equal(olist.style, 'Decimal')
        assert.are_equal(olist.delimiter, 'OneParen')
      end)
    },
    group 'DefinitionList' {
      test('access items via property `content`', function ()
        local deflist = pandoc.DefinitionList{
          {'apple', {{pandoc.Plain 'fruit'}, {pandoc.Plain 'company'}}},
          {pandoc.Str 'coffee', 'Best when hot.'}
        }
        assert.are_equal(#deflist.content, 2)
        assert.are_same(deflist.content[1][1], {pandoc.Str 'apple'})
        assert.are_same(deflist.content[1][2][2],
                         {pandoc.Plain{pandoc.Str 'company'}})
        assert.are_same(deflist.content[2][2],
                         {{pandoc.Plain{pandoc.Str 'Best when hot.'}}})
      end),
      test('modify items via property `content`', function ()
        local deflist = pandoc.DefinitionList{
          {'apple', {{{'fruit'}}, {{'company'}}}}
        }
        deflist.content[1][1] = pandoc.Str 'orange'
        deflist.content[1][2][1] = {pandoc.Plain 'tasty fruit'}
        local newlist = pandoc.DefinitionList{
          { {pandoc.Str 'orange'},
            {{pandoc.Plain 'tasty fruit'}, {pandoc.Plain 'company'}}
          }
        }
        assert.are_equal(deflist, newlist)
      end),
    },
    group 'Para' {
      test('access inline via property `content`', function ()
        local para = pandoc.Para{'Moin, ', pandoc.Space(), 'Sylt!'}
        assert.are_same(
          para.content,
          {pandoc.Str 'Moin, ', pandoc.Space(), pandoc.Str 'Sylt!'}
        )
      end),
      test('modifying `content` changes the element', function ()
        local para = pandoc.Para{'Moin, ', pandoc.Space(), pandoc.Str 'Sylt!'}

        para.content[3] = 'Hamburg!'
        assert.are_same(
          para:clone().content,
          {pandoc.Str 'Moin, ', pandoc.Space(), pandoc.Str 'Hamburg!'}
        )

        para.content = 'Huh'
        assert.are_same(
          para:clone().content,
          {pandoc.Str 'Huh'}
        )
        end),
    },
    group 'LineBlock' {
      test('access lines via property `content`', function ()
        local spc = pandoc.Space()
        local lineblock = pandoc.LineBlock{
          {'200', spc, 'Main', spc, 'St.'},
          {'Berkeley', spc, 'CA', spc, '94718'}
        }
        assert.are_equal(#lineblock.content, 2) -- has two lines
        assert.are_same(lineblock.content[2][1], pandoc.Str 'Berkeley')
      end),
      test('modifying `content` alter the element', function ()
        local spc = pandoc.Space()
        local lineblock = pandoc.LineBlock{
          {'200', spc, 'Main', spc, 'St.'},
          {'Berkeley', spc, 'CA', spc, '94718'}
        }
        lineblock.content[1][1] = '404'
        assert.are_same(
          lineblock:clone().content[1],
          {pandoc.Str '404', spc, pandoc.Str 'Main', spc, pandoc.Str 'St.'}
        )

        lineblock.content = {{'line1'}, {'line2'}}
        assert.are_same(
          lineblock:clone(),
          pandoc.LineBlock{
            {pandoc.Str 'line1'},
            {pandoc.Str 'line2'}
          }
        )
      end)
    },
  },
  group 'MetaValue elements' {
    test('MetaList elements behave like lists', function ()
      local metalist = pandoc.MetaList{}
      assert.are_equal(type(metalist.insert), 'function')
      assert.are_equal(type(metalist.remove), 'function')
    end),
    test('MetaList, MetaMap, MetaInlines, MetaBlocks have `t` tag', function ()
      assert.are_equal((pandoc.MetaList{}).t, 'MetaList')
      assert.are_equal((pandoc.MetaMap{}).t, 'MetaMap')
      assert.are_equal((pandoc.MetaInlines{}).t, 'MetaInlines')
      assert.are_equal((pandoc.MetaBlocks{}).t, 'MetaBlocks')
    end)
  },
  group 'Other types' {
    group 'SimpleTable' {
      test('can access properties', function ()
        local spc = pandoc.Space()
        local caption = {pandoc.Str 'Languages', spc, pandoc.Str 'overview.'}
        local aligns = {pandoc.AlignDefault, pandoc.AlignDefault}
        local widths = {0, 0} -- let pandoc determine col widths
        local headers = {{pandoc.Plain({pandoc.Str "Language"})},
          {pandoc.Plain({pandoc.Str "Typing"})}}
        local rows = {
          {{pandoc.Plain "Haskell"}, {pandoc.Plain "static"}},
          {{pandoc.Plain "Lua"}, {pandoc.Plain "Dynamic"}},
        }
        local simple_table = pandoc.SimpleTable(
          caption,
          aligns,
          widths,
          headers,
          rows
        )
        assert.are_same(simple_table.caption, caption)
        assert.are_same(simple_table.aligns, aligns)
        assert.are_same(simple_table.widths, widths)
        assert.are_same(simple_table.headers, headers)
        assert.are_same(simple_table.rows, rows)
      end),
      test('can modify properties', function ()
        local new_table = pandoc.SimpleTable(
          {'Languages'},
          {pandoc.AlignDefault, pandoc.AlignDefault},
          {0.5, 0.5},
          {{pandoc.Plain({pandoc.Str "Language"})},
           {pandoc.Plain({pandoc.Str "Typing"})}},
          {
            {{pandoc.Plain "Haskell"}, {pandoc.Plain "static"}},
            {{pandoc.Plain "Lua"}, {pandoc.Plain "Dynamic"}},
          }
        )

        new_table.caption = {pandoc.Str 'Good', pandoc.Space(),
                             pandoc.Str 'languages'}
        new_table.aligns[1] = pandoc.AlignLeft
        new_table.widths = {0, 0}
        new_table.headers[2] = {pandoc.Plain{pandoc.Str 'compiled/interpreted'}}
        new_table.rows[1][2] = {pandoc.Plain{pandoc.Str 'both'}}
        new_table.rows[2][2] = {pandoc.Plain{pandoc.Str 'interpreted'}}

        local expected_table = pandoc.SimpleTable(
          {pandoc.Str 'Good', pandoc.Space(), pandoc.Str 'languages'},
          {pandoc.AlignLeft, pandoc.AlignDefault},
          {0, 0},
          {{pandoc.Plain 'Language'}, {pandoc.Plain 'compiled/interpreted'}},
          {
            {{pandoc.Plain 'Haskell'}, {pandoc.Plain 'both'}},
            {{pandoc.Plain 'Lua'}, {pandoc.Plain 'interpreted'}}
          }
        )
        assert.are_same(expected_table, new_table)
      end)
    }
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
        'Extension empty_paragraphs not supported for gfm'
      )
    end),
    test('failing read', function ()
      assert.error_matches(
        function () pandoc.read('foo', 'nosuchreader') end,
        'Unknown reader: nosuchreader'
      )
    end)
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
  }
}
