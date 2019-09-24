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
      test('returns null-Attr if no arguments are given', function ()
        local attr = pandoc.Attr()
        assert.are_equal(attr.identifier, '')
        assert.are_same(attr.classes, {})
        assert.are_same(attr.attributes, {})
      end),
      test(
        'accepts string-indexed table or list of pairs as attributes',
        function ()
          local attributes_list = pandoc.List:new {{'one', '1'}, {'two', '2'}}
          local attr_from_list = pandoc.Attr('', {}, attributes_list:clone())

          assert.are_same(
            pandoc.List:new(attr_from_list.attributes),
            attributes_list
          )

          local attributes_table = {one = '1', two = '2'}
          local attr_from_table = pandoc.Attr('', {}, attributes_table)

          local assoc_list_from_table =
            pandoc.List:new(attr_from_table.attributes)
          -- won't work in general, but does in this special case
          table.sort(assoc_list_from_table, function(x, y) return x[1]<y[1] end)
          assert.are_same(
            assoc_list_from_table,
            attributes_list
          )
        end
      )
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
      test('adds entries by field name', function ()
        local attributes = pandoc.Attr('',{}, {{'c', '1'}, {'d', '2'}}).attributes
        attributes.e = '3'
        assert.are_same(
          -- checking the full AttributeList would "duplicate" entries
          setmetatable(attributes, nil),
          {{'c', '1'}, {'d', '2'}, {'e', '3'}}
        )
      end),
      test('deletes entries by field name', function ()
        local attributes = pandoc.Attr('',{}, {a = '1', b = '2'}).attributes
        attributes.a = nil
        assert.is_nil(attributes.a)
        local assoc_list = setmetatable(attributes, nil)
        assert.are_same(assoc_list, {{'b', '2'}})
      end),
      test('remains unchanged if deleted key did not exist', function ()
        local assoc_list = pandoc.List:new {{'alpha', 'x'}, {'beta', 'y'}}
        local attributes = pandoc.Attr('', {}, assoc_list:clone()).attributes
        attributes.a = nil
        assert.are_same(pandoc.List:new(attributes), assoc_list)
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
