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
    group 'Cite' {
      test('has property `content`', function ()
        local cite = pandoc.Cite({pandoc.Emph 'important'}, {})
        assert.are_same(cite.content, {pandoc.Emph {pandoc.Str 'important'}})

        cite.content = 'boring'
        assert.are_equal(cite, pandoc.Cite({pandoc.Str 'boring'}, {}))
      end),
      test('has list of citations in property `cite`', function ()
        local citations = {
          pandoc.Citation('einstein1905', 'NormalCitation')
        }
        local cite = pandoc.Cite('relativity', citations)
        assert.are_same(cite.citations, citations)

        local new_citations = {
          citations[1],
          pandoc.Citation('Poincaré1905', 'NormalCitation')
        }
        cite.citations = new_citations
        assert.are_equal(cite, pandoc.Cite({'relativity'}, new_citations))
      end),
    },
    group 'Code' {
      test('has property `attr`', function ()
        local code = pandoc.Code('true', {id='true', foo='bar'})
        assert.are_equal(code.attr, pandoc.Attr('true', {}, {{'foo', 'bar'}}))

        code.attr = {id='t', fubar='quux'}
        assert.are_equal(
          pandoc.Code('true', pandoc.Attr('t', {}, {{'fubar', 'quux'}})),
          code
        )
      end),
      test('has property `text`', function ()
        local code = pandoc.Code('true')
        assert.are_equal(code.text, 'true')

        code.text = '1 + 1'
        assert.are_equal(pandoc.Code('1 + 1'), code)
      end),
    },
    group 'Emph' {
      test('has property `content`', function ()
        local elem = pandoc.Emph{'two', pandoc.Space(), 'words'}
        assert.are_same(
          elem.content,
          {pandoc.Str 'two', pandoc.Space(), pandoc.Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, pandoc.Emph{'word'})
      end)
    },
    group 'Image' {
      test('has property `caption`', function ()
        local img = pandoc.Image('example', 'a.png')
        assert.are_same(img.caption, {pandoc.Str 'example'})

        img.caption = 'A'
        assert.are_equal(img, pandoc.Image({'A'}, 'a.png'))
      end),
      test('has property `src`', function ()
        local img = pandoc.Image('example', 'sample.png')
        assert.are_same(img.src, 'sample.png')

        img.src = 'example.svg'
        assert.are_equal(img, pandoc.Image('example', 'example.svg'))
      end),
      test('has property `title`', function ()
        local img = pandoc.Image('here', 'img.gif', 'example')
        assert.are_same(img.title, 'example')

        img.title = 'a'
        assert.are_equal(img, pandoc.Image('here', 'img.gif', 'a'))
      end),
      test('has property `attr`', function ()
        local img = pandoc.Image('up', 'upwards.png', '', {'up', {'point'}})
        assert.are_same(img.attr, pandoc.Attr {'up', {'point'}})

        img.attr = pandoc.Attr {'up', {'point', 'button'}}
        assert.are_equal(
          pandoc.Image('up', 'upwards.png', nil, {'up', {'point', 'button'}}),
          img
        )
      end)
    },
    group 'Link' {
      test('has property `content`', function ()
        local link = pandoc.Link('example', 'https://example.org')
        assert.are_same(link.content, {pandoc.Str 'example'})

        link.content = 'commercial'
        link.target = 'https://example.com'
        assert.are_equal(link, pandoc.Link('commercial', 'https://example.com'))
      end),
      test('has property `target`', function ()
        local link = pandoc.Link('example', 'https://example.org')
        assert.are_same(link.content, {pandoc.Str 'example'})

        link.target = 'https://example.com'
        assert.are_equal(link, pandoc.Link('example', 'https://example.com'))
      end),
      test('has property `title`', function ()
        local link = pandoc.Link('here', 'https://example.org', 'example')
        assert.are_same(link.title, 'example')

        link.title = 'a'
        assert.are_equal(link, pandoc.Link('here', 'https://example.org', 'a'))
      end),
      test('has property `attr`', function ()
        local link = pandoc.Link('up', '../index.html', '', {'up', {'nav'}})
        assert.are_same(link.attr, pandoc.Attr {'up', {'nav'}})

        link.attr = pandoc.Attr {'up', {'nav', 'button'}}
        assert.are_equal(
          pandoc.Link('up', '../index.html', nil, {'up', {'nav', 'button'}}),
          link
        )
      end)
    },
    group 'Math' {
      test('has property `text`', function ()
        local elem = pandoc.Math(pandoc.InlineMath, 'x^2')
        assert.are_same(elem.text, 'x^2')
        elem.text = 'a + b'
        assert.are_equal(elem, pandoc.Math(pandoc.InlineMath, 'a + b'))
      end),
      test('has property `mathtype`', function ()
        local elem = pandoc.Math(pandoc.InlineMath, 'x^2')
        assert.are_same(elem.mathtype, 'InlineMath')
        elem.mathtype = pandoc.DisplayMath
        assert.are_equal(elem, pandoc.Math(pandoc.DisplayMath, 'x^2'))
      end),
    },
    group 'Note' {
      test('has property `content`', function ()
        local elem = pandoc.Note{pandoc.Para {'two', pandoc.Space(), 'words'}}
        assert.are_same(
          elem.content,
          {pandoc.Para {pandoc.Str 'two', pandoc.Space(), pandoc.Str 'words'}}
        )
        elem.content = pandoc.Plain 'word'
        assert.are_equal(elem, pandoc.Note{'word'})
      end)
    },
    group 'Quoted' {
      test('has property `content`', function ()
        local elem = pandoc.Quoted('SingleQuote', pandoc.Emph{'emph'})
        assert.are_same(
          elem.content,
          {pandoc.Emph{pandoc.Str 'emph'}}
        )
        elem.content = {'word'}
        assert.are_equal(elem, pandoc.Quoted(pandoc.SingleQuote, {'word'}))
      end),
      test('has property `quotetype`', function ()
        local elem = pandoc.Quoted('SingleQuote', 'a')
        assert.are_same(elem.quotetype, pandoc.SingleQuote)
        elem.quotetype = 'DoubleQuote'
        assert.are_equal(elem, pandoc.Quoted(pandoc.DoubleQuote, {'a'}))
      end)
    },
    group 'SmallCaps' {
      test('has property `content`', function ()
        local elem = pandoc.SmallCaps{'two', pandoc.Space(), 'words'}
        assert.are_same(
          elem.content,
          {pandoc.Str 'two', pandoc.Space(), pandoc.Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, pandoc.SmallCaps{'word'})
      end)
    },
    group 'SoftBreak' {
      test('can be constructed', function ()
        local sb = pandoc.SoftBreak()
        assert.are_equal(sb.t, 'SoftBreak')
      end)
    },
    group 'Span' {
      test('has property `attr`', function ()
        local elem = pandoc.Span('one', {'', {'number'}})
        assert.are_same(
          elem.attr,
          pandoc.Attr('', {'number'})
        )
        elem.attr = {'', {}, {{'a', 'b'}}}
        assert.are_equal(elem, pandoc.Span({'one'}, {a='b'}))
      end),
      test('has property `content`', function ()
        local elem = pandoc.Span{'two', pandoc.Space(), 'words'}
        assert.are_same(
          elem.content,
          {pandoc.Str 'two', pandoc.Space(), pandoc.Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, pandoc.Span{'word'})
      end)
    },
    group 'Str' {
      test('has property `text`', function ()
        local elem = pandoc.Str 'nein'
        assert.are_same(elem.text, 'nein')
        elem.text = 'doch'
        assert.are_equal(elem, pandoc.Str 'doch')
      end)
    },
    group 'Strikeout' {
      test('has property `content`', function ()
        local elem = pandoc.Strikeout{'two', pandoc.Space(), 'words'}
        assert.are_same(
          elem.content,
          {pandoc.Str 'two', pandoc.Space(), pandoc.Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, pandoc.Strikeout{'word'})
      end)
    },
    group 'Strong' {
      test('has property `content`', function ()
        local elem = pandoc.Strong{'two', pandoc.Space(), 'words'}
        assert.are_same(
          elem.content,
          {pandoc.Str 'two', pandoc.Space(), pandoc.Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, pandoc.Strong{'word'})
      end)
    },
    group 'Subscript' {
      test('has property `content`', function ()
        local elem = pandoc.Subscript{'two', pandoc.Space(), 'words'}
        assert.are_same(
          elem.content,
          {pandoc.Str 'two', pandoc.Space(), pandoc.Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, pandoc.Subscript{'word'})
      end)
    },
    group 'Superscript' {
      test('has property `content`', function ()
        local elem = pandoc.Superscript{'two', pandoc.Space(), 'words'}
        assert.are_same(
          elem.content,
          {pandoc.Str 'two', pandoc.Space(), pandoc.Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, pandoc.Superscript{'word'})
      end)
    },
    group 'Underline' {
      test('has property `content`', function ()
        local elem = pandoc.Underline{'two', pandoc.Space(), 'words'}
        assert.are_same(
          elem.content,
          {pandoc.Str 'two', pandoc.Space(), pandoc.Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, pandoc.Underline{'word'})
      end)
    },
  },
  group "Block elements" {
    group 'BlockQuote' {
      test('access content via property `content`', function ()
        local elem = pandoc.BlockQuote{'word'}
        assert.are_same(elem.content, {pandoc.Plain 'word'})
        assert.are_equal(type(elem.content), 'table')

        elem.content = {
          pandoc.Para{pandoc.Str 'one'},
          pandoc.Para{pandoc.Str 'two'}
        }
        assert.are_equal(
          pandoc.BlockQuote{
            pandoc.Para 'one',
            pandoc.Para 'two'
          },
          elem
        )
      end),
    },
    group 'BulletList' {
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
    group 'CodeBlock' {
      test('access code via property `text`', function ()
        local cb = pandoc.CodeBlock('return true')
        assert.are_equal(cb.text, 'return true')
        assert.are_equal(type(cb.text), 'string')

        cb.text = 'return nil'
        assert.are_equal(cb, pandoc.CodeBlock('return nil'))
      end),
      test('access Attr via property `attr`', function ()
        local cb = pandoc.CodeBlock('true', {'my-code', {'lua'}})
        assert.are_equal(cb.attr, pandoc.Attr{'my-code', {'lua'}})
        assert.are_equal(type(cb.attr), 'userdata')

        cb.attr = pandoc.Attr{'my-other-code', {'java'}}
        assert.are_equal(
          pandoc.CodeBlock('true', {'my-other-code', {'java'}}),
          cb
        )
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
                        {{pandoc.Plain{
                            pandoc.Str 'Best', pandoc.Space(),
                            pandoc.Str 'when', pandoc.Space(),
                            pandoc.Str 'hot.'}}})
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
    group 'Div' {
      test('access content via property `content`', function ()
        local elem = pandoc.Div{pandoc.BlockQuote{pandoc.Plain 'word'}}
        assert.are_same(elem.content, {pandoc.BlockQuote{'word'}})
        assert.are_equal(type(elem.content), 'table')

        elem.content = {
          pandoc.Para{pandoc.Str 'one'},
          pandoc.Para{pandoc.Str 'two'}
        }
        assert.are_equal(
          pandoc.Div{
            pandoc.Para 'one',
            pandoc.Para 'two'
          },
          elem
        )
      end),
      test('access Attr via property `attr`', function ()
        local div = pandoc.Div('word', {'my-div', {'sample'}})
        assert.are_equal(div.attr, pandoc.Attr{'my-div', {'sample'}})
        assert.are_equal(type(div.attr), 'userdata')

        div.attr = pandoc.Attr{'my-other-div', {'example'}}
        assert.are_equal(
          pandoc.Div('word', {'my-other-div', {'example'}}),
          div
        )
      end)
    },
    group 'Header' {
      test('access inlines via property `content`', function ()
        local header = pandoc.Header(1, 'test')
        assert.are_same(header.content, {pandoc.Str 'test'})

        header.content = {'new text'}
        assert.are_equal(header, pandoc.Header(1, {'new text'}))
      end),
      test('access Attr via property `attr`', function ()
        local header = pandoc.Header(1, 'test', {'my-test'})
        assert.are_same(header.attr, pandoc.Attr{'my-test'})

        header.attr = 'second-test'
        assert.are_equal(header, pandoc.Header(1, 'test', 'second-test'))
      end),
      test('access level via property `level`', function ()
        local header = pandoc.Header(3, 'test')
        assert.are_same(header.level, 3)

        header.level = 2
        assert.are_equal(header, pandoc.Header(2, 'test'))
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
    group 'OrderedList' {
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
    group 'RawBlock' {
      test('access raw content via property `text`', function ()
        local raw = pandoc.RawBlock('markdown', '- one')
        assert.are_equal(type(raw.text), 'string')
        assert.are_equal(raw.text, '- one')

        raw.text = '+ one'
        assert.are_equal(raw, pandoc.RawBlock('markdown', '+ one'))
      end),
      test('access Format via property `format`', function ()
        local raw = pandoc.RawBlock('markdown', '* hi')
        assert.are_equal(type(raw.format), 'string')
        assert.are_equal(raw.format, 'markdown')

        raw.format = 'org'
        assert.are_equal(pandoc.RawBlock('org', '* hi'), raw)
      end)
    },
    group 'Table' {
      test('access Attr via property `attr`', function ()
        local caption = {long = {pandoc.Plain 'cap'}}
        local tbl = pandoc.Table(caption, {}, {{}, {}}, {}, {{}, {}},
                                 {'my-tbl', {'a'}})
        assert.are_equal(tbl.attr, pandoc.Attr{'my-tbl', {'a'}})

        tbl.attr = pandoc.Attr{'my-other-tbl', {'b'}}
        assert.are_equal(
          pandoc.Table(caption, {}, {{}, {}}, {}, {{}, {}},
                       {'my-other-tbl', {'b'}}),
          tbl
        )
      end),
      test('access caption via property `caption`', function ()
        local caption = {long = {pandoc.Plain 'cap'}}
        local tbl = pandoc.Table(caption, {}, {{}, {}}, {}, {{}, {}})
        assert.are_same(tbl.caption, {long = {pandoc.Plain 'cap'}})

        tbl.caption.short = 'brief'
        tbl.caption.long  = {pandoc.Plain 'extended'}

        local new_caption = {
          short = 'brief',
          long = {pandoc.Plain 'extended'}
        }
        assert.are_equal(
          pandoc.Table(new_caption, {}, {{}, {}}, {}, {{}, {}}),
          tbl
        )
      end),
      test('access column specifiers via property `colspecs`', function ()
        local colspecs = {{pandoc.AlignCenter, 1}}
        local tbl = pandoc.Table({long = {}}, colspecs, {{}, {}}, {}, {{}, {}})
        assert.are_same(tbl.colspecs, colspecs)

        tbl.colspecs[1][1] = pandoc.AlignRight
        tbl.colspecs[1][2] = nil

        local new_colspecs = {{pandoc.AlignRight}}
        assert.are_equal(
          pandoc.Table({long = {}}, new_colspecs, {{}, {}}, {}, {{}, {}}),
          tbl
        )
      end),
      test('access table head via property `head`', function ()
        local head = {pandoc.Attr{'tbl-head'}, {}}
        local tbl = pandoc.Table({long = {}}, {}, head, {}, {{}, {}})
        assert.are_same(tbl.head, head)

        tbl.head[1] = pandoc.Attr{'table-head'}

        local new_head = {'table-head', {}}
        assert.are_equal(
          pandoc.Table({long = {}}, {}, new_head, {}, {{}, {}}),
          tbl
        )
      end),
      test('access table head via property `head`', function ()
        local foot = {{id = 'tbl-foot'}, {}}
        local tbl = pandoc.Table({long = {}}, {}, {{}, {}}, {}, foot)
        assert.are_same(tbl.foot, {pandoc.Attr('tbl-foot'), {}})

        tbl.foot[1] = pandoc.Attr{'table-foot'}

        local new_foot = {'table-foot', {}}
        assert.are_equal(
          pandoc.Table({long = {}}, {}, {{}, {}}, {}, new_foot),
          tbl
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
    end),
    test('`tag` is an alias for `t``', function ()
      assert.are_equal((pandoc.MetaList{}).tag, (pandoc.MetaList{}).t)
      assert.are_equal((pandoc.MetaMap{}).tag, (pandoc.MetaMap{}).t)
      assert.are_equal((pandoc.MetaInlines{}).tag, (pandoc.MetaInlines{}).t)
      assert.are_equal((pandoc.MetaBlocks{}).tag, (pandoc.MetaBlocks{}).t)
    end)
  },
  group 'Other types' {
    group 'Citation' {
      test('checks equality by comparing Haskell values', function()
        assert.are_equal(
          pandoc.Citation('a', pandoc.NormalCitation),
          pandoc.Citation('a', pandoc.NormalCitation)
        )
        assert.is_falsy(
          pandoc.Citation('a', pandoc.NormalCitation) ==
          pandoc.Citation('a', pandoc.AuthorInText)
        )
        assert.is_falsy(
          pandoc.Citation('a', pandoc.NormalCitation) ==
          pandoc.Citation('b', pandoc.NormalCitation)
        )
      end),
    },
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
    },
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
        'Extension empty_paragraphs not supported for gfm'
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
