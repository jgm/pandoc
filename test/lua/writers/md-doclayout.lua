-- This is a sample custom writer for pandoc, using DocLayout to
-- produce nicely wrapped output.

PANDOC_USE_DOCLAYOUT = true

doclayout = require 'pandoc.doclayout'
text = require 'text'

-- Table to store footnotes, so they can be included at the end.
local notes = pandoc.List()

-- Blocksep is used to separate block elements.
function Blocksep()
  return doclayout.blankline
end

-- This function is called once for the whole document. Parameters:
-- body is a Doc, metadata is a table, variables is a table.
-- This gives you a fragment.  You could use the metadata table to
-- fill variables in a custom lua template.  Or, pass `--template=...`
-- to pandoc, and pandoc will do the template processing as usual.
function Doc(body, metadata, variables)
  local buffer = pandoc.List()
  buffer:insert(body)
  for i, note in ipairs(notes) do
    buffer:insert(
      doclayout.blankline ..
      doclayout.hang(5, '[^' .. tostring(i) .. ']: ', note)
    )
  end
  return doclayout.concat(buffer, doclayout.cr) .. doclayout.cr
end

function Str(s)
  return s:gsub('[%[%]%#%`%\\]', '\\%0')
end

function Space()
  return doclayout.space
end

function SoftBreak()
  return doclayout.space
end

function LineBreak()
  return doclayout.cr
end

function Emph(s)
  return "_" .. s .. "_"
end

function Strong(s)
  return "**" .. s .. "**"
end

function Subscript(s)
  return "~" .. s .. "~"
end

function Superscript(s)
  return "^" .. s .. "^"
end

function SmallCaps(s)
  return text.upper(s)
end

function Strikeout(s)
  return '<del>' .. s .. '</del>'
end

function Link(s, src, tit, attr)
  local title = tit == ''
    and ''
    or ' ' .. tit:gsub('"', '\\"')
  return doclayout.inside('[', ']', s) ..
    doclayout.inside('(', ')', src .. title)
end

function Image(s, src, tit, attr)
  return '!' .. Link(s, src, tit, attr)
end

function Code(s, attr)
  return doclayout.inside('`', '`', s)
end

function InlineMath(s)
  return doclayout.inside('$', '$', s)
end

function DisplayMath(s)
  return doclayout.inside('$$', '$$', s)
end

function SingleQuoted(s)
  return "'" .. s .. "'"
end

function DoubleQuoted(s)
  return '"' .. s .. '"'
end

function Note(s)
  notes:insert(s)
  return '[^' .. tostring(#notes) .. ']'
end

function Span(s, attr)
  return s
end

function RawInline(format, str)
  return format == "markdown" and str or ''
end

function Cite(s, cs)
  return s
end

function Plain(s)
  return s
end

function Para(s)
  return s
end

-- lev is an integer, the header level.
function Header(lev, s, attr)
  return doclayout.nowrap(string.rep('#', lev) + s)
end

function BlockQuote(s)
  return doclayout.prefixed('> ', s)
end

function HorizontalRule()
  return string.rep('- ', 8):sub(1, -2)
end

function LineBlock(ls)
  return '<div style="white-space: pre-line;">' .. table.concat(ls, '\n') ..
         '</div>'
end

function CodeBlock(s, attr)
  return doclayout.literal '```' / s / '```'
end

function BulletList(items)
  local result = pandoc.List()
  for _, item in ipairs(items) do
    result:insert(doclayout.hang(2, '- ', item))
  end
  return doclayout.concat(result, doclayout.blankline)
end

function OrderedList(items)
  local result = pandoc.List()
  for i, item in ipairs(items) do
    result:insert(doclayout.hang(3, tostring(i) .. '. ', item))
  end
  return doclayout.concat(result, doclayout.blankline)
end

function DefinitionList(items)
  local result = pandoc.List()
  for i, item in ipairs(items) do
    local key, value = next(item)
    result:insert(
      doclayout.hang(2,
        key .. doclayout.cr,
        doclayout.concat(value, doclayout.blankline)
      )
    )
  end
  return doclayout.concat(result, doclayout.blankline)
end

function CaptionedImage(src, tit, caption, attr)
  return Image(caption, src, tit, attr)
end

function Table(caption, aligns, widths, headers, rows)
  return ''
end

function RawBlock(format, str)
  return format == 'markdown' and str or ''
end

function Div(s, attr)
  return s
end

-- The following code will produce runtime warnings when you
-- haven't defined all of the functions you need for the custom
-- writer, so it's useful to include when you're working on a
-- writer.
local meta = {}
meta.__index =
  function(_, key)
    io.stderr:write(string.format("WARNING: Undefined function '%s'\n",key))
    return function() return "" end
  end
setmetatable(_G, meta)
