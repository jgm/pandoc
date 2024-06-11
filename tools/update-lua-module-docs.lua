--- Generate documentation for a pandoc Lua module.
-- Copyright: © 2022-2024 Albert Krewinkel
-- License: MIT
--
-- This script can be used as either a custom reader, or as a standalone
-- pandoc Lua script. In the latter case, it expects a module name as
-- argument.

local ipairs, next, pairs, print, tostring, type, warn =
  ipairs, next, pairs, print, tostring, type, warn
local string, table = string, table
local utils = require 'pandoc.utils'
local read, write = pandoc.read, pandoc.write
local Pandoc = pandoc.Pandoc
local Blocks, Inlines, List = pandoc.Blocks, pandoc.Inlines, pandoc.List
local Code, Emph, Link, Span, Str =
  pandoc.Code, pandoc.Emph, pandoc.Link, pandoc.Span, pandoc.Str
local BulletList, DefinitionList, Header, Para, Plain, RawBlock =
  pandoc.BulletList, pandoc.DefinitionList, pandoc.Header, pandoc.Para,
  pandoc.Plain, pandoc.RawBlock

local registry = debug.getregistry()

--- Retrieves the documentation object for the given value.
local function documentation (value)
  return registry['HsLua docs'][value]
end

--- Table containing all known modules
local modules = {}
for k, v in pairs(pandoc) do
  local docs = documentation(v)
  if docs and docs.fields then
    modules[k] = v
  end
end
modules['pandoc'] = pandoc

--- Creates an iterator triple that will return values sorted by key names.
-- @param tbl  table with string keys
-- @return iterator triple to be used in a `for` loop.
local function sorted (tbl)
  local keys = {}
  for key in pairs(tbl) do
    table.insert(keys, key)
  end
  table.sort(keys)
  local i = 0
  local iter = function (_state, ctrl)
    if i > 0 and ctrl == nil then
      return nil
    else
      i = i + 1
      return keys[i], tbl[keys[i]]
    end
  end
  return iter, nil, nil
end

--- Parses text to a list of Block values.
-- @param txt  string value
-- @return {Block,...}
local function read_blocks (txt)
  return read(txt, 'commonmark+smart+wikilinks_title_before_pipe').blocks
end

--- Parses text to a list of Inline values.
-- @param txt  string value
-- @return {Inline,...}
local function read_inlines (txt)
  return utils.blocks_to_inlines(read_blocks(txt))
end

--- Map of all known data types to a heading ID. Used to create hyperlinks.
local known_types = {
  Alignment = 'type-alignment',
  Attr = 'type-attr',
  AttributeList = 'type-attributes',
  Block = 'type-block',
  Blocks = 'type-blocks',
  Caption = 'type-caption',
  Cell = 'type-cell',
  ColSpec = 'type-colspec',
  Doc = 'type-doc',
  ChunkedDoc = 'type-chunkeddoc',
  Figure = 'type-figure',
  Inline = 'type-inline',
  Inlines = 'type-inlines',
  ListAttributes = 'type-listattributes',
  Meta = 'type-meta',
  MetaValue = 'type-metavalue',
  Pandoc = 'type-pandoc',
  ReaderOptions = 'type-readeroptions',
  Row = 'type-row',
  SimpleTable = 'type-simpletable',
  Span = 'type-span',
  Str = 'type-str',
  Table = 'type-table',
  TableBody = 'type-tablebody',
  TableHead = 'type-tablehead',
  TableFoot = 'type-tablefoot',
  Template = 'type-template',
  WriterOptions = 'type-writeroptions',
  Version = 'type-version',
}

local function render_typespec (typespec)
  if typespec.basic then
    return Inlines(Span(typespec.basic, {class="builtin-lua-type"}))
  elseif typespec.named then
    return Inlines(Span(typespec.named, {['unknown-type'] = typespec.named}))
  elseif typespec.sequence then
    local typeinlns = render_typespec(typespec.sequence)
    return Inlines({'{'} .. typeinlns .. {',...}'})
  elseif typespec.sum then
    local result = Inlines{}
    for i, tspec in pairs(List.map(typespec.sum, render_typespec)) do
      if i >= 2 then
        result:insert(Str '|')
      end
      result:extend(tspec)
    end
    return result
  elseif typespec.any then
    return Inlines('any')
  end
  warn("falling back to string representation for type " .. tostring(typespec))
  return Inlines(tostring(typespec))
end

--- Render a type marker.
-- E.g., the type of a parameter.
local function type_to_inlines (typeobj)
  return Inlines ' ('  .. render_typespec(typeobj) .. Inlines ')'
end

--- Append inlines to the last block if possible, or append a new Plain.
local function append_inlines (blocks, inlines)
  local last = blocks[#blocks]
  if last and (last.t == 'Plain' or last.t == 'Para') then
    blocks[#blocks] = Plain(last.content .. inlines)
  else
    table.insert(blocks, Plain(inlines))
  end
  return blocks
end

--- Returns a list of function arguments.
--
-- The parameters are comma-separated; optional arguments are put in brackets.
--
-- @param parameters  list of function parameters
-- @return string
local function argslist (parameters)
  local required = List{}
  local optional = List{}
  for _, param in ipairs(parameters) do
    if param.optional then
      optional:insert(param.name)
    else
      required:extend(optional)
      required:insert(param.name)
      optional = List{}
    end
  end
  if #optional == 0 then
    return table.concat(required, ', ')
  end
  return table.concat(required, ', ') ..
    (#required > 0 and '[, ' or '[') ..
    table.concat(optional, '[, ') .. string.rep(']', #optional)
end

--- Generates rendered documentation for the return values of a function.
-- @param results     list of function results
-- @return {Block,...}
local function render_results (results)
  if type(results) == 'string' then
    return read_blocks(results)
  elseif type(results) == 'table' then
    return {BulletList(
      List(results):map(
        function (res)
          return append_inlines(
            read_blocks(res.description),
            type_to_inlines(res.type)
          )
        end
      )
    )}
  else
    return Blocks{}
  end
end

--- Renders function documentation.
--
-- @param doc         documentation object
-- @param level       the current heading level in the document
-- @param modulename  name of the module that contains this function
-- @return Documentation rendered as list of Blocks
local function render_function (doc, level, modulename)
  local name = doc.name
  level = level or 1
  local id = modulename and modulename .. '.' .. doc.name or ''
  local args = argslist(doc.parameters)
  local paramlist = DefinitionList(
    List(doc.parameters):map(
      function (p)
        return {
          Inlines{Code(p.name)},
          {append_inlines(
            read_blocks(p.description),
            type_to_inlines(p.type)
          )}
        }
      end
    )
  )
  return Blocks{
    Header(level, name, {id}),
    Plain{Code(string.format('%s (%s)', name, args))},
  } .. read_blocks(doc.description)
    .. List(#doc.parameters > 0 and {Para 'Parameters:'} or {})
    .. List{paramlist}
    .. List(#doc.results > 0 and {Para 'Returns:'} or {})
    .. render_results(doc.results)
    .. Blocks(doc.since and {Para{Emph{'Since: ' .. doc.since}}} or {})
end

--- Renders documentation of a module field.
--
-- @param field       field documentation object
-- @param level       the current heading level in the document
-- @param modulename  name of the module that contains this function
-- @return {Block,...}
local function render_field (field, level, modulename)
  local id = modulename and modulename .. '.' .. field.name or ''
  return Blocks{Header(level, field.name, {id})} ..
    {Plain(read_inlines(field.description) .. type_to_inlines(field.type))}
end

--- Renders documentation of a data type associated with a module.
--
-- @param name        data type name
-- @param level       the current heading level in the document
-- @param modulename  name of the module that contains this function
-- @return {Block,...}
local function render_type (name, level, modulename)
  -- FIXME: SPECIAL CASE
  -- Ignore Template type in `pandoc.template` module, as the automatic
  -- content doesn't describe it yet.
  if name == 'pandoc Template' then
    return {}
  end
  -- We just want the modulename prefix, as the type names should already
  -- contain the module name to some extend.
  local nameprefix = modulename and
    modulename:match('(.*)%.[a-z]*$') or
    'pandoc'
  local id = nameprefix .. '.' .. name
  local metatable = registry[name]

  local properties = Blocks{}
  if next(metatable.docs.properties) then
    local propattr = {'type-' .. id .. '-properties'}
    local attr
    properties:insert(Header(level + 1, "Properties", propattr))
    for propname, prop in sorted(metatable.docs.properties) do
      attr = {'type-' .. nameprefix .. '.' .. name .. '.' .. propname}
      properties:insert(Header(level + 2, propname, attr))
      properties:insert(
        Plain(read_inlines(prop.description) ..
              type_to_inlines(prop.type))
      )
    end
  end

  local methods = Blocks{}
  if next(metatable.methods) then
    local attr = {'type-' .. id .. '-methods'}
    methods:insert(Header(level + 1, "Methods", attr))
    for _, method in sorted(metatable.methods) do
      -- attr = {'type-' .. modulename .. '.' .. name .. '.' .. name}
      -- methods:insert(Header(level + 2, name, attr))
      methods:extend(render_function(documentation(method), level+2, id))
    end
  end

  local type_description = properties .. methods
  if name == 'Doc' then
    type_description = Blocks{
      Para {"See the description ", Link("above", "#type-doc"), "."}
    }
  end
  local header_id = 'type-' .. nameprefix .. '.' .. name
  known_types[name] = known_types[name] or header_id
  return {Header(level, name, {header_id})} ..
    type_description
end

--- Renders module documentation.
--
-- @param doc         documentation object of the module
-- @return {Block,...}
local function render_module (doc)
  local fields = Blocks{}
  if #doc.fields > 0 then
    fields:insert(Header(2, 'Fields', {doc.name .. '-' .. 'fields'}))
    for _, fld in ipairs(doc.fields) do
      fields:extend(render_field(fld, 3, doc.name))
    end
  end

  local functions = Blocks{}
  if #doc.functions > 0 then
    functions:insert(Header(2, 'Functions', {doc.name .. '-' .. 'functions'}))
    for _, fun in ipairs(doc.functions) do
      functions:extend(render_function(fun, 3, doc.name))
    end
  end

  local typedocs = Blocks{}
  local types = type(doc.types) == 'function' and doc.types() or {}
  for _, ty in ipairs(types) do
    typedocs:extend(render_type(ty, 3, doc.name))
  end
  if #typedocs > 0 then
    typedocs:insert(1, Header(2, 'Types', {doc.name .. '-' .. 'types'}))
  end

  return Blocks{
    Header(1, Inlines('Module ' .. doc.name), {'module-' .. doc.name})} ..
    read_blocks(doc.description) ..
    fields ..
    functions ..
    typedocs
end

--- Renders the documentation of the main "pandoc" module.
-- FIXME: This function shouldn't exist.
local function render_main_pandoc_module (doc)
  local constants_section = Blocks{Header(2, "Constants")}
  local fields = List{}
  for i, field in ipairs(doc.fields) do
    if tostring(field.type) == 'string' then
      constants_section:extend(render_field(field, 2, "pandoc"))
    elseif field.name:match '^[A-Z]' then
      -- Ignore (these are the `Block` and `Inline` tables)
    else
      fields:insert(field)
    end
  end
  local stop_rendering = false
  local functions = List{}
  for _, fn in ipairs(doc.functions) do
    if stop_rendering then
      pandoc.log.info("Not rendered in module pandoc: " .. fn.name .. '\n')
    else
      functions:insert(fn)
    end
    if fn.name == 'SimpleTable' then
      stop_rendering = true
    end
  end
  doc.fields = fields
  doc.functions = functions
  -- product types don't render well, so we document those manually
  doc.types = {}
  return render_module(doc)
end

local autogen_start =
  '\n<!%-%- BEGIN: AUTOGENERATED CONTENT for module ([a-z%.]+) %-%->'
local autogen_end =
  '<!%-%- END: AUTOGENERATED CONTENT %-%->\n'
local reflinks_marker =
  '<!%-%- BEGIN: GENERATED REFERENCE LINKS %-%->\n'

--- Create a raw Markdown block.
-- @param str  Markdown text
-- @return Block
local rawmd = function (str)
  return RawBlock('markdown', str)
end

--- Generate documentation for content marked for auto-generation.
-- Skips all other contents and includes it as raw Markdown.
local function process_document (input, blocks, start)
  local mstart, mstop, module_name = input:find(autogen_start, start)
  if mstart and mstop and module_name then
    print('Generating docs for module ' .. module_name)
    blocks:insert(rawmd(input:sub(start, mstop)))
    local object = modules[module_name] or modules[module_name:gsub('^pandoc%.', '')]
    local docblocks = (object == pandoc)
      and render_main_pandoc_module(documentation(object))
      or render_module(documentation(object))
    blocks:extend(docblocks)
    return process_document(input, blocks, input:find(autogen_end, mstop) or -1)
  else
    local reflinks_stop = select(2, input:find(reflinks_marker, start))
    blocks:insert(rawmd(input:sub(start, reflinks_stop)))
    return blocks
  end
end

--- Custom reader function
-- Processes all markers for auto-generated contents, ignores the rest.
function Reader (inputs)
  local blocks = process_document(tostring(inputs), Blocks{}, 1)
  blocks = blocks:walk {
    Link = function (link)
      if link.title == 'wikilink' then
        link.title = ''
        if known_types[link.target] then
          link.target = '#' .. known_types[link.target]
        else
          warn('Unknown type: ' .. link.target)
        end
        return link
      end
    end,
    Span = function (span)
      local unknown_type = span.attributes['unknown-type']
      if unknown_type and known_types[unknown_type] then
        return Link(span.content, '#' .. known_types[unknown_type])
      elseif span.classes:includes 'builtin-lua-type' then
        return span.content  -- unwrap
      end
    end,
  }
  return Pandoc(blocks)
end

-- For usage as a standalone script.
-- E.g.
--
--     pandoc lua module-docs.lua
--
-- Generate Markdown docs for the given module and writes them to stdout.
if arg and arg[1] then
  local module_name = arg[1]
  local object = modules[module_name]
  local blocks = render_module(documentation(object))
  print(write(Pandoc(blocks), 'markdown'))
end
