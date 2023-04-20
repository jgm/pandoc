local ipairs, load, next, pairs, print, tostring, type, warn =
  ipairs, load, next, pairs, print, tostring, type, warn
local string, table = string, table
local _G, arg = _G, arg

local registry = debug.getregistry()

_ENV = pandoc

local stringify = utils.stringify

--- Retrieves the documentation object for the given value.
local function documentation (value)
  return registry['HsLua docs'][value]
end

local function sorted (tbl)
  local keys = {}
  for key in pairs(tbl) do
    table.insert(keys, key)
  end
  table.sort(keys)
  local i = 0
  local iter = function (state, ctrl)
    if i > 0 and ctrl == nil then
      return nil
    else
      i = i + 1
      return keys[i], tbl[keys[i]]
    end
  end
  return iter, nil, nil
end

local get = function (fieldname)
  return function (obj) return obj[fieldname] end
end

local function read_blocks (txt)
  return read(txt, 'commonmark+smart+wikilinks_title_before_pipe').blocks
end

local function read_inlines (txt)
  return utils.blocks_to_inlines(read_blocks(txt))
end

local known_types = {
  Block = 'type-block',
  Blocks = 'type-blocks',
  ChunkedDoc = 'type-chunkeddoc',
  Inline = 'type-inline',
  Inlines = 'type-inlines',
  Pandoc = 'type-pandoc',
  SimpleTable = 'type-simpletable',
  Table = 'type-table',
  WriterOptions = 'type-writeroptions',
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
  end
  warn("falling back to string representation for type " .. tostring(typespec))
  return Inlines(tostring(typespec))
end

local function type_to_inlines (typeobj)
  if typeobj == nil then
    return Inlines 'any'
  end

  -- Types starting with a capital letter are pandoc types, so we can
  -- link them.
  return Inlines ' (' .. render_typespec(typeobj) .. Inlines ')'
end

local function append_inlines (blocks, inlines)
  local last = blocks[#blocks]
  if last and (last.t == 'Plain' or last.t == 'Para') then
    blocks[#blocks] = Plain(last.content .. inlines)
  else
    table.insert(blocks, Plain(inlines))
  end
  return blocks
end

local function argslist (parameters)
  local required = List{}
  local optional = List{}
  for i, param in ipairs(parameters) do
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

local function render_field (field, level, modulename)
  local id = modulename and modulename .. '.' .. field.name or ''
  return Blocks{Header(level, field.name, {id})} ..
    {Plain(read_inlines(field.description) .. type_to_inlines(field.type))}
end

local function render_type (name, level, modulename)
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
    methods:insert(Header(level + 1, "Methods", mattr))
    for name, method in sorted(metatable.methods) do
      -- attr = {'type-' .. modulename .. '.' .. name .. '.' .. name}
      -- methods:insert(Header(level + 2, name, attr))
      methods:extend(render_function(documentation(method), level+2, id))
    end
  end

  local header_id = 'type-' .. nameprefix .. '.' .. name
  known_types[name] = header_id
  return {Header(level, name, {header_id})} ..
    properties ..
    methods
end

local function render_module (doc)
  local fields = Blocks{}
  if #doc.fields > 0 then
    fields:insert(Header(2, 'Fields', {doc.name .. '-' .. 'fields'}))
    for i, fld in ipairs(doc.fields) do
      fields:extend(render_field(fld, 3, doc.name))
    end
  end

  local functions = Blocks{}
  if #doc.functions > 0 then
    functions:insert(Header(2, 'Functions', {doc.name .. '-' .. 'functions'}))
    for i, fun in ipairs(doc.functions) do
      functions:extend(render_function(fun, 3, doc.name))
    end
  end

  local typedocs = Blocks{}
  local types = type(doc.types) == 'function' and doc.types() or {}
  if #types > 0 then
    typedocs:insert(Header(2, 'Types', {doc.name .. '-' .. 'types'}))
    for i, ty in ipairs(types) do
      typedocs:extend(render_type(ty, 3, doc.name))
    end
  end

  return Blocks{
    Header(1, Inlines('Module ' .. doc.name), {'module-' .. doc.name})} ..
    read_blocks(doc.description) ..
    fields ..
    functions ..
    typedocs
end

local function get_module_name(header)
  return stringify(header):match 'Module pandoc%.([%w]*)'
end

--- Set of modules for which documentation should be generated.
local handled_modules = {
  layout = true
}

local modules = {
  -- 'cli',
  -- 'utils',
  -- 'mediabag',
  -- 'format',
  -- 'path',
  -- 'structure',
  -- 'system',
  -- 'layout',
  -- 'scaffolding',
  -- 'template',
  -- 'types',
  'zip',
}

-- Generate docs for the given module
if arg and arg[1] then
  local module_name = arg[1]
  local object = _ENV[module_name]
  local blocks = render_module(documentation(object))
  print(write(Pandoc(blocks), 'markdown'))
end

local autogen_start =
  '\n<!%-%- BEGIN: AUTOGENERATED CONTENT for module ([a-z%.]+) %-%->'
local autogen_end =
  '<!%-%- END: AUTOGENERATED CONTENT %-%->\n'
local reflinks_marker =
  '<!%-%- BEGIN: GENERATED REFERENCE LINKS %-%->\n'

local rawmd = function (str)
  return RawBlock('markdown', str)
end

local function foo (input, blocks, start)
  local mstart, mstop, module_name = input:find(autogen_start, start)
  if mstart and mstop and module_name then
    print('Generating docs for module ' .. module_name)
    blocks:insert(rawmd(input:sub(start, mstop)))
    local object = _ENV[module_name] or _ENV[module_name:gsub('^pandoc%.', '')]
    blocks:extend(render_module(documentation(object)))
    return foo(input, blocks, input:find(autogen_end, mstop) or -1)
  else
    local reflinks_start, reflinks_stop = input:find(reflinks_marker, start)
    blocks:insert(rawmd(input:sub(start, reflinks_stop)))
    return blocks
  end
end

function _G.Reader (inputs, opts)
  local blocks = foo(tostring(inputs), Blocks{}, 1)
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
