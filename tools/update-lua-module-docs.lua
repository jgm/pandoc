local ipairs, load, pairs, type = ipairs, load, pairs, type
local debug, string, table = debug, string, table
local _G = _G

_ENV = pandoc

local stringify = utils.stringify

local get = function (fieldname)
  return function (obj) return obj[fieldname] end
end

local function read_blocks (txt)
  return read(txt, 'commonmark').blocks
end

local function read_inlines (txt)
  return utils.blocks_to_inlines(read_blocks(txt))
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
  return table.concat(required, ', ')
    .. '[, ' .. table.concat(optional, '[, ') .. string.rep(']', #optional)
end

local function render_results (results)
  if type(results) == 'string' then
    return read_blocks(results)
  elseif type(results) == 'table' then
    return {BulletList(
      List(results):map(
        function (res)
          -- Types starting with a capital letter are pandoc types, so we can
          -- link them.
          local type_ = res.type:match'^[A-Z]'
            and Link(res.type, '#type-' .. res.type:lower())
            or Str(res.type)
          return Para(
            read_inlines(res.description)
            .. {Space()}
            .. Inlines '(' .. Inlines{type_} .. Inlines ')'
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
          {Code(p.name)},
          {read_blocks(p.description .. ' (' .. p.type .. ')')}
        }
      end
    )
  )
  return Blocks{
    Header(level, name, {id}),
    Plain{Code(string.format('%s (%s)', name, args))},
  } .. read_blocks(doc.description)
    .. List(#doc.parameters > 0 and {Para 'Parameters'} or {})
    .. List{paramlist}
    .. List(#doc.results > 0 and {Para 'Returns'} or {})
    .. render_results(doc.results)
end

local function render_field (field, level, modulename)
  local id = modulename and modulename .. '.' .. field.name or ''
  return {Header(level, field.name, {id})} .. read_blocks(field.description)
end

local function render_module (doc)
  local fields = Blocks{}
  if #doc.fields then
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

  return Blocks{
    Header(1, Inlines('Module ' .. doc.name), {'module-' .. doc.name})
  } .. read_blocks(doc.description) .. fields .. functions
end

--- Retrieves the documentation object for the given value.
local function documentation (value)
  return debug.getregistry()['HsLua docs'][value]
end

local function get_module_name(header)
  return stringify(header):match 'Module pandoc%.([%w]*)'
end

--- Set of modules for which documentation should be generated.
local handled_modules = {
  layout = true
}

return {{
    Pandoc = function (doc)
      local blocks = List{}
      local in_module_docs = false
      for i, blk in ipairs(doc.blocks) do
        if blk.t == 'Header' and blk.level == 1 then
          local module_name = get_module_name(blk)
          if module_name and handled_modules[module_name] then
            local object = _ENV[module_name]
            blocks:extend(render_module(documentation(object)))
            in_module_docs = true
          else
            blocks:insert(blk)
            in_module_docs = false
          end
        elseif not in_module_docs then
          blocks:insert(blk)
        end
      end
      return Pandoc(blocks, doc.meta)
    end
}}
