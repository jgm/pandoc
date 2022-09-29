local num_inlines = 0

function catch_all(el)
  if el.tag and pandoc.Inline.constructor[el.tag] then
    num_inlines = num_inlines + 1
  end
end

function Pandoc(blocks, meta)
  return pandoc.Pandoc {
    pandoc.Para{pandoc.Str(num_inlines)}
  }
end

return {
  setmetatable(
    {Pandoc = Pandoc},
    {__index = function(_) return catch_all end}
  )
}
