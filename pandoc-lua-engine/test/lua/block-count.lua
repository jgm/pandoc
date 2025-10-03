local num_blocks = 0

function Block(_el)
  num_blocks = num_blocks + 1
end

function Pandoc(_blocks, _meta)
  return pandoc.Pandoc {
    pandoc.Para{pandoc.Str(num_blocks)}
  }
end
