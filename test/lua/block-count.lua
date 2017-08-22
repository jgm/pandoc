local num_blocks = 0

function Block(el)
  num_blocks = num_blocks + 1
end

function Pandoc(blocks, meta)
  return pandoc.Pandoc {
    pandoc.Para{pandoc.Str(num_blocks)}
  }
end
