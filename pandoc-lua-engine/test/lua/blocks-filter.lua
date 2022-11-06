function Blocks (blks)
  -- verify that this looks like a `pandoc.List`
  if not blks.find or not blks.map or not blks.filter then
    error("table doesn't seem to be an instance of pandoc.List")
  end
  -- return plain block containing the number of elements in the list
  return {pandoc.Plain {pandoc.Str(tostring(#blks))}}
end
