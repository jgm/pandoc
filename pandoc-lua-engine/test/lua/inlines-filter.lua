function isWorldAfterSpace (fst, snd)
  return fst and fst.t == 'LineBreak'
   and snd and snd.t == 'Str' and snd.text == 'World!'
end

function Inlines (inlns)
  -- verify that this looks like a `pandoc.List`
  if not inlns.find or not inlns.map or not inlns.filter then
    error("table doesn't seem to be an instance of pandoc.List")
  end

  -- Remove spaces before string "World"
  for i = #inlns-1,1,-1 do
    if isWorldAfterSpace(inlns[i], inlns[i+1]) then
      inlns[i] = pandoc.Space()
    end
  end
  return inlns
end
