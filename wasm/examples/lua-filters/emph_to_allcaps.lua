function Emph(el)
  return el:walk({
    Str = function(el)
      el.text = pandoc.text.upper(el.text)
      return el
    end })
end
