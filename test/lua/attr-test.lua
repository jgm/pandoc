function Div (div)
  div.attributes.five = ("%d"):format(div.attributes.two + div.attributes.three)
  div.attributes.two = nil
  div.attributes.one = "eins"
  return div
end
