function Writer (doc, opts)
  return pandoc.write(doc, 'gfm', opts)
end

function Template ()
  return pandoc.template.compile '<!-- start -->\n$body$\n<!-- stop -->\n'
end
