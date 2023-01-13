function Writer (doc, opts)
  return pandoc.write(doc, 'gfm', opts)
end

function Template ()
  return '<!-- start -->\n$body$\n<!-- stop -->\n'
end
