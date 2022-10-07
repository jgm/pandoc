function Writer (doc, opts)
  local output = 'smart extension is %s;\ncitations extension is %s\n'
  local status = function (ext)
    return opts.extensions:includes(ext) and 'enabled' or 'disabled'
  end
  return output:format(status('smart'), status('citations'))
end

writer_extensions = {
  smart = true,
  citations = false,
}
