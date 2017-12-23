utils = require 'pandoc.utils'

-- hierarchicalize
------------------------------------------------------------------------
function test_hierarchicalize ()
  local blks = {
    pandoc.Header(1, {pandoc.Str 'First'}),
    pandoc.Header(2, {pandoc.Str 'Second'}),
    pandoc.Header(2, {pandoc.Str 'Third'}),
  }
  local hblks = utils.hierarchicalize(blks)
  return hblks[1].t == "Sec"
    and hblks[1].contents[1].t == "Sec"
    and hblks[1].contents[2].numbering[1] == 1
    and hblks[1].contents[2].numbering[2] == 2
end

-- SHA1
------------------------------------------------------------------------
function test_sha1 ()
  local ref_hash = '0a0a9f2a6772942557ab5355d76af442f8f65e01'
  local hash = utils.sha1 'Hello, World!'
  return hash == ref_hash
end

-- Pipe
------------------------------------------------------------------------
function file_exists (filename)
  local fh = io.open(filename, 'r')
  return fh ~= nil and (fh:close() or true)
end

function warn (...) io.stderr:write(...) end

function test_pipe ()
  if not file_exists('/bin/sed') then
    warn 'Did not find /bin/sed, skipping test'
    return true
  end
  local pipe_result = pandoc.pipe('/bin/sed', {'-e', 's/a/b/'}, 'abc')
  return pipe_result == 'bbc'
end

function test_failing_pipe ()
  if not file_exists('/bin/false') then
    warn 'Did not find /bin/false, skipping test'
    return true
  end
  local res, err = pcall(pandoc.pipe, '/bin/false', {}, 'abc')
  return not res and
    err.command == '/bin/false' and
    err.error_code == 1 and
    err.output == ''
end

-- Read
------------------------------------------------------------------------
function test_read ()
  local valid_markdown = '*Hello*, World!\n'
  local res = pandoc.read(valid_markdown).blocks[1].content
  return res[1].t == 'Emph' and res[3].t == 'Space' and res[4].t == 'Str'
end

function test_failing_read ()
  local res, err = pcall(pandoc.read, 'foo', 'nosuchreader')
  return not res and err:match 'Unknown reader: nosuchreader'
end

-- Stringify
------------------------------------------------------------------------
function test_stringify ()
  local inline = pandoc.Emph{
    pandoc.Str 'Cogito',
    pandoc.Space(),
    pandoc.Str 'ergo',
    pandoc.Space(),
    pandoc.Str 'sum.',
  }
  return utils.stringify(inline) == 'Cogito ergo sum.'
end

-- to_roman_numeral
------------------------------------------------------------------------
function test_to_roman_numeral ()
  return utils.to_roman_numeral(1888) == 'MDCCCLXXXVIII'
    -- calling with a string fails
    and not pcall(utils.to_roman_numeral, 'not a number')
end

-- normalize_date
------------------------------------------------------------------------
function test_normalize_date ()
  return utils.normalize_date("12/31/2017") == '2017-12-31'
    and utils.normalize_date("pandoc") == nil
end

-- Return result
------------------------------------------------------------------------
function run(fn)
  return fn() and "OK" or "FAIL"
end

function Para (el)
  return {
    pandoc.Plain{pandoc.Str("hierarchicalize: " .. run(test_hierarchicalize))},
    pandoc.Plain{pandoc.Str("normalize_date: " .. run(test_normalize_date))},
    pandoc.Plain{pandoc.Str("pipe: " .. run(test_pipe))},
    pandoc.Plain{pandoc.Str("failing pipe: " .. run(test_failing_pipe))},
    pandoc.Plain{pandoc.Str("read: " .. run(test_read))},
    pandoc.Plain{pandoc.Str("failing read: " .. run(test_failing_read))},
    pandoc.Plain{pandoc.Str("sha1: " .. run(test_sha1))},
    pandoc.Plain{pandoc.Str("stringify: " .. run(test_stringify))},
    pandoc.Plain{pandoc.Str("to_roman_numeral: " .. run(test_to_roman_numeral))},
  }
end
