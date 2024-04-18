--
-- Tests for the pandoc.log module
--
-- =========================================
-- PLEASE BE CAREFUL WHEN UPDATING THE TESTS
-- =========================================
--
-- Some tests here are very, very fragile, as their correctness depends on the
-- correct line number in this file.
local log = require 'pandoc.log'
local json = require 'pandoc.json'
local tasty = require 'tasty'

local group = tasty.test_group
local test = tasty.test_case
local assert = tasty.assert

return {
  group 'info' {
    test('is a function', function ()
           assert.are_equal(type(log.info), 'function')
    end),
    test('reports a warning', function ()
      log.info('info test')
      local msg = json.decode(json.encode(PANDOC_STATE.log[1]))
      assert.are_equal(msg.message, 'info test')
      assert.are_equal(msg.type, 'ScriptingInfo')
    end),
    test('info includes the correct number', function ()
      log.info('line number test')
      local msg = json.decode(json.encode(PANDOC_STATE.log[1]))
      -- THIS NEEDS UPDATING if lines above are shifted.
      assert.are_equal(msg.line, 30)
    end),
  },

  group 'warn' {
    test('is a function', function ()
      assert.are_equal(type(log.warn), 'function')
    end),
    test('reports a warning', function ()
      log.warn('testing')
      local msg = json.decode(json.encode(PANDOC_STATE.log[1]))
      assert.are_equal(msg.message, 'testing')
      assert.are_equal(msg.type, 'ScriptingWarning')
    end),
  },

  group 'silence' {
    test('prevents info from being logged', function ()
      local current_messages = PANDOC_STATE.log
      log.silence(log.info, 'Just so you know')
      assert.are_same(#current_messages, #PANDOC_STATE.log)
      for i = 1, #current_messages do
        assert.are_equal(
          json.encode(current_messages[i]),
          json.encode(PANDOC_STATE.log[i])
        )
      end
    end),
    test('returns the messages raised by the called function', function ()
           local msgs = log.silence(log.info, 'Just so you know')
           local msg = json.decode(json.encode(msgs[1]))
           assert.are_equal(msg.message, 'Just so you know')
    end),
    test('returns function application results as additional return values',
         function ()
           local l, x, y = log.silence(function (a, b) return b, a + b end, 5, 8)
           assert.are_same(l, {})
           assert.are_equal(x, 8)
        assert.are_equal(y, 13)
      end
    )
  }
}
