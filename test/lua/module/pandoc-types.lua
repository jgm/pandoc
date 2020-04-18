local tasty = require 'tasty'
local types = require 'pandoc.types'
local Version = types.Version

local assert = tasty.assert
local test = tasty.test_case
local group = tasty.test_group

return {
  group 'Version' {

    group 'constructor' {
      test('has type `userdata`', function ()
        assert.are_same(type(Version {2}), 'userdata')
      end),
      test('accepts list of integers', function ()
        assert.are_same(type(Version {2, 7, 3}), 'userdata')
      end),
      test('accepts a single integer', function ()
        assert.are_same(Version(5), Version {5})
      end),
      test('accepts version as string', function ()
        assert.are_same(
          Version '4.45.1',
          Version {4, 45, 1}
        )
      end),
      test('non-version string is rejected', function ()
        local success, msg = pcall(function () Version '11friends' end)
        assert.is_falsy(success)
        assert.is_truthy(tostring(msg):match('11friends'))
      end)
    },

    group 'comparison' {
      test('smaller (equal) than', function ()
        assert.is_truthy(Version {2, 58, 3} < Version {2, 58, 4})
        assert.is_falsy(Version {2, 60, 1} < Version {2, 59, 2})
        assert.is_truthy(Version {0, 14, 3} < Version {0, 14, 3, 1})
        assert.is_truthy(Version {3, 58, 3} <= Version {4})
        assert.is_truthy(Version {0, 14, 3} <= Version {0, 14, 3, 1})
      end),
      test('larger (equal) than', function ()
        assert.is_truthy(Version{2,58,3} > Version {2, 57, 4})
        assert.is_truthy(Version{2,58,3} > Version {2, 58, 2})
        assert.is_truthy(Version {0, 8} >= Version {0, 8})
        assert.is_falsy(Version {0, 8} >= Version {0, 8, 2})
      end),
      test('equality', function ()
        assert.is_truthy(Version '8.8', Version {8, 8})
      end),
      test('second argument can be a version string', function ()
        assert.is_truthy(Version '8' < '9.1')
        assert.is_falsy(Version '8.8' < '8.7')
      end),
    },

    group 'list-like behavior' {
      test('can access version component numbers', function ()
        local version = Version '2.7.3'
        assert.is_nil(version[0])
        assert.are_equal(version[1], 2)
        assert.are_equal(version[2], 7)
        assert.are_equal(version[3], 3)
      end),
      test('can be iterated over', function ()
        local version_list = {2, 7, 3}
        local final_index = 0
        for i, v in pairs(Version(version_list)) do
          assert.are_equal(v, version_list[i])
          final_index = i
        end
        assert.are_equal(final_index, 3)
      end),
      test('length is the number of components', function ()
        assert.are_equal(#(Version '0'), 1)
        assert.are_equal(#(Version '1.6'), 2)
        assert.are_equal(#(Version '8.7.5'), 3)
        assert.are_equal(#(Version '2.9.1.5'), 4)
      end)
    },

    group 'conversion to string' {
      test('converting from and to string is a noop', function ()
        local version_string = '1.19.4'
        assert.are_equal(tostring(Version(version_string)), version_string)
      end)
    },

    group 'convenience functions' {
      test('throws error if version is too old', function ()
        local actual = Version {2, 8}
        local expected = Version {2, 9}
        assert.error_matches(
          function () actual:must_be_at_least(expected) end,
          'expected version 2.9 or newer, got 2.8'
        )
      end),
      test('does nothing if expected version is older than actual', function ()
        local actual = Version '2.9'
        local expected = Version '2.8'
        actual:must_be_at_least(expected)
      end),
      test('does nothing if expected version equals to actual', function ()
        local actual = Version '2.8'
        local expected = Version '2.8'
        actual:must_be_at_least(expected)
      end)
    }
  }
}
