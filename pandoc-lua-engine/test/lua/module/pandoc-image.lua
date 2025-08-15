--
-- Tests for the system module
--
local image = require 'pandoc.image'
local tasty = require 'tasty'

local group = tasty.test_group
local test = tasty.test_case
local assert = tasty.assert

local svg_image = [==[<?xml version="1.0"?>
<svg xmlns="http://www.w3.org/2000/svg"
     xmlns:xlink="http://www.w3.org/1999/xlink"
     height="70" width="70"
     viewBox="-35 -35 70 70">
  <title>test</title>
  <!-- document shape -->
  <polygon points="-10,-31.53 -10,-3.25 0,0 10,-3.25 10,-23.53 2,-31.53" />
</svg>
]==]

return {
  -- Check existence of static fields
  group 'static fields' {
  },

  group 'size' {
    test('returns a table', function ()
      local imgsize = {
        width = 70,
        height = 70,
        dpi_horz = 96,
        dpi_vert = 96,
      }
      assert.are_same(image.size(svg_image), imgsize)
    end),
    test('fails on faulty eps', function ()
      assert.error_matches(
        function () image.size('%!PS EPSF') end,
        'could not determine EPS size'
      )
    end),
    test('fails if input is not an image', function ()
      assert.error_matches(
        function () image.size('not an image') end,
        'could not determine image type'
      )
    end),
    test('respects the dpi setting', function ()
      local imgsize = {
        width = 70,
        height = 70,
        dpi_horz = 300,
        dpi_vert = 300,
      }
      assert.are_same(image.size(svg_image, {dpi=300}), imgsize)
    end),
  },

  group 'format' {
    test('SVG', function ()
      assert.are_equal(image.format(svg_image), 'svg')
    end),
    test('returns nil if input is not an image', function ()
      assert.is_nil(image.format('not an image'))
    end),
  },
}
