-- This Lua script is run every time the Lua interpreter is started when running
-- a Lua filter. It can be customized to load additional modules or to alter the
-- default modules.

pandoc = require 'pandoc'
pandoc.mediabag = require 'pandoc.mediabag'
pandoc.utils = require 'pandoc.utils'
