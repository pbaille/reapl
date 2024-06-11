-- @noindex
local opsys = reaper.GetOS()
local extension
if opsys:match('Win') then
  extension = 'dll'
else -- Linux and Macos
  extension = 'so'
end

local info = debug.getinfo(1, 'S');
local script_path = info.source:match("^@?(.+)/[^/]+/[^/]+$")
package.cpath = script_path .. "/lib/socket module/?."..extension .. ";" .. package.cpath
package.path = script_path .. "/lib/?.lua" .. ";" .. script_path .. "/compiled/?.lua" .. ";" .. package.path

local reapl = require('reapl')

return reapl({ ports = {input= 9999, output= 9997}})
