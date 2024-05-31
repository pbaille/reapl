-- @noindex
local sok = require("reaper-socket")
local bencode = require("bencode")
local json = require("dkjson")
local fnl = require("fennel")
u = require("pb-utils")
ru = require("reaper-utils")
local _local_1_ = ru
local _local_2_ = _local_1_["misc"]
local log = _local_2_["log"]
local r = reaper
local udp = assert(sok.udp())
local udp_out = assert(sok.udp())
local function setup_udp(in_port, out_port)
  udp:setoption("reuseaddr", true)
  assert(udp:setsockname("127.0.0.1", in_port))
  assert(udp_out:setpeername("127.0.0.1", out_port))
  return log(("setting udp sockets:\n" .. "in: " .. in_port .. "\nout-port: " .. out_port .. "\n\nWelcome to fennel repl !\n"))
end
local function log_as_error(err)
  return log(("error:\n\n" .. (err or "nil")))
end
local function send_back_as_error(e)
  return udp_out:send(json.encode({error = e}, {}))
end
local function repl()
  udp:settimeout(0.0001)
  local m = udp:receive()
  if m then
    local _let_3_ = bencode.decode(m)
    local code = _let_3_["code"]
    local compiled = _let_3_["compiled"]
    local no_return = _let_3_["no-return"]
    local compiled0 = (compiled or fnl["compile-string"](code))
    local f, err = load(compiled0)
    local success, ret = pcall(f)
    log(("__________\n\n>> " .. code .. "\n"))
    if success then
      log(ret)
      local function _4_()
        return udp_out:send(json.encode(ret, {}))
      end
      do local _ = (no_return or xpcall(_4_, send_back_as_error)) end
    else
      log_as_error(ret)
      do local _ = (no_return or send_back_as_error(ret)) end
    end
  else
  end
  return reaper.defer(repl)
end
local function start_repl(_7_)
  local _arg_8_ = _7_
  local socketPort = _arg_8_["socketPort"]
  local peerPort = _arg_8_["peerPort"]
  setup_udp(socketPort, peerPort)
  return repl()
end
return start_repl
