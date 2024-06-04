-- @noindex
local sok = require("reaper-socket")
local bencode = require("bencode")
local json = require("dkjson")
local fnl = require("fennel")
u = require("pb-utils")
ru = require("reaper-utils")
local r = reaper
local udp = assert(sok.udp())
local udp_out = assert(sok.udp())
local function setup_udp(_1_)
  local _arg_2_ = _1_
  local socketPort = _arg_2_["socketPort"]
  local peerPort = _arg_2_["peerPort"]
  local noConsole = _arg_2_["noConsole"]
  udp:setoption("reuseaddr", true)
  assert(udp:setsockname("127.0.0.1", socketPort))
  assert(udp_out:setpeername("127.0.0.1", peerPort))
  if not noConsole then
    return ru.misc.log(("setting udp sockets:\n" .. "in: localhost:" .. __fnl_global__in_2dport .. "\nout: localhost:" .. __fnl_global__out_2dport .. "\n\nWelcome to fennel repl !\n"))
  else
    return nil
  end
end
local function send_back_as_error(e)
  return udp_out:send(json.encode({error = e}, {}))
end
local function repl_fn(_4_)
  local _arg_5_ = _4_
  local noConsole = _arg_5_["noConsole"]
  local log
  if noConsole then
    local function _6_(_)
    end
    log = _6_
  else
    log = ru.misc.log
  end
  local log_as_error
  local function _8_(err)
    return log(("error:\n\n" .. (err or "nil")))
  end
  log_as_error = _8_
  local function repl()
    udp:settimeout(0.0001)
    local m = udp:receive()
    if m then
      local _let_9_ = bencode.decode(m)
      local code = _let_9_["code"]
      local compiled = _let_9_["compiled"]
      local no_return = _let_9_["no-return"]
      local compiled_3f, ret = nil, nil
      local function _10_()
        return (compiled or fnl["compile-string"](code))
      end
      compiled_3f, ret = pcall(_10_)
      log(("__________\n\n>> " .. code .. "\n"))
      if not compiled_3f then
        log_as_error(ret)
        do local _ = (no_return or send_back_as_error(ret)) end
      else
        local f, err = load(ret)
        local success, ret0 = pcall(f)
        if success then
          log(ret0)
          local function _11_()
            return udp_out:send(json.encode(ret0, {}))
          end
          do local _ = (no_return or xpcall(_11_, send_back_as_error)) end
        else
          log_as_error(ret0)
          do local _ = (no_return or send_back_as_error(ret0)) end
        end
      end
    else
    end
    return reaper.defer(repl)
  end
  return repl
end
local function start_repl(_15_)
  local _arg_16_ = _15_
  local options = _arg_16_
  local socketPort = _arg_16_["socketPort"]
  local peerPort = _arg_16_["peerPort"]
  local noConsole = _arg_16_["noConsole"]
  setup_udp(options)
  return repl_fn(options)()
end
return start_repl
