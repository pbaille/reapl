-- @noindex
local sok = require("reaper-socket")
local json = require("dkjson")
local fnl = require("fennel")
u = require("pb-utils")
ru = require("reaper-utils")
local r = reaper
local udp = assert(sok.udp())
local udp_eval_out = assert(sok.udp())
local udp_complete_out = assert(sok.udp())
local function setup_udp(_1_)
  local _arg_2_ = _1_
  local socketPort = _arg_2_["socketPort"]
  local evalPeerPort = _arg_2_["evalPeerPort"]
  local completePeerPort = _arg_2_["completePeerPort"]
  local noConsole = _arg_2_["noConsole"]
  udp:setoption("reuseaddr", true)
  assert(udp:setsockname("127.0.0.1", socketPort))
  assert(udp_eval_out:setpeername("127.0.0.1", evalPeerPort))
  assert(udp_complete_out:setpeername("127.0.0.1", completePeerPort))
  return ru.misc.log(("setting udp sockets:\n" .. "in: localhost:" .. socketPort .. "\neval-out: localhost:" .. evalPeerPort .. "\ncomplete-out: localhost:" .. completePeerPort .. "\n\nReapl server running !\n"))
end
local function wrap_repl(options)
  local repl_complete = nil
  local ret = nil
  local function send()
    local log = ru.misc.log
    local opts
    do
      local tbl_14_auto = {useMetadata = true}
      for k, x in pairs((options or {})) do
        local k_15_auto, v_16_auto = k, x
        if ((k_15_auto ~= nil) and (v_16_auto ~= nil)) then
          tbl_14_auto[k_15_auto] = v_16_auto
        else
        end
      end
      opts = tbl_14_auto
    end
    opts.readChunk = function()
      return coroutine.yield(ret)
    end
    opts.onValues = function(x)
      ret = {values = x}
      return nil
    end
    opts.onError = function(e_type, e, lua_src)
      ret = {error = {type = e_type, msg = e, src = lua_src}}
      return nil
    end
    opts.registerCompleter = function(x)
      repl_complete = x
      return nil
    end
    opts.pp = function(x)
      return x
    end
    opts["error-pinpoint"] = {"\194\171", "\194\187"}
    return fnl.repl(opts)
  end
  local co = coroutine.create(send)
  local repl_send
  local function _4_(x)
    return coroutine.resume(co, x)
  end
  repl_send = _4_
  local repl_close
  local function _5_()
    return coroutine.close(co)
  end
  repl_close = _5_
  repl_send()
  return repl_send, repl_complete, repl_close
end
local function error_handler(sok0, error_type)
  local function _6_(e)
    return sok0:send(json.encode({error = {type = error_type, message = e}}, {}))
  end
  return _6_
end
local function repl_fn(_7_)
  local _arg_8_ = _7_
  local noConsole = _arg_8_["noConsole"]
  local send, comp, close = wrap_repl()
  local function repl()
    udp:settimeout(0.0001)
    local m = udp:receive()
    if m then
      local _let_9_ = json.decode(m)
      local eval = _let_9_["eval"]
      local complete = _let_9_["complete"]
      if eval then
        local function _10_()
          local ok_3f, ret = send(eval)
          local function _11_()
            return udp_eval_out:send(json.encode({expression = eval, output = ret}, {}))
          end
          return xpcall(_11_, error_handler(udp_eval_out, "encode"))
        end
        xpcall(_10_, error_handler(udp_eval_out, "eval"))
      elseif complete then
        local function _12_()
          return udp_complete_out:send(json.encode({request = complete, completions = comp(complete)}, {}))
        end
        xpcall(_12_, error_handler(udp_complete_out, "encode"))
      else
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
  local evalPeerPort = _arg_16_["evalPeerPort"]
  local completePeerPort = _arg_16_["completePeerPort"]
  local noConsole = _arg_16_["noConsole"]
  setup_udp(options)
  return repl_fn(options)()
end
return start_repl
