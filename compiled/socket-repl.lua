-- @noindex
local sok = require("reaper-socket")
local json = require("dkjson")
local fnl = require("fennel")
u = require("pb-utils")
ru = require("reaper-utils")
local r = reaper
local log = ru.misc.log
local udp = assert(sok.udp())
local udp_out = assert(sok.udp())
local function setup_udp(_1_)
  local _arg_2_ = _1_
  local _arg_3_ = _arg_2_["ports"]
  local input = _arg_3_["input"]
  local output = _arg_3_["output"]
  udp:setoption("reuseaddr", true)
  assert(udp:setsockname("127.0.0.1", input))
  assert(udp_out:setpeername("127.0.0.1", output))
  return ru.misc.log(("setting udp sockets:\n" .. "in: localhost:" .. input .. "\nout: localhost:" .. output .. "\n\nReapl server running !\n"))
end
local function wrap_repl(options)
  local repl_complete = nil
  local ret = nil
  local function send()
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
      ret = {error = {type = e_type, message = e, src = lua_src}}
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
  local function _5_(x)
    return coroutine.resume(co, x)
  end
  repl_send = _5_
  local repl_close
  local function _6_()
    return coroutine.close(co)
  end
  repl_close = _6_
  repl_send()
  return repl_send, repl_complete, repl_close
end
local function error_handler(sok0, error_type)
  local function _7_(e)
    return sok0:send(json.encode({error = {type = error_type, message = e}}, {}))
  end
  return _7_
end
local repl_ops = {"eval", "complete", "doc", "reload", "find", "compile", "apropos", "apropos-doc", "apropos-show-docs"}
local function repl_fn(_8_)
  local _arg_9_ = _8_
  local debug = _arg_9_["debug"]
  local send, comp, close = wrap_repl()
  local function repl()
    udp:settimeout(0.0001)
    local m = udp:receive()
    if m then
      if debug then
        log(m)
      else
      end
      local _let_11_ = json.decode(m)
      local opts = _let_11_
      local op = _let_11_["op"]
      local arg = _let_11_["arg"]
      if (op == "eval") then
        local function _12_()
          local ok_3f, ret = send(arg)
          local function _13_()
            return udp_out:send(json.encode({op = op, expression = arg, output = ret}, {}))
          end
          return xpcall(_13_, error_handler(udp_out, "encode"))
        end
        xpcall(_12_, error_handler(udp_out, "eval"))
      elseif (op == "complete") then
        local function _16_()
          local completions = comp(arg)
          local types
          do
            local tbl_14_auto = {}
            for _, v in ipairs(completions) do
              local k_15_auto, v_16_auto = nil, nil
              do
                local ok_3f, ret = send(("(type " .. v .. ")"))
                local function _14_()
                  if (ok_3f and ret) then
                    return ret.values[1]
                  else
                    return "unknown"
                  end
                end
                k_15_auto, v_16_auto = v, _14_()
              end
              if ((k_15_auto ~= nil) and (v_16_auto ~= nil)) then
                tbl_14_auto[k_15_auto] = v_16_auto
              else
              end
            end
            types = tbl_14_auto
          end
          return json.encode({op = op, symbol = arg, completions = completions, types = types}, {})
        end
        udp_out:send(_16_())
      else
        local _ = op
        local function _17_(_241)
          return (op == _241)
        end
        if u.seq.find(repl_ops, _17_) then
          local _0 = log(("," .. op .. " " .. arg))
          local ok_3f, ret = send(",doc collect")
          local _18_
          if (ok_3f and ret) then
            _18_ = ret
          else
            _18_ = {error = {type = "repl:op", message = (ret or "op fail...")}}
          end
          udp_out:send(json.encode(u.tbl.merge(opts, {output = _18_})))
        else
          udp_out:send(json.encode({error = {type = "unknow-op", message = ("Reapl: '" .. op .. "' not supported.")}}))
        end
      end
    else
    end
    return reaper.defer(repl)
  end
  return repl
end
local function start_repl(_23_)
  local _arg_24_ = _23_
  local options = _arg_24_
  local ports = _arg_24_["ports"]
  setup_udp(options)
  return repl_fn(options)()
end
return start_repl
