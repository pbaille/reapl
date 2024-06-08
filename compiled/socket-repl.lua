-- @noindex
local sok = require("reaper-socket")
local json = require("dkjson")
local fnl = require("fennel")
u = require("pb-utils")
ru = require("reaper-utils")
__fnl_global__reascript_2ddoc = require("reascript-doc")
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
pcall(function() require("fennel").metadata:setall(setup_udp, "fnl/arglist", {"{:ports {:input input :output output}}"}) end)
local result = nil
local debug = true
local function dbg(x)
  if debug then
    return log(fnl.view(x))
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(dbg, "fnl/arglist", {"x"}) end)
local function wrap_repl(options)
  local repl_complete = nil
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
    opts.readChunk = function(x)
      dbg({"readChunk", x})
      return coroutine.yield(x)
    end
    pcall(function() require("fennel").metadata:setall(opts.readChunk, "fnl/arglist", {"x"}) end)
    opts.onValues = function(x)
      dbg({"onValues", x})
      result = {values = x}
      return nil
    end
    pcall(function() require("fennel").metadata:setall(opts.onValues, "fnl/arglist", {"x"}) end)
    opts.onError = function(e_type, e, lua_src)
      dbg({"onError", {["e-type"] = e_type, e = e, ["lua-src"] = lua_src}})
      result = {error = {type = e_type, message = e, src = lua_src}}
      return nil
    end
    pcall(function() require("fennel").metadata:setall(opts.onError, "fnl/arglist", {"e-type", "e", "lua-src"}) end)
    opts.registerCompleter = function(x)
      repl_complete = x
      return nil
    end
    pcall(function() require("fennel").metadata:setall(opts.registerCompleter, "fnl/arglist", {"x"}) end)
    opts.pp = function(x)
      return x
    end
    pcall(function() require("fennel").metadata:setall(opts.pp, "fnl/arglist", {"x"}) end)
    opts["error-pinpoint"] = {"\194\171", "\194\187"}
    return fnl.repl(opts)
  end
  pcall(function() require("fennel").metadata:setall(send, "fnl/arglist", {}) end)
  local repl_send = coroutine.wrap(send)
  repl_send()
  return repl_send, repl_complete, ret
end
pcall(function() require("fennel").metadata:setall(wrap_repl, "fnl/arglist", {"options"}) end)
local function error_handler(command, error_type)
  local function _6_(e)
    return udp_out:send(json.encode(u.tbl.merge(command, {error = {type = error_type, message = e}}), {}))
  end
  pcall(function() require("fennel").metadata:setall(_6_, "fnl/arglist", {"e"}) end)
  return _6_
end
pcall(function() require("fennel").metadata:setall(error_handler, "fnl/arglist", {"command", "error-type"}) end)
local repl_ops = {"eval", "complete", "doc", "reload", "find", "compile", "apropos", "apropos-doc", "apropos-show-docs"}
local function repl_fn()
  local send, comp = wrap_repl()
  local function repl()
    udp:settimeout(0.0001)
    local m = udp:receive()
    if m then
      result = nil
      dbg({"input", m})
      local _let_7_ = json.decode(m)
      local opts = _let_7_
      local op = _let_7_["op"]
      local arg = _let_7_["arg"]
      if (op == "eval") then
        local function _8_()
          local _ = send(arg)
          dbg({"eval", result})
          local function _9_()
            return udp_out:send(json.encode({op = op, expression = arg, output = result}, {}))
          end
          pcall(function() require("fennel").metadata:setall(_9_, "fnl/arglist", {}) end)
          return xpcall(_9_, error_handler(opts, "encode"))
        end
        pcall(function() require("fennel").metadata:setall(_8_, "fnl/arglist", {}) end)
        xpcall(_8_, error_handler(opts, "eval"))
      elseif (op == "complete") then
        local function _14_()
          local completions = comp(arg)
          local types
          do
            local tbl_14_auto = {}
            for _, v in ipairs(completions) do
              local k_15_auto, v_16_auto = nil, nil
              do
                local _0 = send(("(type " .. v .. ")"))
                local function _12_()
                  if result then
                    local _10_ = result.values
                    if (nil ~= _10_) then
                      return _10_[1]
                    else
                      return _10_
                    end
                  else
                    return "unknown"
                  end
                end
                k_15_auto, v_16_auto = v, _12_()
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
        udp_out:send(_14_())
      else
        local _ = op
        local function _15_(_241)
          return (op == _241)
        end
        if u.seq.find(repl_ops, _15_) then
          local _0 = send(("," .. op .. " " .. arg .. "\n"))
          udp_out:send(json.encode(u.tbl.merge(opts, {output = result})))
        else
          udp_out:send(json.encode({error = {type = "unknow-op", message = ("Reapl: '" .. op .. "' not supported.")}}))
        end
      end
    else
    end
    return reaper.defer(repl)
  end
  pcall(function() require("fennel").metadata:setall(repl, "fnl/arglist", {}) end)
  return repl
end
pcall(function() require("fennel").metadata:setall(repl_fn, "fnl/arglist", {}) end)
local function start_repl(_19_)
  local _arg_20_ = _19_
  local options = _arg_20_
  local ports = _arg_20_["ports"]
  setup_udp(options)
  return repl_fn()()
end
pcall(function() require("fennel").metadata:setall(start_repl, "fnl/arglist", {"{:ports ports &as options}"}) end)
return start_repl
