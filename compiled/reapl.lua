-- @noindex
local sok = require("reaper-socket")
local json = require("dkjson")
fennel = require("fennel")
u = require("pb-utils")
ru = require("reaper-utils")
__fnl_global__reascript_2ddoc = require("reascript-doc")
actions = require("reaper-actions")
r = reaper
local function dbg(...)
  if debug then
    for _, x in ipairs({...}) do
      local function _1_(...)
        if ("string" == type(x)) then
          return x
        else
          return fennel.view(x)
        end
      end
      ru.misc.log(_1_(...))
    end
    return nil
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(dbg, "fnl/arglist", {"..."}) end)
local debug = true
local udp = assert(sok.udp())
local udp_out = assert(sok.udp())
local function setup_udp(_3_)
  local _arg_4_ = _3_
  local _arg_5_ = _arg_4_["ports"]
  local input = _arg_5_["input"]
  local output = _arg_5_["output"]
  udp:setoption("reuseaddr", true)
  assert(udp:setsockname("127.0.0.1", input))
  assert(udp_out:setpeername("127.0.0.1", output))
  return ru.misc.log(("setting udp sockets:\n" .. "in: localhost:" .. input .. "\nout: localhost:" .. output .. "\n\nReapl server running !\n"))
end
pcall(function() require("fennel").metadata:setall(setup_udp, "fnl/arglist", {"{:ports {:input input :output output}}"}) end)
local function error_handler(command, error_type)
  local function _6_(e)
    return udp_out:send(json.encode(u.tbl.merge(command, {output = {error = {type = error_type, message = e}}}), {}))
  end
  pcall(function() require("fennel").metadata:setall(_6_, "fnl/arglist", {"e"}) end)
  return _6_
end
pcall(function() require("fennel").metadata:setall(error_handler, "fnl/arglist", {"command", "error-type"}) end)
local repl_ops = {"eval", "complete", "doc", "reload", "find", "compile", "apropos", "apropos-doc", "apropos-show-docs"}
local function encode(data)
  local function deep_encode_fn(t)
    local _7_ = type(t)
    if (_7_ == "function") then
      return "#<function>"
    elseif (_7_ == "table") then
      local result = {}
      for k, v in pairs(t) do
        result[k] = deep_encode_fn(v)
      end
      return result
    else
      local _ = _7_
      return t
    end
  end
  pcall(function() require("fennel").metadata:setall(deep_encode_fn, "fnl/arglist", {"t"}) end)
  return json.encode(deep_encode_fn(data), {})
end
pcall(function() require("fennel").metadata:setall(encode, "fnl/arglist", {"data"}) end)
local function repl_fn()
  local _let_9_ = require("simple-repl")
  local complete = _let_9_["complete"]
  local eval = _let_9_["eval"]
  local ops = _let_9_
  local function repl()
    udp:settimeout(0.0001)
    local m = udp:receive()
    if m then
      dbg({"input", m})
      local _let_10_ = json.decode(m)
      local opts = _let_10_
      local op = _let_10_["op"]
      local arg = _let_10_["arg"]
      if (op == "eval") then
        local function _11_()
          local output = eval(arg)
          dbg({"eval", output})
          local function _12_()
            return udp_out:send(encode({op = op, arg = arg, output = output}))
          end
          pcall(function() require("fennel").metadata:setall(_12_, "fnl/arglist", {}) end)
          return xpcall(_12_, error_handler(opts, "encode"))
        end
        pcall(function() require("fennel").metadata:setall(_11_, "fnl/arglist", {}) end)
        xpcall(_11_, error_handler(opts, "eval"))
      elseif (op == "complete") then
        local function _19_()
          local completions = complete(arg)
          local types
          do
            local tbl_14_auto = {}
            for _, v in ipairs(completions) do
              local k_15_auto, v_16_auto = nil, nil
              do
                local result = eval(("(type " .. v .. ")"))
                local function _16_()
                  local e
                  do
                    local t_13_ = result
                    if (nil ~= t_13_) then
                      t_13_ = t_13_.error
                    else
                    end
                    if (nil ~= t_13_) then
                      t_13_ = t_13_.message
                    else
                    end
                    e = t_13_
                  end
                  local function _17_()
                    if string.find(e, "tried to reference a special form") then
                      return "keyword"
                    elseif string.find(e, "tried to reference a macro") then
                      return "macro"
                    else
                      return "unknown"
                    end
                  end
                  return (e and _17_())
                end
                k_15_auto, v_16_auto = v, (result.value or _16_())
              end
              if ((k_15_auto ~= nil) and (v_16_auto ~= nil)) then
                tbl_14_auto[k_15_auto] = v_16_auto
              else
              end
            end
            types = tbl_14_auto
          end
          return encode({op = op, symbol = arg, completions = completions, types = types})
        end
        udp_out:send(_19_())
      else
        local _ = op
        local _21_
        do
          local t_20_ = ops
          if (nil ~= t_20_) then
            t_20_ = t_20_[op]
          else
          end
          _21_ = t_20_
        end
        if _21_ then
          local output = ops[op](arg)
          dbg({"op", op, "output", output})
          udp_out:send(encode(u.tbl.merge(opts, {output = output})))
        else
          dbg("unknown op")
          udp_out:send(encode({error = {type = "unknow-op", message = ("Reapl: '" .. op .. "' not supported.")}}))
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
local function start_repl(_26_)
  local _arg_27_ = _26_
  local options = _arg_27_
  local ports = _arg_27_["ports"]
  setup_udp(options)
  return repl_fn()()
end
pcall(function() require("fennel").metadata:setall(start_repl, "fnl/arglist", {"{:ports ports &as options}"}) end)
return start_repl
