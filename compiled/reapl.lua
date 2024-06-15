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
local input_socket = assert(sok.udp())
local output_socket = assert(sok.udp())
local function setup_udp(_3_)
  local _arg_4_ = _3_
  local _arg_5_ = _arg_4_["ports"]
  local input = _arg_5_["input"]
  local output = _arg_5_["output"]
  input_socket:setoption("reuseaddr", true)
  input_socket:setoption("send-buffer-size", 32000)
  assert(input_socket:setsockname("127.0.0.1", input))
  assert(output_socket:setpeername("127.0.0.1", output))
  return ru.misc.log(("setting udp sockets:\n" .. "in: localhost:" .. input .. "\nout: localhost:" .. output .. "\n\nReapl server running !\n"))
end
pcall(function() require("fennel").metadata:setall(setup_udp, "fnl/arglist", {"{:ports {:input input :output output}}"}) end)
local repl_ops = {"eval", "complete", "doc", "reload", "find", "compile", "apropos", "apropos-doc", "apropos-show-docs"}
local function encode(data)
  local function deep_encode_fn(t)
    local _6_ = type(t)
    if (_6_ == "function") then
      return "#<function>"
    elseif (_6_ == "table") then
      local result = {}
      for k, v in pairs(t) do
        result[k] = deep_encode_fn(v)
      end
      return result
    else
      local _ = _6_
      return t
    end
  end
  pcall(function() require("fennel").metadata:setall(deep_encode_fn, "fnl/arglist", {"t"}) end)
  return json.encode(deep_encode_fn(data), {})
end
pcall(function() require("fennel").metadata:setall(encode, "fnl/arglist", {"data"}) end)
local function respond(opts, output)
  local res = u.tbl.merge(opts, {output = output})
  dbg({"out", res})
  return output_socket:send(encode(res))
end
pcall(function() require("fennel").metadata:setall(respond, "fnl/arglist", {"opts", "output"}) end)
local function error_handler(opts, error_type)
  local function _8_(e)
    return respond(opts, {error = {type = error_type, message = e}})
  end
  pcall(function() require("fennel").metadata:setall(_8_, "fnl/arglist", {"e"}) end)
  return _8_
end
pcall(function() require("fennel").metadata:setall(error_handler, "fnl/arglist", {"opts", "error-type"}) end)
local function fennel_type_error__3ekind(e)
  local function _9_()
    if string.find(e, "tried to reference a special form") then
      return "keyword"
    elseif string.find(e, "tried to reference a macro") then
      return "macro"
    else
      return "unknown"
    end
  end
  return (e and _9_())
end
pcall(function() require("fennel").metadata:setall(fennel_type_error__3ekind, "fnl/arglist", {"e"}) end)
local function repl_fn()
  local _let_10_ = require("simple-repl")
  local complete = _let_10_["complete"]
  local eval = _let_10_["eval"]
  local ops = _let_10_
  local _let_11_ = require("udp-utils")
  local listen = _let_11_["listen"]
  local flush
  local function _14_(_12_)
    local _arg_13_ = _12_
    local opts = _arg_13_
    local op = _arg_13_["op"]
    local data = _arg_13_["data"]
    dbg({"in", opts})
    if (op == "eval") then
      local function _15_()
        return respond(opts, eval(data))
      end
      pcall(function() require("fennel").metadata:setall(_15_, "fnl/arglist", {}) end)
      return xpcall(_15_, error_handler(opts, "eval"))
    elseif (op == "complete") then
      local completions = complete(data)
      local types
      do
        local tbl_14_auto = {}
        for _, v in ipairs(completions) do
          local k_15_auto, v_16_auto = nil, nil
          do
            local result = eval(("(type " .. v .. ")"))
            local function _17_()
              local t_16_ = result
              if (nil ~= t_16_) then
                t_16_ = t_16_.error
              else
              end
              if (nil ~= t_16_) then
                t_16_ = t_16_.message
              else
              end
              return t_16_
            end
            k_15_auto, v_16_auto = v, (result.value or fennel_type_error__3ekind(_17_()))
          end
          if ((k_15_auto ~= nil) and (v_16_auto ~= nil)) then
            tbl_14_auto[k_15_auto] = v_16_auto
          else
          end
        end
        types = tbl_14_auto
      end
      return respond(opts, {completions = completions, types = types})
    else
      local _ = op
      local _22_
      do
        local t_21_ = ops
        if (nil ~= t_21_) then
          t_21_ = t_21_[op]
        else
        end
        _22_ = t_21_
      end
      if _22_ then
        return respond(opts, ops[op](data))
      else
        return respond(opts, {error = {type = "unknow-op", message = ("Reapl: '" .. op .. "' not supported.")}})
      end
    end
  end
  pcall(function() require("fennel").metadata:setall(_14_, "fnl/arglist", {"{:data data :op op &as opts}"}) end)
  flush = listen(input_socket, _14_)
  local function repl()
    flush()
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
