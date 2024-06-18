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
local action_socket = assert(sok.udp())
local function setup_udp(_3_)
  local _arg_4_ = _3_
  local _arg_5_ = _arg_4_["ports"]
  local input = _arg_5_["input"]
  local output = _arg_5_["output"]
  local action = _arg_5_["action"]
  input_socket:setoption("reuseaddr", true)
  input_socket:setoption("send-buffer-size", 32000)
  action_socket:setoption("reuseaddr", true)
  action_socket:settimeout(0.0001)
  assert(action_socket:setsockname("127.0.0.1", action))
  assert(input_socket:setsockname("127.0.0.1", input))
  assert(output_socket:setpeername("127.0.0.1", output))
  return ru.misc.log(("setting udp sockets:\n" .. "in: localhost:" .. input .. "\nout: localhost:" .. output .. "\naction: localhost:" .. action .. "\n\nReapl server running !\n"))
end
pcall(function() require("fennel").metadata:setall(setup_udp, "fnl/arglist", {"{:ports {:action action :input input :output output}}"}) end)
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
local function string_chunk(s, n)
  local ret = {}
  for i = 1, #s, n do
    table.insert(ret, s:sub(i, ((i + n) - 1)))
  end
  return ret
end
pcall(function() require("fennel").metadata:setall(string_chunk, "fnl/arglist", {"s", "n"}) end)
local function respond(opts, data)
  local res = u.tbl.merge(opts, {data = data})
  local encoded = encode(res)
  local len = #encoded
  if (len > 32000) then
    local chunks = string_chunk(encode(data), 8000)
    for _, chunk in ipairs(chunks) do
      output_socket:send(encode({id = opts.id, data = chunk}))
    end
    return output_socket:send(encode({id = opts.id, op = opts.op}))
  else
    return output_socket:send(encoded)
  end
end
pcall(function() require("fennel").metadata:setall(respond, "fnl/arglist", {"opts", "data"}) end)
local function error_handler(opts, error_type)
  local function _9_(e)
    return respond(opts, {error = {type = error_type, message = e}})
  end
  pcall(function() require("fennel").metadata:setall(_9_, "fnl/arglist", {"e"}) end)
  return _9_
end
pcall(function() require("fennel").metadata:setall(error_handler, "fnl/arglist", {"opts", "error-type"}) end)
local function fennel_type_error__3ekind(e)
  local function _10_()
    if string.find(e, "tried to reference a special form") then
      return "keyword"
    elseif string.find(e, "tried to reference a macro") then
      return "macro"
    else
      return "unknown"
    end
  end
  return (e and _10_())
end
pcall(function() require("fennel").metadata:setall(fennel_type_error__3ekind, "fnl/arglist", {"e"}) end)
local function action_step(port)
  local function _11_()
    local m = action_socket:receive()
    if m then
      dbg({"got action ", m})
      local f
      do
        local t_12_ = actions.dispatch
        if (nil ~= t_12_) then
          t_12_ = t_12_[m]
        else
        end
        f = t_12_
      end
      if f then
        return f()
      else
        return nil
      end
    else
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(_11_, "fnl/arglist", {}) end)
  return _11_
end
pcall(function() require("fennel").metadata:setall(action_step, "fnl/arglist", {"port"}) end)
local function repl_step()
  local _let_16_ = require("simple-repl")
  local complete = _let_16_["complete"]
  local eval = _let_16_["eval"]
  local ops = _let_16_
  local _let_17_ = require("udp-utils")
  local listen = _let_17_["listen"]
  local function _20_(_18_)
    local _arg_19_ = _18_
    local opts = _arg_19_
    local op = _arg_19_["op"]
    local data = _arg_19_["data"]
    dbg({"in", opts})
    if (op == "do") then
      local function _21_()
        return eval(data)
      end
      pcall(function() require("fennel").metadata:setall(_21_, "fnl/arglist", {}) end)
      return xpcall(_21_, error_handler(opts, "do"))
    elseif (op == "eval") then
      local function _22_()
        return respond(opts, eval(data))
      end
      pcall(function() require("fennel").metadata:setall(_22_, "fnl/arglist", {}) end)
      return xpcall(_22_, error_handler(opts, "eval"))
    elseif (op == "complete") then
      local completions = complete(data)
      local types
      do
        local tbl_14_auto = {}
        for _, v in ipairs(completions) do
          local k_15_auto, v_16_auto = nil, nil
          do
            local result = eval(("(type " .. v .. ")"))
            local function _24_()
              local t_23_ = result
              if (nil ~= t_23_) then
                t_23_ = t_23_.error
              else
              end
              if (nil ~= t_23_) then
                t_23_ = t_23_.message
              else
              end
              return t_23_
            end
            k_15_auto, v_16_auto = v, (result.value or fennel_type_error__3ekind(_24_()))
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
      local _29_
      do
        local t_28_ = ops
        if (nil ~= t_28_) then
          t_28_ = t_28_[op]
        else
        end
        _29_ = t_28_
      end
      if _29_ then
        return respond(opts, ops[op](data))
      else
        return respond(opts, {error = {type = "unknow-op", message = ("Reapl: '" .. op .. "' not supported.")}})
      end
    end
  end
  pcall(function() require("fennel").metadata:setall(_20_, "fnl/arglist", {"{:data data :op op &as opts}"}) end)
  return listen(input_socket, _20_)
end
pcall(function() require("fennel").metadata:setall(repl_step, "fnl/arglist", {}) end)
local function start_repl(_33_)
  local _arg_34_ = _33_
  local options = _arg_34_
  local ports = _arg_34_["ports"]
  setup_udp(options)
  local action_step0 = action_step(ports.action)
  local repl_step0 = repl_step()
  local loop
  local function rec()
    action_step0()
    repl_step0()
    return reaper.defer(rec)
  end
  pcall(function() require("fennel").metadata:setall(rec, "fnl/arglist", {}) end)
  loop = rec
  return loop()
end
pcall(function() require("fennel").metadata:setall(start_repl, "fnl/arglist", {"{:ports ports &as options}"}) end)
return start_repl
