-- @noindex
local json = require("dkjson")
local sok = require("socket")
local BEG_DEL = "<UDP|"
local END_DEL = "|UDP>"
local function last_chunk_3f(s)
  return (END_DEL == s:sub(( - #END_DEL)))
end
pcall(function() require("fennel").metadata:setall(last_chunk_3f, "fnl/arglist", {"s"}) end)
local function first_chunk_3f(s)
  return (BEG_DEL == s:sub(1, #BEG_DEL))
end
pcall(function() require("fennel").metadata:setall(first_chunk_3f, "fnl/arglist", {"s"}) end)
local function chunks_to_string(xs)
  local ret = ""
  for i, v in ipairs(xs) do
    ret = (ret .. v)
  end
  ret = ret:sub((1 + #BEG_DEL), ( - (1 + #END_DEL)))
  return ret
end
pcall(function() require("fennel").metadata:setall(chunks_to_string, "fnl/arglist", {"xs"}) end)
local function decode_chunks(xs, on_success, on_error)
  local str = chunks_to_string(xs)
  local _1_, _2_, _3_ = json.decode(str)
  if ((nil ~= _1_) and true and (_3_ == nil)) then
    local ret = _1_
    local _ = _2_
    return on_success(ret)
  elseif ((_1_ == nil) and true and (nil ~= _3_)) then
    local _ = _2_
    local e = _3_
    return on_error({type = "encode", error = e, message = str})
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(decode_chunks, "fnl/arglist", {"xs", "on-success", "on-error"}) end)
local function listen(socket, on_success, on_error)
  local input = nil
  socket:settimeout(0.0001)
  local function loop()
    local chunk = socket:receive()
    if chunk then
      if first_chunk_3f(chunk) then
        input = {chunk}
      else
        if input then
          table.insert(input, chunk)
        else
        end
      end
      if (input and last_chunk_3f(chunk)) then
        decode_chunks(input, on_success, on_error)
      else
      end
      return loop()
    else
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(loop, "fnl/arglist", {}) end)
  return loop
end
pcall(function() require("fennel").metadata:setall(listen, "fnl/arglist", {"socket", "on-success", "on-error"}) end)
--[[ (do (local sok1 (assert (sok.udp))) (local sok2 (assert (sok.udp))) (sok1:setoption "reuseaddr" true) (sok2:setoption "reuseaddr" true) (assert (sok1:setsockname "127.0.0.1" "55551")) (assert (sok2:setpeername "127.0.0.1" "8083")) (local view (require "fennel.view")) (local co (listen sok1 (fn [m] (print (view {:got m}))) (fn [e] (print (view {:err e}))))) (local go (listen sok1 (fn [m] (print (view {:got m}))) (fn [e] (print (view {:err e}))))) (go) (sok2:send "<UDP|{\"io\": [") (sok2:send "1,2]}|UDP>") (sok2:send "|UDP>") (sok2:send "<UDP|{\"hey\": 3|UDP>") (coroutine.resume co) (comment (+ 1 2) (sok1:settimeout 0.0001) (sok1:receive) (coroutine.close co) (let [(ret _ e) (json.decode "{\"io\": [1]}")] [ret e]) (let [(ret _ e) (json.decode "{\"io\": [1]")] [ret e]))) ]]
return nil
