-- @noindex
local json = require("dkjson")
local function listen(socket, handler)
  local input = {data = ""}
  local function reset_input()
    input = {data = ""}
    return nil
  end
  pcall(function() require("fennel").metadata:setall(reset_input, "fnl/arglist", {}) end)
  socket:settimeout(0.0001)
  local function loop()
    local m = socket:receive()
    if m then
      local _let_1_ = json.decode(m)
      local json_message = _let_1_
      local id = _let_1_["id"]
      local op = _let_1_["op"]
      local data = _let_1_["data"]
      if id then
        if not (input.id == id) then
          reset_input()
        else
        end
        if (data and not op) then
          input = {id = id, data = (input.data .. data)}
          return loop()
        else
          if (input.id == id) then
            json_message["data"] = input.data
          else
          end
          local ret = handler(json_message)
          reset_input()
          return ret
        end
      else
        return nil
      end
    else
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(loop, "fnl/arglist", {}) end)
  return loop
end
pcall(function() require("fennel").metadata:setall(listen, "fnl/arglist", {"socket", "handler"}) end)
--[[ (local sok (require "socket")) (local sok1 (assert (sok.udp))) (local sok2 (assert (sok.udp))) (assert (sok1:setsockname "127.0.0.1" "55551")) (assert (sok2:setpeername "127.0.0.1" "55551")) (local go (listen sok1 (fn [{:data data :op op}] (case op "print" (do (print data) [data data]) _ "pouetpouet")))) (go) (sok2:send (json.encode {:data "hello " :id 1})) (sok2:send (json.encode {:id 1 :op "print"})) (sok2:send (json.encode {:data "pouet" :id 2 :op "print"})) (local sok3 (assert (sok.udp))) (assert (sok3:setpeername "127.0.0.1" "8088")) (sok3:send (json.encode {:data "hello " :id 1})) (sok3:send (json.encode {:id 1 :op "print"})) ]]
return {listen = listen}
