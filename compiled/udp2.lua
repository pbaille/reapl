-- @noindex
local json = require("dkjson")
local sok = require("socket")
local function listen(socket, ops)
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
          local f
          do
            local t_3_ = ops
            if (nil ~= t_3_) then
              t_3_ = t_3_[op]
            else
            end
            f = t_3_
          end
          local ret
          local function _5_()
            if (input.id == id) then
              return input.data
            else
              return data
            end
          end
          ret = f(_5_())
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
pcall(function() require("fennel").metadata:setall(listen, "fnl/arglist", {"socket", "ops"}) end)
--[[ (local sok1 (assert (sok.udp))) (local sok2 (assert (sok.udp))) (assert (sok1:setsockname "127.0.0.1" "55551")) (assert (sok2:setpeername "127.0.0.1" "55551")) (local go (listen sok1 {:print (fn [x] [x x])})) (go) (sok2:send (json.encode {:data "hello " :id 1})) (sok2:send (json.encode {:id 1 :op "print"})) (sok2:send (json.encode {:data "pouet" :id 2 :op "print"})) (local sok3 (assert (sok.udp))) (assert (sok3:setpeername "127.0.0.1" "8088")) (sok3:send (json.encode {:data "hello " :id 1})) (sok3:send (json.encode {:id 1 :op "print"})) ]]
return nil
