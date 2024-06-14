-- @noindex
local actions
local function _1_()
  return ru.take.cursor.update(ru.take["get-active"](), 1)
end
local function _2_()
  return ru.take.cursor.update(ru.take["get-active"](), -1)
end
local function _3_()
  local me = ru["midi-editor"]
  return me["pitch-cursor"].update(me["get-active"](), 1)
end
local function _4_()
  local me = ru["midi-editor"]
  return me["pitch-cursor"].update(me["get-active"](), -1)
end
local function _5_()
  return ru.take.grid.set(T, 1)
end
local function _6_()
  local T = ru.take["get-active"]()
  return ru.take.grid.set(T, (3 * ru.take.grid.get(T)))
end
local function _7_()
  local T = ru.take["get-active"]()
  return ru.take.grid.set(T, (2 * ru.take.grid.get(T)))
end
local function _8_()
  local T = ru.take["get-active"]()
  return ru.take.grid.set(T, (ru.take.grid.get(T) / 2))
end
local function _9_()
  local T = ru.take["get-active"]()
  return ru.take.grid.set(T, (ru.take.grid.get(T) / 3))
end
local function _10_()
  return ru.take["time-selection"].update(ru.take["get-active"](), nil, 1)
end
local function _11_()
  return ru.take["time-selection"].update(ru.take["get-active"](), nil, -1)
end
local function _12_()
  local t = ru.take
  local T = t["get-active"]()
  t["time-selection"].update(T, "fw", -1)
  return t.cursor.set(T, t["time-selection"].get(T)["end"])
end
local function _13_()
  local t = ru.take
  local T = t["get-active"]()
  t["time-selection"].update(T, "bw", 1)
  return t.cursor.set(T, t["time-selection"].get(T).start)
end
local function _14_()
  local t = ru.take
  local T = t["get-active"]()
  t["time-selection"].update(T, "fw", 1)
  return t.cursor.set(T, t["time-selection"].get(T)["end"])
end
local function _15_()
  local t = ru.take
  local T = t["get-active"]()
  t["time-selection"].update(T, "bw", -1)
  return t.cursor.set(T, t["time-selection"].get(T).start)
end
local function _16_()
  return ru.take["time-selection"].set(ru.take["get-active"](), 0, 0)
end
local function _17_()
  local t = ru.take
  local T = t["get-active"]()
  local focus = t.focus.get(T)
  local grid = t.grid["get-ppq"](T)
  return t["insert-note"](T, {["start-position"] = focus.x, ["end-position"] = (focus.x + grid), pitch = focus.y})
end
local function _18_()
  local T = ru.take["get-active"]()
  return ru.take.focus["next-note"](T)
end
local function _19_()
  local T = ru.take["get-active"]()
  return ru.take.focus["previous-note"](T)
end
local function _20_()
  local T = ru.take["get-active"]()
  return ru.take["set-note"](T, u.tbl.upd(ru.take["focused-note"](T), {selected = u.hof["not"]}))
end
local function _21_()
  local T = ru.take["get-active"]()
  local function _22_(c)
    return ((1 + c) % 16)
  end
  pcall(function() require("fennel").metadata:setall(_22_, "fnl/arglist", {"c"}) end)
  return ru.take["set-note"](T, u.tbl.upd(ru.take["focused-note"](T), {channel = _22_}))
end
local function _23_()
  local T = ru.take["get-active"]()
  local function _24_(c)
    return ((c - 1) % 16)
  end
  pcall(function() require("fennel").metadata:setall(_24_, "fnl/arglist", {"c"}) end)
  return ru.take["set-note"](T, u.tbl.upd(ru.take["focused-note"](T), {channel = _24_}))
end
local function _25_()
  local T = ru.take["get-active"]()
  local function _26_(v)
    return math.min(127, (10 + v))
  end
  pcall(function() require("fennel").metadata:setall(_26_, "fnl/arglist", {"v"}) end)
  return ru.take["set-note"](T, u.tbl.upd(ru.take["focused-note"](T), {velocity = _26_}))
end
local function _27_()
  local T = ru.take["get-active"]()
  local function _28_(v)
    return math.min(127, (v - 10))
  end
  pcall(function() require("fennel").metadata:setall(_28_, "fnl/arglist", {"v"}) end)
  return ru.take["set-note"](T, u.tbl.upd(ru.take["focused-note"](T), {velocity = _28_}))
end
actions = {move = {key = "m", children = {right = {key = "l", fn = _1_}, left = {key = "h", fn = _2_}, up = {key = "k", fn = _3_}, down = {key = "j", fn = _4_}}}, grid = {key = "g", children = {quarters = {key = "n", fn = _5_}, mul3 = {key = "J", fn = _6_}, mul2 = {key = "j", fn = _7_}, div2 = {key = "k", fn = _8_}, div3 = {key = "K", fn = _9_}}}, ["time-selection"] = {key = "t", children = {["move-fw"] = {key = "l", fn = _10_}, ["move-bw"] = {key = "h", fn = _11_}, ["shrink-fw"] = {key = "H", fn = _12_}, ["shrink-bw"] = {key = "C-l", fn = _13_}, ["grow-fw"] = {key = "L", fn = _14_}, ["grow-bw"] = {key = "C-h", fn = _15_}, clear = {key = "x", fn = _16_}}}, note = {key = "n", children = {insert = {key = "i", fn = _17_}, next = {key = "f", fn = _18_}, previous = {key = "b", fn = _19_}, ["toggle-selection"] = {key = "t", fn = _20_}, channel = {key = "c", children = {up = {key = "k", fn = _21_}, down = {key = "j", fn = _23_}}}, velocity = {key = "v", children = {up = {key = "k", fn = _25_}, down = {key = "j", fn = _27_}}}}}}
local function action_path(p)
  local _29_ = u.tbl.path(p)
  if (nil ~= _29_) then
    return u.seq.interpose(_29_, "children")
  else
    return _29_
  end
end
pcall(function() require("fennel").metadata:setall(action_path, "fnl/arglist", {"p"}) end)
local function do_action(p)
  local fn_path
  do
    local _31_ = action_path(p)
    if (nil ~= _31_) then
      fn_path = u.seq.append(_31_, "fn")
    else
      fn_path = _31_
    end
  end
  local _33_ = u.tbl.get(actions, fn_path)
  if (nil ~= _33_) then
    local f = _33_
    return f()
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(do_action, "fnl/arglist", {"p"}, "fnl/docstring", "Retrieve the fn at given path and call it") end)
--[[ ((?. actions "move" "children" "left" "fn")) (action-path "move.left") (do-action "move.left") (do-action "move.right") (do-action "move.up") (do-action nil) ]]
return nil
