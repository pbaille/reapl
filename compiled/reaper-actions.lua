-- @noindex
local _local_1_ = require("pb-utils")
local tbl = _local_1_["tbl"]
local clone = _local_1_["clone"]
local file = _local_1_["file"]
local seq = _local_1_["seq"]
local ru = require("reaper-utils")
ru.misc.log("LOAD ACTIONS")
local reaper_actions
local function _2_()
  return ru.take.cursor.update(ru.take["get-active"](), 1)
end
local function _3_()
  return ru.take.cursor.update(ru.take["get-active"](), -1)
end
local function _4_()
  local me = ru["midi-editor"]
  return me["pitch-cursor"].update(me["get-active"](), 1)
end
local function _5_()
  local me = ru["midi-editor"]
  return me["pitch-cursor"].update(me["get-active"](), -1)
end
local function _6_()
  return ru.take.grid.set(T, 1)
end
local function _7_()
  local T = ru.take["get-active"]()
  return ru.take.grid.set(T, (3 * ru.take.grid.get(T)))
end
local function _8_()
  local T = ru.take["get-active"]()
  return ru.take.grid.set(T, (2 * ru.take.grid.get(T)))
end
local function _9_()
  local T = ru.take["get-active"]()
  return ru.take.grid.set(T, (ru.take.grid.get(T) / 2))
end
local function _10_()
  local T = ru.take["get-active"]()
  return ru.take.grid.set(T, (ru.take.grid.get(T) / 3))
end
local function _11_()
  return ru.take["time-selection"].update(ru.take["get-active"](), nil, 1)
end
local function _12_()
  return ru.take["time-selection"].update(ru.take["get-active"](), nil, -1)
end
local function _13_()
  local t = ru.take
  local T = t["get-active"]()
  t["time-selection"].update(T, "fw", -1)
  return t.cursor.set(T, t["time-selection"].get(T)["end"])
end
local function _14_()
  local t = ru.take
  local T = t["get-active"]()
  t["time-selection"].update(T, "bw", 1)
  return t.cursor.set(T, t["time-selection"].get(T).start)
end
local function _15_()
  local t = ru.take
  local T = t["get-active"]()
  t["time-selection"].update(T, "fw", 1)
  return t.cursor.set(T, t["time-selection"].get(T)["end"])
end
local function _16_()
  local t = ru.take
  local T = t["get-active"]()
  t["time-selection"].update(T, "bw", -1)
  return t.cursor.set(T, t["time-selection"].get(T).start)
end
local function _17_()
  return ru.take["time-selection"].set(ru.take["get-active"](), 0, 0)
end
local function _18_()
  local t = ru.take
  local T = t["get-active"]()
  local focus = t.focus.get(T)
  local grid = t.grid["get-ppq"](T)
  return t["insert-note"](T, {["start-position"] = focus.x, ["end-position"] = (focus.x + grid), pitch = focus.y})
end
local function _19_()
  local T = ru.take["get-active"]()
  return ru.take.focus["next-note"](T)
end
local function _20_()
  local T = ru.take["get-active"]()
  return ru.take.focus["previous-note"](T)
end
local function _21_()
  local T = ru.take["get-active"]()
  return ru.take["set-note"](T, u.tbl.upd(ru.take["focused-note"](T), {selected = u.hof["not"]}))
end
local function _22_()
  local T = ru.take["get-active"]()
  local function _23_(c)
    return ((1 + c) % 16)
  end
  pcall(function() require("fennel").metadata:setall(_23_, "fnl/arglist", {"c"}) end)
  return ru.take["set-note"](T, u.tbl.upd(ru.take["focused-note"](T), {channel = _23_}))
end
local function _24_()
  local T = ru.take["get-active"]()
  local function _25_(c)
    return ((c - 1) % 16)
  end
  pcall(function() require("fennel").metadata:setall(_25_, "fnl/arglist", {"c"}) end)
  return ru.take["set-note"](T, u.tbl.upd(ru.take["focused-note"](T), {channel = _25_}))
end
local function _26_()
  local T = ru.take["get-active"]()
  local function _27_(v)
    return math.min(127, (10 + v))
  end
  pcall(function() require("fennel").metadata:setall(_27_, "fnl/arglist", {"v"}) end)
  return ru.take["set-note"](T, u.tbl.upd(ru.take["focused-note"](T), {velocity = _27_}))
end
local function _28_()
  local T = ru.take["get-active"]()
  local function _29_(v)
    return math.min(127, (v - 10))
  end
  pcall(function() require("fennel").metadata:setall(_29_, "fnl/arglist", {"v"}) end)
  return ru.take["set-note"](T, u.tbl.upd(ru.take["focused-note"](T), {velocity = _29_}))
end
reaper_actions = {move = {key = "m", children = {right = {key = "l", fn = _2_}, left = {key = "h", fn = _3_}, up = {key = "k", fn = _4_}, down = {key = "j", fn = _5_}}}, grid = {key = "g", children = {quarters = {key = "n", fn = _6_}, mul3 = {key = "J", fn = _7_}, mul2 = {key = "j", fn = _8_}, div2 = {key = "k", fn = _9_}, div3 = {key = "K", fn = _10_}}}, ["time-selection"] = {key = "t", children = {["move-fw"] = {key = "l", fn = _11_}, ["move-bw"] = {key = "h", fn = _12_}, ["shrink-fw"] = {key = "H", fn = _13_}, ["shrink-bw"] = {key = "C-l", fn = _14_}, ["grow-fw"] = {key = "L", fn = _15_}, ["grow-bw"] = {key = "C-h", fn = _16_}, clear = {key = "x", fn = _17_}}}, note = {key = "n", children = {insert = {key = "i", fn = _18_}, next = {key = "f", fn = _19_}, previous = {key = "b", fn = _20_}, ["toggle-selection"] = {key = "t", fn = _21_}, channel = {key = "c", children = {up = {key = "k", fn = _22_}, down = {key = "j", fn = _24_}}}, velocity = {key = "v", children = {up = {key = "k", fn = _26_}, down = {key = "j", fn = _28_}}}}}}
local function action_path(p)
  local _30_ = tbl.path(p)
  if (nil ~= _30_) then
    return seq.interpose(_30_, "children")
  else
    return _30_
  end
end
pcall(function() require("fennel").metadata:setall(action_path, "fnl/arglist", {"p"}) end)
local function do_action(p)
  local fn_path
  do
    local _32_ = action_path(p)
    if (nil ~= _32_) then
      fn_path = seq.append(_32_, "fn")
    else
      fn_path = _32_
    end
  end
  local _34_ = tbl.get(reaper_actions, fn_path)
  if (nil ~= _34_) then
    local f = _34_
    return f()
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(do_action, "fnl/arglist", {"p"}, "fnl/docstring", "Retrieve the fn at given path and call it") end)
local function str_join(s, x)
  local ret = ""
  for i, v in ipairs(s) do
    if (i > 1) then
      ret = (ret .. x .. v)
    else
      ret = v
    end
  end
  return ret
end
pcall(function() require("fennel").metadata:setall(str_join, "fnl/arglist", {"s", "x"}) end)
local function get_binding_tree()
  local function _37_(at, node)
    if ("table" == type(node)) then
      local f = node.fn
      if f then
        node["fn"] = str_join(seq["take-nth"](at, 2), "-")
      else
      end
      return node
    else
      return node
    end
  end
  pcall(function() require("fennel").metadata:setall(_37_, "fnl/arglist", {"at", "node"}) end)
  return tbl["indexed-prewalk"](clone(reaper_actions), _37_)
end
pcall(function() require("fennel").metadata:setall(get_binding_tree, "fnl/arglist", {}) end)
local function make_action_dispatcher()
  local dispatch = {}
  local function _40_(at, node)
    if ("table" == type(node)) then
      local f = node.fn
      if f then
        dispatch[str_join(seq["take-nth"](at, 2), "-")] = f
      else
      end
      return node
    else
      return node
    end
  end
  pcall(function() require("fennel").metadata:setall(_40_, "fnl/arglist", {"at", "node"}) end)
  tbl["indexed-prewalk"](clone(reaper_actions), _40_)
  return dispatch
end
pcall(function() require("fennel").metadata:setall(make_action_dispatcher, "fnl/arglist", {}) end)
local function file_exists_3f(path)
  local f = io.open(path, "r")
  if f then
    f:close()
    return true
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(file_exists_3f, "fnl/arglist", {"path"}) end)
local function create_actions_dir(compilation_path, dispatch)
  local actions_path = (compilation_path .. "/actions")
  local action_name__3escript
  local function _44_(name)
    return ("package.path = \"" .. compilation_path .. "/?.lua;\" .. package.path\n" .. "if not reapl_actions then reapl_actions = require(\"reaper-actions\")\n end" .. "reapl_actions.dispatch[\"" .. name .. "\"]()")
  end
  pcall(function() require("fennel").metadata:setall(_44_, "fnl/arglist", {"name"}) end)
  action_name__3escript = _44_
  if not os.execute(("ls -d " .. actions_path)) then
    os.execute(("mkdir " .. actions_path))
  else
  end
  if (not file_exists_3f((compilation_path .. "/reaper-utils.lua")) or not file_exists_3f((compilation_path .. "/reaper-actions.lua"))) then
    error("reaper-utils.lua and reaper-acttions.lua should be in the given compilation path")
  else
  end
  local codes = {}
  for name, _ in pairs(dispatch) do
    local filepath = (actions_path .. "/pbaille_" .. name .. ".lua")
    file.spit(filepath, action_name__3escript(name))
    do end (codes)[name] = reaper.AddRemoveReaScript(true, 0, filepath, true)
  end
  return codes
end
pcall(function() require("fennel").metadata:setall(create_actions_dir, "fnl/arglist", {"compilation-path", "dispatch"}) end)
--[[ ((?. actions "move" "children" "left" "fn")) (action-path "move.left") (actions.do-action "move.left") (do-action "move.right") (do-action "move.up") (do-action nil) ]]
return {tree = reaper_actions, ["do-action"] = do_action, ["get-binding-tree"] = get_binding_tree, dispatch = make_action_dispatcher(), ["create-actions-dir"] = create_actions_dir}
