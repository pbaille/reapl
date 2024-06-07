-- @noindex
local _local_1_ = require("pb-utils")
local u = _local_1_
local tbl = _local_1_["tbl"]
local seq = _local_1_["seq"]
local hof = _local_1_["hof"]
local pp = require("pprint")
local r = reaper
local time = {signature = {}}
time.signature.get = function()
  local bpm, bpi = reaper.GetProjectTimeSignature2(0)
  return {bpm = bpm, bpi = bpi}
end
pcall(function() require("fennel").metadata:setall(time.signature.get, "fnl/arglist", {}) end)
local misc = {}
misc.log = function(param)
  return reaper.ShowConsoleMsg((tostring(param) .. "\n"))
end
pcall(function() require("fennel").metadata:setall(misc.log, "fnl/arglist", {"param"}, "fnl/docstring", "Log the given parameter to the reaper console.") end)
misc.pp = function(param)
  return reaper.ShowConsoleMsg(pp.pformat(param))
end
pcall(function() require("fennel").metadata:setall(misc.pp, "fnl/arglist", {"param"}, "fnl/docstring", "Pretty print the given parameter to the reaper console.") end)
local midi_editor = {["pitch-cursor"] = {}}
midi_editor["get-active"] = function()
  return r.MIDIEditor_GetActive()
end
pcall(function() require("fennel").metadata:setall(midi_editor["get-active"], "fnl/arglist", {}, "fnl/docstring", "Return the active MIDI editor instance.") end)
midi_editor["get-take"] = function(me)
  return r.MIDIEditor_GetTake(me)
end
pcall(function() require("fennel").metadata:setall(midi_editor["get-take"], "fnl/arglist", {"me"}, "fnl/docstring", "Get the take from the given MIDI editor instance.") end)
midi_editor["pitch-cursor"].get = function(me)
  return reaper.MIDIEditor_GetSetting_int(me, "active_note_row")
end
pcall(function() require("fennel").metadata:setall(midi_editor["pitch-cursor"].get, "fnl/arglist", {"me"}, "fnl/docstring", "Get the pitch cursor position from the given MIDI editor instance.") end)
midi_editor["pitch-cursor"].set = function(me, i)
  return reaper.MIDIEditor_SetSetting_int(me, "active_note_row", i)
end
pcall(function() require("fennel").metadata:setall(midi_editor["pitch-cursor"].set, "fnl/arglist", {"me", "i"}, "fnl/docstring", "Set the pitch cursor position in the given MIDI editor instance.") end)
midi_editor["pitch-cursor"].update = function(me, delta)
  return midi_editor["pitch-cursor"].set(me, (delta + midi_editor["pitch-cursor"].get(me)))
end
pcall(function() require("fennel").metadata:setall(midi_editor["pitch-cursor"].update, "fnl/arglist", {"me", "delta"}, "fnl/docstring", "Update the pitch cursor position by a given delta\n   in the given MIDI editor instance.") end)
local note = {}
note["shift-position"] = function(n, offset)
  return tbl.upd(n, {["start-position"] = hof.adder(offset), ["end-position"] = hof.adder(offset)})
end
pcall(function() require("fennel").metadata:setall(note["shift-position"], "fnl/arglist", {"n", "offset"}, "fnl/docstring", "Shift the start and end positions of note `n` by `offset`.") end)
note.lte = function(a, b)
  if (a["start-position"] == b["start-position"]) then
    return (a.pitch < b.pitch)
  else
    return (a["start-position"] < b["start-position"])
  end
end
pcall(function() require("fennel").metadata:setall(note.lte, "fnl/arglist", {"a", "b"}, "fnl/docstring", "Compare if note `a` is less than or equal to note `b`.\n   Comparison is based on start position, then pitch.") end)
note.gte = function(a, b)
  if (a["start-position"] == b["start-position"]) then
    return (a.pitch > b.pitch)
  else
    return (a["start-position"] > b["start-position"])
  end
end
pcall(function() require("fennel").metadata:setall(note.gte, "fnl/arglist", {"a", "b"}, "fnl/docstring", "Compare if note `a` is greater than or equal to note `b`.\n   Comparison is based on start position, then pitch.") end)
local take = {grid = {}, ["time-selection"] = {}, ["note-selection"] = {}, cursor = {}, focus = {}, note = {}, notes = {}, ccs = {}}
take["get-active"] = function()
  return midi_editor["get-take"](midi_editor["get-active"]())
end
pcall(function() require("fennel").metadata:setall(take["get-active"], "fnl/arglist", {}, "fnl/docstring", "Get the active take from the active MIDI editor.") end)
take["get-ppq"] = function(t)
  return math.floor(r.MIDI_GetPPQPosFromProjQN(t, 1))
end
pcall(function() require("fennel").metadata:setall(take["get-ppq"], "fnl/arglist", {"t"}, "fnl/docstring", "Get PPQ (Pulses Per Quarter note) from the given take `t`.") end)
take["ppq->qpos"] = function(t, x)
  return (x / take["get-ppq"](t))
end
pcall(function() require("fennel").metadata:setall(take["ppq->qpos"], "fnl/arglist", {"t", "x"}, "fnl/docstring", "Convert PPQ `x` to quarter note position in the given take `t`.") end)
take["qpos->ppq"] = function(t, x)
  return (x * take["get-ppq"](t))
end
pcall(function() require("fennel").metadata:setall(take["qpos->ppq"], "fnl/arglist", {"t", "x"}, "fnl/docstring", "Convert quarter note position `x` to PPQ in the given take `t`.") end)
take["mark-dirty"] = function(t)
  r.MarkTrackItemsDirty(r.GetMediaItemTake_Track(t), r.GetMediaItemTake_Item(t))
  return "ok"
end
pcall(function() require("fennel").metadata:setall(take["mark-dirty"], "fnl/arglist", {"t"}, "fnl/docstring", "Mark the given take `t` as dirty, indicating it has been modified.") end)
take.sort = function(t)
  r.MIDI_Sort(t)
  return "ok"
end
pcall(function() require("fennel").metadata:setall(take.sort, "fnl/arglist", {"t"}, "fnl/docstring", "Sort the MIDI events in the given take `t`.") end)
take["disable-sort"] = function(t)
  r.MIDI_DisableSort(t)
  return "ok"
end
pcall(function() require("fennel").metadata:setall(take["disable-sort"], "fnl/arglist", {"t"}, "fnl/docstring", "Disable automatic sorting of MIDI events in the given take `t`.") end)
take.note.default = function(t)
  return {channel = 1, pitch = 60, velocity = 80, ["start-position"] = 0, ["end-position"] = take["get-ppq"](t), selected = true, muted = false}
end
pcall(function() require("fennel").metadata:setall(take.note.default, "fnl/arglist", {"t"}, "fnl/docstring", "Return a default note table for the given take `t`.") end)
take.note["to-absolute-position"] = function(t, n)
  local _let_4_ = n
  local position = _let_4_["position"]
  local duration = _let_4_["duration"]
  local start_pos = take["qpos->ppq"](t, position)
  local end_pos = (start_pos + take["qpos->ppq"](t, duration))
  do end (n)["position"] = nil
  n["duration"] = nil
  n["start-position"] = start_pos
  n["end-position"] = end_pos
  return n
end
pcall(function() require("fennel").metadata:setall(take.note["to-absolute-position"], "fnl/arglist", {"t", "n"}, "fnl/docstring", "Convert a note `n` with relative position and duration to absolute positions in the given take `t`.") end)
take.note.mk = function(t, n)
  if (n.position and n.duration) then
    return take.note.mk(t, take.note["to-absolute-position"](t, n))
  else
    return tbl.merge(take.note.default(t), n)
  end
end
pcall(function() require("fennel").metadata:setall(take.note.mk, "fnl/arglist", {"t", "n"}, "fnl/docstring", "Create a note for the given take `t` from the note table `n`.\n   If `n` contains relative positions, convert them to absolute positions.") end)
take["project-time->ppq"] = function(t, x)
  return reaper.MIDI_GetPPQPosFromProjTime(t, x)
end
pcall(function() require("fennel").metadata:setall(take["project-time->ppq"], "fnl/arglist", {"t", "x"}, "fnl/docstring", "Convert project time `x` to PPQ in the given take `t`.") end)
take["ppq->project-time"] = function(t, x)
  return reaper.MIDI_GetProjTimeFromPPQPos(t, x)
end
pcall(function() require("fennel").metadata:setall(take["ppq->project-time"], "fnl/arglist", {"t", "x"}, "fnl/docstring", "Convert PPQ `x` to project time in the given take `t`.") end)
take["project-time->qpos"] = function(t, x)
  return take["ppq->qpos"](t, take["project-time->ppq"](t, x))
end
pcall(function() require("fennel").metadata:setall(take["project-time->qpos"], "fnl/arglist", {"t", "x"}, "fnl/docstring", "Convert project time `x` to QPOS in the given take `t`.") end)
take["qpos->project-time"] = function(t, x)
  return take["ppq->project-time"](t, take["qpos->ppq"](t, x))
end
pcall(function() require("fennel").metadata:setall(take["qpos->project-time"], "fnl/arglist", {"t", "x"}, "fnl/docstring", "Convert QPOS `x` to project time in the given take `t`.") end)
take.grid.get = function(t)
  return reaper.MIDI_GetGrid(t)
end
pcall(function() require("fennel").metadata:setall(take.grid.get, "fnl/arglist", {"t"}, "fnl/docstring", "Get the grid setting of the given take `t`.") end)
take.grid["get-ppq"] = function(t)
  return take["qpos->ppq"](t, reaper.MIDI_GetGrid(t))
end
pcall(function() require("fennel").metadata:setall(take.grid["get-ppq"], "fnl/arglist", {"t"}, "fnl/docstring", "Get the grid setting in PPQ for the given take `t`.") end)
take.grid.set = function(t, x)
  local sig = time.signature.get()
  return reaper.SetMIDIEditorGrid(0, (x / sig.bpi))
end
pcall(function() require("fennel").metadata:setall(take.grid.set, "fnl/arglist", {"t", "x"}, "fnl/docstring", "Set the grid of the given take `t` to `x`.") end)
take["time-selection"].get = function(t)
  local start, _end = reaper.GetSet_LoopTimeRange(false, false, 0, 0, false)
  if not (start == _end) then
    return {start = take["project-time->ppq"](t, start), ["end"] = take["project-time->ppq"](t, _end)}
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(take["time-selection"].get, "fnl/arglist", {"t"}, "fnl/docstring", "Get the time selection of the given take `t`.") end)
take["time-selection"].set = function(t, start, _end)
  local start0 = take["ppq->project-time"](t, start)
  local _end0 = take["ppq->project-time"](t, _end)
  return reaper.GetSet_LoopTimeRange(true, false, start0, _end0, false)
end
pcall(function() require("fennel").metadata:setall(take["time-selection"].set, "fnl/arglist", {"t", "start", "end"}, "fnl/docstring", "Set the time selection of the given take `t` from `start` to `end`.") end)
take["time-selection"]["get-qpos"] = function(t)
  local _let_7_ = take["time-selection"].get(t)
  local start = _let_7_["start"]
  local _end = _let_7_["end"]
  return {start = take["ppq->qpos"](t, start), ["end"] = take["ppq->qpos"](t, _end)}
end
pcall(function() require("fennel").metadata:setall(take["time-selection"]["get-qpos"], "fnl/arglist", {"t"}, "fnl/docstring", "Get the time selection in QPOS of the given take `t`.") end)
take["time-selection"]["set-qpos"] = function(t, start, _end)
  return take["time-selection"].set(t, take["qpos->ppq"](t, start), take["qpos->ppq"](t, _end))
end
pcall(function() require("fennel").metadata:setall(take["time-selection"]["set-qpos"], "fnl/arglist", {"t", "start", "end"}, "fnl/docstring", "Set the time selection of the given take `t` from `start` to `end` using QPOS.") end)
take["time-selection"].update = function(t, side, delta)
  local cursor_pos = take.cursor.get(t)
  local sel = (take["time-selection"].get(t) or {start = cursor_pos, ["end"] = cursor_pos})
  local increment = take["qpos->ppq"](t, (delta * take.grid.get(t)))
  if (side == "fw") then
    take["time-selection"].set(t, sel.start, (sel["end"] + increment))
  elseif (side == "bw") then
    take["time-selection"].set(t, (sel.start + increment), sel["end"])
  else
    local _ = side
    take["time-selection"].set(t, (sel.start + increment), (sel["end"] + increment))
  end
  return "ok"
end
pcall(function() require("fennel").metadata:setall(take["time-selection"].update, "fnl/arglist", {"t", "side", "delta"}, "fnl/docstring", "Update the time selection of the given take `t` by `delta` on `side`.") end)
local NOON_MIDI_NOTE_SHIFT = 2
take["note-selection"].get = function(t)
  local notes = take.notes.get(t)
  local selected_notes
  local function _9_(n)
    return n.selected
  end
  pcall(function() require("fennel").metadata:setall(_9_, "fnl/arglist", {"n"}) end)
  selected_notes = seq.filter(notes, _9_)
  local candidates
  if (0 == #selected_notes) then
    candidates = notes
  else
    candidates = selected_notes
  end
  local time_selection = take["time-selection"].get(t)
  if ((_G.type(time_selection) == "table") and (nil ~= time_selection.start) and (nil ~= time_selection["end"])) then
    local start = time_selection.start
    local _end = time_selection["end"]
    local function _11_(n)
      return (function(_12_,_13_,_14_,_15_) return (_12_ <= _13_) and (_13_ <= _14_) and (_14_ <= _15_) end)(start,n["start-position"],n["end-position"],(_end + NOON_MIDI_NOTE_SHIFT))
    end
    pcall(function() require("fennel").metadata:setall(_11_, "fnl/arglist", {"n"}) end)
    return seq.filter(candidates, _11_)
  else
    local _ = time_selection
    return candidates
  end
end
pcall(function() require("fennel").metadata:setall(take["note-selection"].get, "fnl/arglist", {"t"}, "fnl/docstring", "Get the selected notes or, if none selected, all notes within the time selection of the given take `t`.") end)
take["note-selection"]["delete-all"] = function(t)
  return take["delete-notes"](t, take["note-selection"].get(t))
end
pcall(function() require("fennel").metadata:setall(take["note-selection"]["delete-all"], "fnl/arglist", {"t"}, "fnl/docstring", "Delete all selected notes of the given take `t`.") end)
take.cursor.get = function(t)
  local curs_pos = r.GetCursorPosition()
  return take["project-time->ppq"](t, curs_pos)
end
pcall(function() require("fennel").metadata:setall(take.cursor.get, "fnl/arglist", {"t"}, "fnl/docstring", "Get the current cursor position in PPQ for the given take `t`.") end)
take.cursor.set = function(t, p)
  return reaper.SetEditCurPos(ru.take["ppq->project-time"](t, p), true, false)
end
pcall(function() require("fennel").metadata:setall(take.cursor.set, "fnl/arglist", {"t", "p"}, "fnl/docstring", "Set the cursor position in the given take `t` to PPQ position `p`.") end)
take.cursor["get-qpos"] = function(t)
  return take["ppq->qpos"](t, take.cursor.get(t))
end
pcall(function() require("fennel").metadata:setall(take.cursor["get-qpos"], "fnl/arglist", {"t"}, "fnl/docstring", "Get the cursor position in QPOS for the given take `t`.") end)
take.cursor["set-qpos"] = function(t, p)
  return take.cursor.set(t, take["qpos->ppq"](t, p))
end
pcall(function() require("fennel").metadata:setall(take.cursor["set-qpos"], "fnl/arglist", {"t", "p"}, "fnl/docstring", "Set the cursor position in the given take `t` to QPOS position `p`.") end)
take.cursor.ceil = function(t)
  return take.cursor.set(t, reaper.BR_GetNextGridDivision(take.cursor.get(t)))
end
pcall(function() require("fennel").metadata:setall(take.cursor.ceil, "fnl/arglist", {"t"}, "fnl/docstring", "Set the cursor to the next grid division in the given take `t`.") end)
take.cursor.floor = function(t)
  return take.cursor.set(t, reaper.BR_GetPrevGridDivision(take.cursor.get(t)))
end
pcall(function() require("fennel").metadata:setall(take.cursor.floor, "fnl/arglist", {"t"}, "fnl/docstring", "Set the cursor to the previous grid division in the given take `t`.") end)
take.cursor.round = function(t)
  return take.cursor.set(t, reaper.BR_GetClosestGridDivision(take.cursor.get(t)))
end
pcall(function() require("fennel").metadata:setall(take.cursor.round, "fnl/arglist", {"t"}, "fnl/docstring", "Set the cursor to the closest grid division in the given take `t`.") end)
take.cursor.update = function(t, delta)
  local increment = take["qpos->ppq"](t, (delta * take.grid.get(t)))
  return take.cursor.set(t, (take.cursor.get(t) + increment))
end
pcall(function() require("fennel").metadata:setall(take.cursor.update, "fnl/arglist", {"t", "delta"}, "fnl/docstring", "Update the cursor position by `delta` grid units in the given take `t`.") end)
take.focus.get = function(t)
  return {x = take.cursor.get(t), y = midi_editor["pitch-cursor"].get(midi_editor["get-active"]())}
end
pcall(function() require("fennel").metadata:setall(take.focus.get, "fnl/arglist", {"t"}, "fnl/docstring", "Get the current focus position in the given take `t`.") end)
take.focus.set = function(t, upd)
  local _let_17_ = tbl.upd(take.focus.get(t), upd)
  local x = _let_17_["x"]
  local y = _let_17_["y"]
  local new_focus = _let_17_
  take.cursor.set(t, x)
  midi_editor["pitch-cursor"].set(midi_editor["get-active"](), y)
  return new_focus
end
pcall(function() require("fennel").metadata:setall(take.focus.set, "fnl/arglist", {"t", "upd"}, "fnl/docstring", "Set the focus position in the given take `t` with updates from `upd`.") end)
take.focus["next-note"] = function(t)
  local _let_18_ = take.focus.get(t)
  local x = _let_18_["x"]
  local y = _let_18_["y"]
  local candidates
  local function _19_(n)
    return ((n["start-position"] > x) or ((n["start-position"] == x) and (n.pitch > y)))
  end
  pcall(function() require("fennel").metadata:setall(_19_, "fnl/arglist", {"n"}) end)
  candidates = take.notes.filter(t, _19_)
  return take["focus-note"](t, seq.first(seq["sort-with"](candidates, note.lte)))
end
pcall(function() require("fennel").metadata:setall(take.focus["next-note"], "fnl/arglist", {"t"}, "fnl/docstring", "Move the focus to the next note in the given take `t`.") end)
take.focus["previous-note"] = function(t)
  local _let_20_ = take.focus.get(t)
  local x = _let_20_["x"]
  local y = _let_20_["y"]
  local candidates
  local function _21_(n)
    return ((n["start-position"] < x) or ((n["start-position"] == x) and (n.pitch < y)))
  end
  pcall(function() require("fennel").metadata:setall(_21_, "fnl/arglist", {"n"}) end)
  candidates = take.notes.filter(t, _21_)
  return take["focus-note"](t, seq.first(seq["sort-with"](candidates, note.gte)))
end
pcall(function() require("fennel").metadata:setall(take.focus["previous-note"], "fnl/arglist", {"t"}, "fnl/docstring", "Move the focus to the previous note in the given take `t`.") end)
take.focus["get-closest-note"] = function(t)
  local _let_22_ = take.focus.get(t)
  local x = _let_22_["x"]
  local y = _let_22_["y"]
  local notes = take.notes.get(t)
  local by_dist
  local function _23_(n)
    return {["delta-x"] = math.abs((n["start-position"] - x)), ["delta-y"] = math.abs((n.pitch - y)), note = n}
  end
  pcall(function() require("fennel").metadata:setall(_23_, "fnl/arglist", {"n"}) end)
  local function _26_(_24_)
    local _arg_25_ = _24_
    local delta_x = _arg_25_["delta-x"]
    local delta_y = _arg_25_["delta-y"]
    return (delta_x + delta_y)
  end
  pcall(function() require("fennel").metadata:setall(_26_, "fnl/arglist", {"{:delta-x delta-x :delta-y delta-y}"}) end)
  by_dist = seq["sort-by"](seq.keep(notes, _23_), _26_)
  local t_27_ = seq.first(by_dist)
  if (nil ~= t_27_) then
    t_27_ = t_27_.note
  else
  end
  return t_27_
end
pcall(function() require("fennel").metadata:setall(take.focus["get-closest-note"], "fnl/arglist", {"t"}, "fnl/docstring", "Get the closest note to the current focus position in the given take `t`.") end)
take.focus["closest-note"] = function(t)
  return take["focus-note"](t, take.focus["get-closest-note"](t))
end
pcall(function() require("fennel").metadata:setall(take.focus["closest-note"], "fnl/arglist", {"t"}, "fnl/docstring", "Set the focus to the closest note in the given take `t`.") end)
take.focus["cycle-at-cursor"] = function(t)
  local _let_29_ = take.focus.get(t)
  local x = _let_29_["x"]
  local y = _let_29_["y"]
  local candidates
  local function _30_(n)
    return (n["start-position"] == x)
  end
  pcall(function() require("fennel").metadata:setall(_30_, "fnl/arglist", {"n"}) end)
  candidates = take.notes.filter(t, _30_)
  local n_choices = #candidates
  local current_idx = seq["index-of"](candidates, y)
  if not current_idx then
    return take.focus.set(t, {x = x, y = seq.first(candidates)})
  elseif (n_choices > 1) then
    local _31_
    if (current_idx == n_choices) then
      _31_ = 1
    else
      _31_ = (1 + current_idx)
    end
    return take.focus.set(t, {x = x, y = candidates[_31_]})
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(take.focus["cycle-at-cursor"], "fnl/arglist", {"t"}, "fnl/docstring", "Cycle through notes at the cursor position in the given take `t`.") end)
take["get-note"] = function(t, idx)
  local _, selected, muted, start_position, end_position, channel, pitch, velocity = r.MIDI_GetNote(t, idx)
  return {channel = channel, ["end-position"] = end_position, muted = muted, pitch = pitch, selected = selected, ["start-position"] = start_position, velocity = velocity, idx = idx}
end
pcall(function() require("fennel").metadata:setall(take["get-note"], "fnl/arglist", {"t", "idx"}, "fnl/docstring", "Get the note at index `idx` in the given take `t`.") end)
take["focus-note"] = function(t, n)
  if n then
    return take.focus.set(t, {x = n["start-position"], y = n.pitch})
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(take["focus-note"], "fnl/arglist", {"t", "n"}, "fnl/docstring", "Set the focus to note `n` in the given take `t`.") end)
take["focused-note"] = function(t)
  local _let_35_ = take.focus.get(t)
  local x = _let_35_["x"]
  local y = _let_35_["y"]
  local function _36_(n)
    return ((x == n["start-position"]) and (y == n.pitch))
  end
  pcall(function() require("fennel").metadata:setall(_36_, "fnl/arglist", {"n"}) end)
  return seq.find(take.notes.get(t), _36_)
end
pcall(function() require("fennel").metadata:setall(take["focused-note"], "fnl/arglist", {"t"}, "fnl/docstring", "Get the currently focused note in the given take `t`.") end)
take["set-note"] = function(t, _37_)
  local _arg_38_ = _37_
  local channel = _arg_38_["channel"]
  local end_position = _arg_38_["end-position"]
  local muted = _arg_38_["muted"]
  local pitch = _arg_38_["pitch"]
  local selected = _arg_38_["selected"]
  local start_position = _arg_38_["start-position"]
  local velocity = _arg_38_["velocity"]
  local idx = _arg_38_["idx"]
  return r.MIDI_SetNote(t, idx, selected, muted, start_position, end_position, channel, pitch, velocity, true)
end
pcall(function() require("fennel").metadata:setall(take["set-note"], "fnl/arglist", {"t", "{:channel channel :end-position end-position :idx idx :muted muted :pitch pitch :selected selected :start-position start-position :velocity velocity}"}, "fnl/docstring", "Set note properties in the given take `t` using the note table.") end)
take["upd-note"] = function(t, n, u0)
  return take["set-note"](t, tbl.upd(n, u0))
end
pcall(function() require("fennel").metadata:setall(take["upd-note"], "fnl/arglist", {"t", "n", "u"}, "fnl/docstring", "Update note `n` in the given take `t` with updates `u`.") end)
take["delete-note"] = function(t, idx)
  return r.MIDI_DeleteNote(t, idx)
end
pcall(function() require("fennel").metadata:setall(take["delete-note"], "fnl/arglist", {"t", "idx"}, "fnl/docstring", "Delete the note at index `idx` in the given take `t`.") end)
take["delete-notes"] = function(t, xs)
  local idxs
  do
    local idxs0
    local function _39_(n)
      return n.idx
    end
    pcall(function() require("fennel").metadata:setall(_39_, "fnl/arglist", {"n"}) end)
    idxs0 = seq.keep(xs, _39_)
    local function _40_(a, b)
      return (a > b)
    end
    pcall(function() require("fennel").metadata:setall(_40_, "fnl/arglist", {"a", "b"}) end)
    idxs = seq["sort-with"](idxs0, _40_)
  end
  take["disable-sort"](t)
  for _, i in ipairs(idxs) do
    take["delete-note"](t, i)
  end
  return take.sort(t)
end
pcall(function() require("fennel").metadata:setall(take["delete-notes"], "fnl/arglist", {"t", "xs"}, "fnl/docstring", "Delete multiple notes `xs` in the given take `t`.") end)
take["insert-note"] = function(t, n)
  local _let_41_ = take.note.mk(t, n)
  local channel = _let_41_["channel"]
  local end_position = _let_41_["end-position"]
  local muted = _let_41_["muted"]
  local pitch = _let_41_["pitch"]
  local selected = _let_41_["selected"]
  local start_position = _let_41_["start-position"]
  local velocity = _let_41_["velocity"]
  local idx = take.notes.count(t)
  r.MIDI_InsertNote(t, selected, muted, start_position, end_position, channel, pitch, velocity, true)
  return take["get-note"](t, idx)
end
pcall(function() require("fennel").metadata:setall(take["insert-note"], "fnl/arglist", {"t", "n"}, "fnl/docstring", "Insert a note `n` in the given take `t`.") end)
take["insert-notes"] = function(t, xs)
  take["disable-sort"](t)
  for _, n in ipairs(xs) do
    take["insert-note"](t, n)
  end
  return take.sort(t)
end
pcall(function() require("fennel").metadata:setall(take["insert-notes"], "fnl/arglist", {"t", "xs"}, "fnl/docstring", "Insert multiple notes `xs` in the given take `t`.") end)
take.notes.count = function(t)
  local _, notecnt, _0, _1 = r.MIDI_CountEvts(t)
  return notecnt
end
pcall(function() require("fennel").metadata:setall(take.notes.count, "fnl/arglist", {"t"}, "fnl/docstring", "Get the count of notes in the given take `t`.") end)
take.notes.get = function(t)
  local cnt = take.notes.count(t)
  if (cnt > 0) then
    local ret = {}
    for i = 0, (cnt - 1) do
      ret = seq.append(ret, take["get-note"](t, i))
    end
    return ret
  else
    return {}
  end
end
pcall(function() require("fennel").metadata:setall(take.notes.get, "fnl/arglist", {"t"}, "fnl/docstring", "Get all notes in the given take `t`.") end)
take.notes.clear = function(t)
  local cnt = take.notes.count(t)
  if (cnt > 0) then
    for i = (cnt - 1), 0, -1 do
      take["delete-note"](t, i)
    end
    return nil
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(take.notes.clear, "fnl/arglist", {"t"}, "fnl/docstring", "Clear all notes in the given take `t`.") end)
take.notes.upd = function(t, u0)
  take["disable-sort"](t)
  for _, n in ipairs(take.notes.get(t)) do
    take["upd-note"](t, n, u0)
  end
  return take.sort(t)
end
pcall(function() require("fennel").metadata:setall(take.notes.upd, "fnl/arglist", {"t", "u"}, "fnl/docstring", "Update all notes in the given take `t` with updates `u`.") end)
take.notes.filter = function(t, matcher)
  return seq.filter(take.notes.get(t), tbl.matcher(matcher))
end
pcall(function() require("fennel").metadata:setall(take.notes.filter, "fnl/arglist", {"t", "matcher"}, "fnl/docstring", "Filter notes in the given take `t` using the `matcher` function.") end)
take.notes["filtered-upd"] = function(t, matcher, u0)
  for _, n in ipairs(take.notes.filter(t, matcher)) do
    take["upd-note"](t, n, u0)
  end
  return take.sort()
end
pcall(function() require("fennel").metadata:setall(take.notes["filtered-upd"], "fnl/arglist", {"t", "matcher", "u"}, "fnl/docstring", "Update filtered notes in the given take `t` matching `matcher` with updates `u`.") end)
take.ccs.count = function(t)
  local _, _0, cc_cnt, _1 = r.MIDI_CountEvts(t)
  return cc_cnt
end
pcall(function() require("fennel").metadata:setall(take.ccs.count, "fnl/arglist", {"t"}, "fnl/docstring", "Get the count of MIDI CC events in the given take `t`.") end)
--[[ (fn cycle-select-at-cursor [] "Cycle through notes at the cursor position." (let [cp (cursor-position (active-take)) notes (filter-notes (hashfn (= $.start-position cp))) note-count (length notes)] (table.sort notes (fn [{:pitch p1} {:pitch p2}] (> p1 p2))) (let [selected-idxs (icollect [i n (ipairs notes)] (if n.selected i))] (each [_ n (ipairs notes)] (transform-note n {:selected (fn [_] false)})) (each [_ i (ipairs selected-idxs)] (transform-note (. notes (+ 1 (% i note-count))) {:selected (fn [_] true)}))))) (fn channel-up-at-cursor [] "Increase the midi channel of the note at the cursor position." (transform-at-cursor {:channel (hashfn (+ 1 $))})) ]]
return {take = take, note = note, misc = misc, ["midi-editor"] = midi_editor, time = time}
