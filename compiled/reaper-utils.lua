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
local misc = {}
misc.log = function(param)
  return reaper.ShowConsoleMsg((tostring(param) .. "\n"))
end
misc.pp = function(param)
  return reaper.ShowConsoleMsg(pp.pformat(param))
end
local midi_editor = {["pitch-cursor"] = {}}
midi_editor["get-active"] = function()
  return r.MIDIEditor_GetActive()
end
midi_editor["get-take"] = function(me)
  return r.MIDIEditor_GetTake(me)
end
midi_editor["pitch-cursor"].get = function(me)
  return reaper.MIDIEditor_GetSetting_int(me, "active_note_row")
end
midi_editor["pitch-cursor"].set = function(me, i)
  return reaper.MIDIEditor_SetSetting_int(me, "active_note_row", i)
end
midi_editor["pitch-cursor"].update = function(me, delta)
  return midi_editor["pitch-cursor"].set(me, (delta + midi_editor["pitch-cursor"].get(me)))
end
local note = {}
note["shift-position"] = function(n, offset)
  return tbl.upd(n, {["start-position"] = hof.adder(offset), ["end-position"] = hof.adder(offset)})
end
note.lte = function(a, b)
  if (a["start-position"] == b["start-position"]) then
    return (a.pitch < b.pitch)
  else
    return (a["start-position"] < b["start-position"])
  end
end
note.gte = function(a, b)
  if (a["start-position"] == b["start-position"]) then
    return (a.pitch > b.pitch)
  else
    return (a["start-position"] > b["start-position"])
  end
end
local take = {grid = {}, ["time-selection"] = {}, ["note-selection"] = {}, cursor = {}, focus = {}, note = {}, notes = {}, ccs = {}}
take["get-active"] = function()
  return midi_editor["get-take"](midi_editor["get-active"]())
end
take["get-ppq"] = function(t)
  return math.floor(r.MIDI_GetPPQPosFromProjQN(t, 1))
end
take["ppq->qpos"] = function(t, x)
  return (x / take["get-ppq"](t))
end
take["qpos->ppq"] = function(t, x)
  return (x * take["get-ppq"](t))
end
take["mark-dirty"] = function(t)
  r.MarkTrackItemsDirty(r.GetMediaItemTake_Track(t), r.GetMediaItemTake_Item(t))
  return "ok"
end
take.sort = function(t)
  r.MIDI_Sort(t)
  return "ok"
end
take["disable-sort"] = function(t)
  r.MIDI_DisableSort(t)
  return "ok"
end
take.note.default = function(t)
  return {channel = 1, pitch = 60, velocity = 80, ["start-position"] = 0, ["end-position"] = take["get-ppq"](t), selected = true, muted = false}
end
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
take.note.mk = function(t, n)
  if (n.position and n.duration) then
    return take.note.mk(t, take.note["to-absolute-position"](t, n))
  else
    return tbl.merge(take.note.default(t), n)
  end
end
take["project-time->ppq"] = function(t, x)
  return reaper.MIDI_GetPPQPosFromProjTime(t, x)
end
take["ppq->project-time"] = function(t, x)
  return reaper.MIDI_GetProjTimeFromPPQPos(t, x)
end
take["project-time->qpos"] = function(t, x)
  return take["ppq->qpos"](t, take["project-time->ppq"](t, x))
end
take["qpos->project-time"] = function(t, x)
  return take["ppq->project-time"](t, take["qpos->ppq"](t, x))
end
take.grid.get = function(t)
  return reaper.MIDI_GetGrid(t)
end
take.grid["get-ppq"] = function(t)
  return take["qpos->ppq"](t, reaper.MIDI_GetGrid(t))
end
take.grid.set = function(t, x)
  local sig = time.signature.get()
  return reaper.SetMIDIEditorGrid(0, (x / sig.bpi))
end
take["time-selection"].get = function(t)
  local start, _end = reaper.GetSet_LoopTimeRange(false, false, 0, 0, false)
  if not (start == _end) then
    return {start = take["project-time->ppq"](t, start), ["end"] = take["project-time->ppq"](t, _end)}
  else
    return nil
  end
end
take["time-selection"].set = function(t, start, _end)
  local start0 = take["ppq->project-time"](t, start)
  local _end0 = take["ppq->project-time"](t, _end)
  return reaper.GetSet_LoopTimeRange(true, false, start0, _end0, false)
end
take["time-selection"]["get-qpos"] = function(t)
  local _let_7_ = take["time-selection"].get(t)
  local start = _let_7_["start"]
  local _end = _let_7_["end"]
  return {start = take["ppq->qpos"](t, start), ["end"] = take["ppq->qpos"](t, _end)}
end
take["time-selection"]["set-qpos"] = function(t, start, _end)
  return take["time-selection"].set(t, take["qpos->ppq"](t, start), take["qpos->ppq"](t, _end))
end
take["time-selection"].update = function(t, side, delta)
  local cursor_pos = take.cursor.get(t)
  local sel = (take["time-selection"].get(t) or {start = cursor_pos, ["end"] = cursor_pos})
  local increment = take["qpos->ppq"](t, (delta * take.grid.get(t)))
  do
    local _8_ = side
    if (_8_ == "fw") then
      take["time-selection"].set(t, sel.start, (sel["end"] + increment))
    elseif (_8_ == "bw") then
      take["time-selection"].set(t, (sel.start + increment), sel["end"])
    elseif true then
      local _ = _8_
      take["time-selection"].set(t, (sel.start + increment), (sel["end"] + increment))
    else
    end
  end
  return "ok"
end
local NOON_MIDI_NOTE_SHIFT = 2
take["note-selection"].get = function(t)
  local notes = take.notes.get(t)
  local selected_notes
  local function _10_(n)
    return n.selected
  end
  selected_notes = seq.filter(notes, _10_)
  local candidates
  if (0 == #selected_notes) then
    candidates = notes
  else
    candidates = selected_notes
  end
  local time_selection = take["time-selection"].get(t)
  local _12_ = time_selection
  if ((_G.type(_12_) == "table") and (nil ~= (_12_).start) and (nil ~= (_12_)["end"])) then
    local start = (_12_).start
    local _end = (_12_)["end"]
    local function _13_(n)
      return (function(_14_,_15_,_16_,_17_) return (_14_ <= _15_) and (_15_ <= _16_) and (_16_ <= _17_) end)(start,n["start-position"],n["end-position"],(_end + NOON_MIDI_NOTE_SHIFT))
    end
    return seq.filter(candidates, _13_)
  elseif true then
    local _ = _12_
    return candidates
  else
    return nil
  end
end
take["note-selection"]["delete-all"] = function(t)
  return take["delete-notes"](t, take["note-selection"].get(t))
end
take.cursor.get = function(t)
  local curs_pos = r.GetCursorPosition()
  return take["project-time->ppq"](t, curs_pos)
end
take.cursor.set = function(t, p)
  return reaper.SetEditCurPos(ru.take["ppq->project-time"](t, p), true, false)
end
take.cursor["get-qpos"] = function(t)
  return take["ppq->qpos"](t, take.cursor.get(t))
end
take.cursor["set-qpos"] = function(t, p)
  return take.cursor.set(t, take["qpos->ppq"](t, p))
end
take.cursor.ceil = function(t)
  return take.cursor.set(t, reaper.BR_GetNextGridDivision(take.cursor.get(t)))
end
take.cursor.floor = function(t)
  return take.cursor.set(t, reaper.BR_GetPrevGridDivision(take.cursor.get(t)))
end
take.cursor.round = function(t)
  return take.cursor.set(t, reaper.BR_GetClosestGridDivision(take.cursor.get(t)))
end
take.cursor.update = function(t, delta)
  local increment = take["qpos->ppq"](t, (delta * take.grid.get(t)))
  return take.cursor.set(t, (take.cursor.get(t) + increment))
end
take.focus.get = function(t)
  return {x = take.cursor.get(t), y = midi_editor["pitch-cursor"].get(midi_editor["get-active"]())}
end
take.focus.set = function(t, upd)
  local _let_19_ = tbl.upd(take.focus.get(t), upd)
  local x = _let_19_["x"]
  local y = _let_19_["y"]
  local new_focus = _let_19_
  take.cursor.set(t, x)
  midi_editor["pitch-cursor"].set(midi_editor["get-active"](), y)
  return new_focus
end
take.focus["next-note"] = function(t)
  local _let_20_ = take.focus.get(t)
  local x = _let_20_["x"]
  local y = _let_20_["y"]
  local candidates
  local function _21_(n)
    return ((n["start-position"] > x) or ((n["start-position"] == x) and (n.pitch > y)))
  end
  candidates = take.notes.filter(t, _21_)
  return take["focus-note"](t, seq.first(seq["sort-with"](candidates, note.lte)))
end
take.focus["previous-note"] = function(t)
  local _let_22_ = take.focus.get(t)
  local x = _let_22_["x"]
  local y = _let_22_["y"]
  local candidates
  local function _23_(n)
    return ((n["start-position"] < x) or ((n["start-position"] == x) and (n.pitch < y)))
  end
  candidates = take.notes.filter(t, _23_)
  return take["focus-note"](t, seq.first(seq["sort-with"](candidates, note.gte)))
end
take.focus["get-closest-note"] = function(t)
  local _let_24_ = take.focus.get(t)
  local x = _let_24_["x"]
  local y = _let_24_["y"]
  local notes = take.notes.get(t)
  local by_dist
  local function _25_(n)
    return {["delta-x"] = math.abs((n["start-position"] - x)), ["delta-y"] = math.abs((n.pitch - y)), note = n}
  end
  local function _28_(_26_)
    local _arg_27_ = _26_
    local delta_x = _arg_27_["delta-x"]
    local delta_y = _arg_27_["delta-y"]
    return (delta_x + delta_y)
  end
  by_dist = seq["sort-by"](seq.keep(notes, _25_), _28_)
  local t_29_ = seq.first(by_dist)
  if (nil ~= t_29_) then
    t_29_ = (t_29_).note
  else
  end
  return t_29_
end
take.focus["closest-note"] = function(t)
  return take["focus-note"](t, take.focus["get-closest-note"](t))
end
take.focus["cycle-at-cursor"] = function(t)
  local _let_31_ = take.focus.get(t)
  local x = _let_31_["x"]
  local y = _let_31_["y"]
  local candidates
  local function _32_(n)
    return (n["start-position"] == x)
  end
  candidates = take.notes.filter(t, _32_)
  local n_choices = #candidates
  local current_idx = seq["index-of"](candidates, y)
  if not current_idx then
    return take.focus.set(t, {x = x, y = seq.first(candidates)})
  elseif (n_choices > 1) then
    local _33_
    if (current_idx == n_choices) then
      _33_ = 1
    else
      _33_ = (1 + current_idx)
    end
    return take.focus.set(t, {x = x, y = candidates[_33_]})
  else
    return nil
  end
end
take["get-note"] = function(t, idx)
  local _, selected, muted, start_position, end_position, channel, pitch, velocity = r.MIDI_GetNote(t, idx)
  return {channel = channel, ["end-position"] = end_position, muted = muted, pitch = pitch, selected = selected, ["start-position"] = start_position, velocity = velocity, idx = idx}
end
take["focus-note"] = function(t, n)
  if n then
    return take.focus.set(t, {x = n["start-position"], y = n.pitch})
  else
    return nil
  end
end
take["focused-note"] = function(t)
  local _let_37_ = take.focus.get(t)
  local x = _let_37_["x"]
  local y = _let_37_["y"]
  local function _38_(n)
    return ((x == n["start-position"]) and (y == n.pitch))
  end
  return seq.find(take.notes.get(t), _38_)
end
take["set-note"] = function(t, _39_)
  local _arg_40_ = _39_
  local channel = _arg_40_["channel"]
  local end_position = _arg_40_["end-position"]
  local muted = _arg_40_["muted"]
  local pitch = _arg_40_["pitch"]
  local selected = _arg_40_["selected"]
  local start_position = _arg_40_["start-position"]
  local velocity = _arg_40_["velocity"]
  local idx = _arg_40_["idx"]
  return r.MIDI_SetNote(t, idx, selected, muted, start_position, end_position, channel, pitch, velocity, true)
end
take["upd-note"] = function(t, n, u0)
  return take["set-note"](t, tbl.upd(n, u0))
end
take["delete-note"] = function(t, idx)
  return r.MIDI_DeleteNote(t, idx)
end
take["delete-notes"] = function(t, xs)
  local idxs
  do
    local idxs0
    local function _41_(n)
      return n.idx
    end
    idxs0 = seq.keep(xs, _41_)
    local function _42_(a, b)
      return (a > b)
    end
    idxs = seq["sort-with"](idxs0, _42_)
  end
  take["disable-sort"](t)
  for _, i in ipairs(idxs) do
    take["delete-note"](t, i)
  end
  return take.sort(t)
end
take["insert-note"] = function(t, n)
  local _let_43_ = take.note.mk(t, n)
  local channel = _let_43_["channel"]
  local end_position = _let_43_["end-position"]
  local muted = _let_43_["muted"]
  local pitch = _let_43_["pitch"]
  local selected = _let_43_["selected"]
  local start_position = _let_43_["start-position"]
  local velocity = _let_43_["velocity"]
  local idx = take.notes.count(t)
  r.MIDI_InsertNote(t, selected, muted, start_position, end_position, channel, pitch, velocity, true)
  return take["get-note"](t, idx)
end
take["insert-notes"] = function(t, xs)
  take["disable-sort"](t)
  for _, n in ipairs(xs) do
    take["insert-note"](t, n)
  end
  return take.sort(t)
end
take.notes.count = function(t)
  local _, notecnt, _0, _1 = r.MIDI_CountEvts(t)
  return notecnt
end
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
take.notes.upd = function(t, u0)
  take["disable-sort"](t)
  for _, n in ipairs(take.notes.get(t)) do
    take["upd-note"](t, n, u0)
  end
  return take.sort(t)
end
take.notes.filter = function(t, matcher)
  return seq.filter(take.notes.get(t), tbl.matcher(matcher))
end
take.notes["filtered-upd"] = function(t, matcher, u0)
  for _, n in ipairs(take.notes.filter(t, matcher)) do
    take["upd-note"](t, n, u0)
  end
  return take.sort()
end
take.ccs.count = function(t)
  local _, _0, cc_cnt, _1 = r.MIDI_CountEvts(t)
  return cc_cnt
end
--[[ (fn cycle-select-at-cursor [] (let [cp (cursor-position (active-take)) notes (filter-notes (hashfn (= $.start-position cp))) note-count (length notes)] (table.sort notes (fn [{:pitch p1} {:pitch p2}] (> p1 p2))) (let [selected-idxs (icollect [i n (ipairs notes)] (if n.selected i))] (each [_ n (ipairs notes)] (transform-note n {:selected (fn [_] false)})) (each [_ i (ipairs selected-idxs)] (transform-note (. notes (+ 1 (% i note-count))) {:selected (fn [_] true)}))))) (comment pouet) (fn channel-up-at-cursor [] (transform-at-cursor {:channel (hashfn (+ 1 $))})) ]]
return {take = take, note = note, misc = misc, ["midi-editor"] = midi_editor, time = time}
