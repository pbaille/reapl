-- @noindex
local function reload(p)
  package.loaded[p] = nil
  return require(p)
end
local function fold(x, f, xs)
  local x0 = x
  for _, e in ipairs(xs) do
    x0 = f(x0, e)
  end
  return x0
end
local path = {}
path.pwd = function()
  return io.popen("pwd"):read()
end
path.home = "/Users/pierrebaille"
path.user = (path.home .. "/Code/Lua")
path.relative = function(subpath)
  return (path.pwd() .. "/" .. subpath)
end
local file = {}
file.slurp = function(path0)
  local _1_, _2_ = io.open(path0)
  if (nil ~= _1_) then
    local f = _1_
    local content = f:read("*all")
    f:close()
    return content
  elseif ((_1_ == nil) and (nil ~= _2_)) then
    local err_msg = _2_
    return print("Could not open file:", err_msg)
  else
    return nil
  end
end
file.spit = function(path0, content)
  _G.assert((nil ~= content), "Missing argument content on ./src/fennel/pb-utils.fnl:36")
  _G.assert((nil ~= path0), "Missing argument path on ./src/fennel/pb-utils.fnl:36")
  local _4_, _5_ = io.open(path0, "w")
  if (nil ~= _4_) then
    local f = _4_
    f:write(content)
    return f:close()
  elseif ((_4_ == nil) and (nil ~= _5_)) then
    local err_msg = _5_
    return print("Could not open file:", err_msg)
  else
    return nil
  end
end
local tbl = {}
tbl.matcher = function(m)
  local _7_ = type(m)
  if (_7_ == "function") then
    return m
  elseif (_7_ == "table") then
    local function _8_(t)
      local ok = true
      for km, vm in pairs(m) do
        local function _9_(...)
          local _10_ = t[km]
          if (nil ~= _10_) then
            local vt = _10_
            local _11_ = type(vm)
            if (_11_ == "function") then
              return vm(vt)
            else
              local _ = _11_
              return (vm == vt)
            end
          else
            return nil
          end
        end
        ok = (ok and _9_())
      end
      return ok
    end
    return _8_
  else
    return nil
  end
end
tbl.getter = function(at)
  local _15_ = type(at)
  if (_15_ == "string") then
    local function _16_(this)
      local t_17_ = this
      if (nil ~= t_17_) then
        t_17_ = t_17_[at]
      else
      end
      return t_17_
    end
    return _16_
  elseif (_15_ == "table") then
    local function _19_(this)
      local this0 = this
      for _, k in ipairs(at) do
        local t_20_ = this0
        if (nil ~= t_20_) then
          t_20_ = t_20_[k]
        else
        end
        this0 = t_20_
      end
      return this0
    end
    return _19_
  else
    return nil
  end
end
tbl.match = function(t, m)
  return tbl.matcher(m)(t)
end
tbl.get = function(t, p)
  return tbl.getter(p)(t)
end
tbl["upd-at"] = function(t, k, u)
  do
    local _23_ = type(u)
    if (_23_ == "function") then
      t[k] = u(t[k])
    else
      local _ = _23_
      t[k] = u
    end
  end
  return t
end
tbl.upd = function(t, u)
  do
    local _25_ = type(u)
    if (_25_ == "table") then
      for k, f in pairs(u) do
        tbl["upd-at"](t, k, f)
      end
    elseif (_25_ == "function") then
      u(t)
    else
    end
  end
  return t
end
tbl.merge = function(a, b)
  for k, v in pairs(b) do
    a[k] = v
  end
  return a
end
tbl.put = function(t, k, v)
  t[k] = v
  return t
end
tbl.rem = function(t, k)
  t[k] = nil
  return t
end
local seq = {}
seq.first = function(s)
  return s[1]
end
seq.last = function(s)
  return s[#s]
end
seq["index-of"] = function(s, v)
  local idx = nil
  for i, x in ipairs(s) do
    if idx then break end
    if (x == v) then
      idx = i
    else
    end
  end
  return idx
end
seq.append = function(s, x)
  table.insert(s, x)
  return s
end
seq.concat = function(s, xs)
  for _, x in ipairs(xs) do
    seq.append(s, x)
  end
  return s
end
seq.keep = function(s, f)
  local tbl_19_auto = {}
  local i_20_auto = 0
  for _, x in ipairs(s) do
    local val_21_auto = f(x)
    if (nil ~= val_21_auto) then
      i_20_auto = (i_20_auto + 1)
      do end (tbl_19_auto)[i_20_auto] = val_21_auto
    else
    end
  end
  return tbl_19_auto
end
seq.filter = function(s, f)
  local function _29_(x)
    if f(x) then
      return x
    else
      return nil
    end
  end
  return seq.keep(s, _29_)
end
seq.remove = function(s, f)
  local function _31_(x)
    if not f(x) then
      return x
    else
      return nil
    end
  end
  return seq.keep(s, _31_)
end
seq.find = function(s, f)
  local found = nil
  for _, x in ipairs(s) do
    if found then break end
    if f(x) then
      found = x
    else
    end
  end
  return found
end
seq.sort = function(s, key_fn, compare_fn)
  if not key_fn then
    table.sort(s)
  elseif not compare_fn then
    seq["sort-by"](s, key_fn)
  else
    local function _34_(a, b)
      return compare_fn(key_fn(a), key_fn(b))
    end
    table.sort(s, _34_)
  end
  return s
end
seq["sort-with"] = function(s, f)
  table.sort(s, f)
  return s
end
seq["sort-by"] = function(s, key_fn)
  local function _36_(a, b)
    return (key_fn(a) < key_fn(b))
  end
  seq["sort-with"](s, _36_)
  return s
end
seq["reverse-sort-by"] = function(s, key_fn)
  local function _37_(a, b)
    return (key_fn(a) > key_fn(b))
  end
  seq["sort-with"](s, _37_)
  return s
end
local hof = {}
local function _38_(_241)
  return not _241
end
hof["not"] = _38_
hof.k = function(x)
  local function _39_(_)
    return x
  end
  return _39_
end
hof.inc = function(x)
  return (1 + x)
end
hof.dec = function(x)
  return (1 - x)
end
hof.adder = function(x)
  local function _40_(y)
    return (x + y)
  end
  return _40_
end
hof.gt = function(x)
  local function _41_(y)
    return (y > x)
  end
  return _41_
end
hof.lt = function(x)
  local function _42_(y)
    return (y < x)
  end
  return _42_
end
hof.gte = function(x)
  local function _43_(y)
    return (y >= x)
  end
  return _43_
end
hof.lte = function(x)
  local function _44_(y)
    return (y <= x)
  end
  return _44_
end
return {path = path, file = file, tbl = tbl, hof = hof, reload = reload, fold = fold, seq = seq}
