-- @noindex
local function reload(p)
  package.loaded[p] = nil
  return require(p)
end
pcall(function() require("fennel").metadata:setall(reload, "fnl/arglist", {"p"}, "fnl/docstring", "Reload the given package `p`.") end)
local function fold(x, f, xs)
  local x0 = x
  for _, e in ipairs(xs) do
    x0 = f(x0, e)
  end
  return x0
end
pcall(function() require("fennel").metadata:setall(fold, "fnl/arglist", {"x", "f", "xs"}, "fnl/docstring", "Accumulate a result over the sequence `xs`, starting with `x` and applying the function `f`.") end)
local path = {}
path.pwd = function()
  return io.popen("pwd"):read()
end
pcall(function() require("fennel").metadata:setall(path.pwd, "fnl/arglist", {}, "fnl/docstring", "Get the current working directory.") end)
path.home = "/Users/pierrebaille"
path.user = (path.home .. "/Code/Lua")
path.relative = function(subpath)
  return (path.pwd() .. "/" .. subpath)
end
pcall(function() require("fennel").metadata:setall(path.relative, "fnl/arglist", {"subpath"}, "fnl/docstring", "Return the relative path by appending `subpath` to the current working directory.") end)
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
pcall(function() require("fennel").metadata:setall(file.slurp, "fnl/arglist", {"path"}, "fnl/docstring", "Read the contents of the file at `path`.") end)
file.spit = function(path0, content)
  _G.assert((nil ~= content), "Missing argument content on ./src/fennel/pb-utils.fnl:40")
  _G.assert((nil ~= path0), "Missing argument path on ./src/fennel/pb-utils.fnl:40")
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
pcall(function() require("fennel").metadata:setall(file.spit, "fnl/arglist", {"path", "content"}, "fnl/docstring", "Write `content` to the file at `path`.") end)
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
        pcall(function() require("fennel").metadata:setall(_9_, "fnl/arglist", {"..."}) end)
        ok = (ok and _9_())
      end
      return ok
    end
    pcall(function() require("fennel").metadata:setall(_8_, "fnl/arglist", {"t"}) end)
    return _8_
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(tbl.matcher, "fnl/arglist", {"m"}, "fnl/docstring", "Return a matcher function for the table `m`.") end)
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
    pcall(function() require("fennel").metadata:setall(_16_, "fnl/arglist", {"this"}) end)
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
    pcall(function() require("fennel").metadata:setall(_19_, "fnl/arglist", {"this"}) end)
    return _19_
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(tbl.getter, "fnl/arglist", {"at"}, "fnl/docstring", "Return a getter function for accessing elements in `at`.") end)
tbl.match = function(t, m)
  return tbl.matcher(m)(t)
end
pcall(function() require("fennel").metadata:setall(tbl.match, "fnl/arglist", {"t", "m"}, "fnl/docstring", "Check if table `t` matches the pattern `m`.") end)
tbl.get = function(t, p)
  return tbl.getter(p)(t)
end
pcall(function() require("fennel").metadata:setall(tbl.get, "fnl/arglist", {"t", "p"}, "fnl/docstring", "Get the value at path `p` in table `t`.") end)
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
pcall(function() require("fennel").metadata:setall(tbl["upd-at"], "fnl/arglist", {"t", "k", "u"}, "fnl/docstring", "Update the key `k` in table `t` with the value `u`.") end)
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
pcall(function() require("fennel").metadata:setall(tbl.upd, "fnl/arglist", {"t", "u"}, "fnl/docstring", "Update the table `t` with the updates in `u`.") end)
tbl.merge = function(a, b)
  for k, v in pairs(b) do
    a[k] = v
  end
  return a
end
pcall(function() require("fennel").metadata:setall(tbl.merge, "fnl/arglist", {"a", "b"}, "fnl/docstring", "Merge table `b` into table `a`.") end)
tbl.put = function(t, k, v)
  t[k] = v
  return t
end
pcall(function() require("fennel").metadata:setall(tbl.put, "fnl/arglist", {"t", "k", "v"}, "fnl/docstring", "Set key `k` in table `t` to value `v`.") end)
tbl.rem = function(t, k)
  t[k] = nil
  return t
end
pcall(function() require("fennel").metadata:setall(tbl.rem, "fnl/arglist", {"t", "k"}, "fnl/docstring", "Remove key `k` from table `t`.") end)
tbl.keys = function(t)
  local tbl_19_auto = {}
  local i_20_auto = 0
  for k, _ in pairs(t) do
    local val_21_auto = k
    if (nil ~= val_21_auto) then
      i_20_auto = (i_20_auto + 1)
      do end (tbl_19_auto)[i_20_auto] = val_21_auto
    else
    end
  end
  return tbl_19_auto
end
pcall(function() require("fennel").metadata:setall(tbl.keys, "fnl/arglist", {"t"}) end)
tbl.vals = function(t)
  local tbl_19_auto = {}
  local i_20_auto = 0
  for _, v in pairs(t) do
    local val_21_auto = v
    if (nil ~= val_21_auto) then
      i_20_auto = (i_20_auto + 1)
      do end (tbl_19_auto)[i_20_auto] = val_21_auto
    else
    end
  end
  return tbl_19_auto
end
pcall(function() require("fennel").metadata:setall(tbl.vals, "fnl/arglist", {"t"}) end)
tbl["="] = function(t1, t2)
  if not (#t1 == #t2) then
    return false
  else
    local equal = true
    for k, v1 in pairs(t1) do
      local v2 = t2[k]
      if ((("table" == type(v1)) and not __fnl_global__table_2dequal(v1, v2)) or not (v1 == v2)) then
        equal = false
      else
      end
    end
    return equal
  end
end
pcall(function() require("fennel").metadata:setall(tbl["="], "fnl/arglist", {"t1", "t2"}) end)
local seq = {}
seq.first = function(s)
  return s[1]
end
pcall(function() require("fennel").metadata:setall(seq.first, "fnl/arglist", {"s"}, "fnl/docstring", "Get the first element of the sequence `s`.") end)
seq.last = function(s)
  return s[#s]
end
pcall(function() require("fennel").metadata:setall(seq.last, "fnl/arglist", {"s"}, "fnl/docstring", "Get the last element of the sequence `s`.") end)
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
pcall(function() require("fennel").metadata:setall(seq["index-of"], "fnl/arglist", {"s", "v"}, "fnl/docstring", "Get the index of value `v` in the sequence `s`.") end)
seq.append = function(s, x)
  table.insert(s, x)
  return s
end
pcall(function() require("fennel").metadata:setall(seq.append, "fnl/arglist", {"s", "x"}, "fnl/docstring", "Append value `x` to the sequence `s`.") end)
seq.concat = function(s, xs)
  for _, x in ipairs(xs) do
    seq.append(s, x)
  end
  return s
end
pcall(function() require("fennel").metadata:setall(seq.concat, "fnl/arglist", {"s", "xs"}, "fnl/docstring", "Concatenate sequences `s` and `xs`.") end)
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
pcall(function() require("fennel").metadata:setall(seq.keep, "fnl/arglist", {"s", "f"}, "fnl/docstring", "Keep elements of sequence `s` that satisfy the function `f`.") end)
seq.filter = function(s, f)
  local function _33_(x)
    if f(x) then
      return x
    else
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(_33_, "fnl/arglist", {"x"}) end)
  return seq.keep(s, _33_)
end
pcall(function() require("fennel").metadata:setall(seq.filter, "fnl/arglist", {"s", "f"}, "fnl/docstring", "Filter elements of sequence `s` using the function `f`.") end)
seq.remove = function(s, f)
  local function _35_(x)
    if not f(x) then
      return x
    else
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(_35_, "fnl/arglist", {"x"}) end)
  return seq.keep(s, _35_)
end
pcall(function() require("fennel").metadata:setall(seq.remove, "fnl/arglist", {"s", "f"}, "fnl/docstring", "Remove elements of sequence `s` that satisfy the function `f`.") end)
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
pcall(function() require("fennel").metadata:setall(seq.find, "fnl/arglist", {"s", "f"}, "fnl/docstring", "Find the first element in sequence `s` that satisfies the function `f`.") end)
seq.sort = function(s, key_fn, compare_fn)
  if not key_fn then
    table.sort(s)
  elseif not compare_fn then
    seq["sort-by"](s, key_fn)
  else
    local function _38_(a, b)
      return compare_fn(key_fn(a), key_fn(b))
    end
    pcall(function() require("fennel").metadata:setall(_38_, "fnl/arglist", {"a", "b"}) end)
    table.sort(s, _38_)
  end
  return s
end
pcall(function() require("fennel").metadata:setall(seq.sort, "fnl/arglist", {"s", "key-fn", "compare-fn"}, "fnl/docstring", "Sort sequence `s` using `key-fn` and `compare-fn`.") end)
seq["sort-with"] = function(s, f)
  table.sort(s, f)
  return s
end
pcall(function() require("fennel").metadata:setall(seq["sort-with"], "fnl/arglist", {"s", "f"}, "fnl/docstring", "Sort sequence `s` using the function `f`.") end)
seq["sort-by"] = function(s, key_fn)
  local function _40_(a, b)
    return (key_fn(a) < key_fn(b))
  end
  pcall(function() require("fennel").metadata:setall(_40_, "fnl/arglist", {"a", "b"}) end)
  seq["sort-with"](s, _40_)
  return s
end
pcall(function() require("fennel").metadata:setall(seq["sort-by"], "fnl/arglist", {"s", "key-fn"}, "fnl/docstring", "Sort sequence `s` by the key function `key-fn`.") end)
seq["reverse-sort-by"] = function(s, key_fn)
  local function _41_(a, b)
    return (key_fn(a) > key_fn(b))
  end
  pcall(function() require("fennel").metadata:setall(_41_, "fnl/arglist", {"a", "b"}) end)
  seq["sort-with"](s, _41_)
  return s
end
pcall(function() require("fennel").metadata:setall(seq["reverse-sort-by"], "fnl/arglist", {"s", "key-fn"}, "fnl/docstring", "Sort sequence `s` in reverse order by the key function `key-fn`.") end)
local hof = {}
local function _42_(_241)
  return not _241
end
hof["not"] = _42_
hof.k = function(x)
  local function _43_(_)
    return x
  end
  pcall(function() require("fennel").metadata:setall(_43_, "fnl/arglist", {"_"}) end)
  return _43_
end
pcall(function() require("fennel").metadata:setall(hof.k, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that always returns `x`.") end)
hof.inc = function(x)
  return (1 + x)
end
pcall(function() require("fennel").metadata:setall(hof.inc, "fnl/arglist", {"x"}, "fnl/docstring", "Increment `x` by 1.") end)
hof.dec = function(x)
  return (1 - x)
end
pcall(function() require("fennel").metadata:setall(hof.dec, "fnl/arglist", {"x"}, "fnl/docstring", "Decrement `x` by 1.") end)
hof.adder = function(x)
  local function _44_(y)
    return (x + y)
  end
  pcall(function() require("fennel").metadata:setall(_44_, "fnl/arglist", {"y"}) end)
  return _44_
end
pcall(function() require("fennel").metadata:setall(hof.adder, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that adds `x` to its argument.") end)
hof.gt = function(x)
  local function _45_(y)
    return (y > x)
  end
  pcall(function() require("fennel").metadata:setall(_45_, "fnl/arglist", {"y"}) end)
  return _45_
end
pcall(function() require("fennel").metadata:setall(hof.gt, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that returns true if its argument is greater than `x`.") end)
hof.lt = function(x)
  local function _46_(y)
    return (y < x)
  end
  pcall(function() require("fennel").metadata:setall(_46_, "fnl/arglist", {"y"}) end)
  return _46_
end
pcall(function() require("fennel").metadata:setall(hof.lt, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that returns true if its argument is less than `x`.") end)
hof.gte = function(x)
  local function _47_(y)
    return (y >= x)
  end
  pcall(function() require("fennel").metadata:setall(_47_, "fnl/arglist", {"y"}) end)
  return _47_
end
pcall(function() require("fennel").metadata:setall(hof.gte, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that returns true if its argument is greater than or equal to `x`.") end)
hof.lte = function(x)
  local function _48_(y)
    return (y <= x)
  end
  pcall(function() require("fennel").metadata:setall(_48_, "fnl/arglist", {"y"}) end)
  return _48_
end
pcall(function() require("fennel").metadata:setall(hof.lte, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that returns true if its argument is less than or equal to `x`.") end)
return {path = path, file = file, tbl = tbl, hof = hof, reload = reload, fold = fold, seq = seq}
