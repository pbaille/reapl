-- @noindex
local function reload(p)
  package.loaded[p] = nil
  return require(p)
end
pcall(function() require("fennel").metadata:setall(reload, "fnl/arglist", {"p"}, "fnl/docstring", "Reload the given package `p`.") end)
local path = {}
local file = {}
local seq = {}
local tbl = {}
local hof = {}
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
  _G.assert((nil ~= content), "Missing argument content on ./src/fennel/pb-utils.fnl:37")
  _G.assert((nil ~= path0), "Missing argument path on ./src/fennel/pb-utils.fnl:37")
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
tbl.path = function(x)
  if (type(x) == "string") then
    local function _7_()
      local parts = {}
      for part in x:gmatch("[^%.%:]+[%.%:]?") do
        local last_char = part:sub(-1)
        if (last_char == ":") then
          parts["multi-sym-method-call"] = true
        else
        end
        if ((last_char == ":") or (last_char == ".")) then
          parts[(#parts + 1)] = part:sub(1, -2)
        else
          parts[(#parts + 1)] = part
        end
      end
      return (next(parts) and parts)
    end
    return (_7_() or {x})
  elseif seq.seq(x) then
    local parts = seq.keep(x, tbl.path)
    if parts then
      local ret = {}
      for _, p in ipairs(parts) do
        seq.concat(ret, p)
      end
      return ret
    else
      return nil
    end
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(tbl.path, "fnl/arglist", {"x"}, "fnl/docstring", "Returns a table containing the symbol's segments if passed a multi-sym.\nA multi-sym refers to a table field reference like tbl.x or access.channel:deny.\nReturns nil if passed something other than a multi-sym.") end)
tbl.matcher = function(m)
  local _12_ = type(m)
  if (_12_ == "function") then
    return m
  elseif (_12_ == "table") then
    local function _13_(t)
      local ok = true
      for km, vm in pairs(m) do
        local function _14_(...)
          local _15_ = t[km]
          if (nil ~= _15_) then
            local vt = _15_
            local _16_ = type(vm)
            if (_16_ == "function") then
              return vm(vt)
            else
              local _ = _16_
              return (vm == vt)
            end
          else
            return nil
          end
        end
        pcall(function() require("fennel").metadata:setall(_14_, "fnl/arglist", {"..."}) end)
        ok = (ok and _14_())
      end
      return ok
    end
    pcall(function() require("fennel").metadata:setall(_13_, "fnl/arglist", {"t"}) end)
    return _13_
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(tbl.matcher, "fnl/arglist", {"m"}, "fnl/docstring", "Return a matcher function for the table `m`.") end)
tbl.getter = function(at)
  local _20_ = type(at)
  if (_20_ == "string") then
    local function _21_(this)
      local t_22_ = this
      if (nil ~= t_22_) then
        t_22_ = t_22_[at]
      else
      end
      return t_22_
    end
    pcall(function() require("fennel").metadata:setall(_21_, "fnl/arglist", {"this"}) end)
    return _21_
  elseif (_20_ == "table") then
    local function _24_(this)
      local this0 = this
      for _, k in ipairs(at) do
        local t_25_ = this0
        if (nil ~= t_25_) then
          t_25_ = t_25_[k]
        else
        end
        this0 = t_25_
      end
      return this0
    end
    pcall(function() require("fennel").metadata:setall(_24_, "fnl/arglist", {"this"}) end)
    return _24_
  else
    local _ = _20_
    local function _27_(_0)
      return nil
    end
    pcall(function() require("fennel").metadata:setall(_27_, "fnl/arglist", {"_"}) end)
    return _27_
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
    local _29_ = type(u)
    if (_29_ == "function") then
      t[k] = u(t[k])
    else
      local _ = _29_
      t[k] = u
    end
  end
  return t
end
pcall(function() require("fennel").metadata:setall(tbl["upd-at"], "fnl/arglist", {"t", "k", "u"}, "fnl/docstring", "Update the key `k` in table `t` with the value `u`.") end)
tbl.upd = function(t, u)
  do
    local _31_ = type(u)
    if (_31_ == "table") then
      for k, f in pairs(u) do
        tbl["upd-at"](t, k, f)
      end
    elseif (_31_ == "function") then
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
seq.seq = function(t)
  return (("table" == type(t)) and (#t > 0) and t)
end
pcall(function() require("fennel").metadata:setall(seq.seq, "fnl/arglist", {"t"}) end)
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
  local function _39_(x)
    if f(x) then
      return x
    else
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(_39_, "fnl/arglist", {"x"}) end)
  return seq.keep(s, _39_)
end
pcall(function() require("fennel").metadata:setall(seq.filter, "fnl/arglist", {"s", "f"}, "fnl/docstring", "Filter elements of sequence `s` using the function `f`.") end)
seq.remove = function(s, f)
  local function _41_(x)
    if not f(x) then
      return x
    else
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(_41_, "fnl/arglist", {"x"}) end)
  return seq.keep(s, _41_)
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
seq.fold = function(xs, f, x)
  local x0 = x
  for _, e in ipairs(xs) do
    x0 = f(x0, e)
  end
  return x0
end
pcall(function() require("fennel").metadata:setall(seq.fold, "fnl/arglist", {"xs", "f", "x"}, "fnl/docstring", "Accumulate a result over the sequence `xs`, starting with `x` and applying the function `f`.") end)
seq.sort = function(s, key_fn, compare_fn)
  if not key_fn then
    table.sort(s)
  elseif not compare_fn then
    seq["sort-by"](s, key_fn)
  else
    local function _44_(a, b)
      return compare_fn(key_fn(a), key_fn(b))
    end
    pcall(function() require("fennel").metadata:setall(_44_, "fnl/arglist", {"a", "b"}) end)
    table.sort(s, _44_)
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
  local function _46_(a, b)
    return (key_fn(a) < key_fn(b))
  end
  pcall(function() require("fennel").metadata:setall(_46_, "fnl/arglist", {"a", "b"}) end)
  seq["sort-with"](s, _46_)
  return s
end
pcall(function() require("fennel").metadata:setall(seq["sort-by"], "fnl/arglist", {"s", "key-fn"}, "fnl/docstring", "Sort sequence `s` by the key function `key-fn`.") end)
seq["reverse-sort-by"] = function(s, key_fn)
  local function _47_(a, b)
    return (key_fn(a) > key_fn(b))
  end
  pcall(function() require("fennel").metadata:setall(_47_, "fnl/arglist", {"a", "b"}) end)
  seq["sort-with"](s, _47_)
  return s
end
pcall(function() require("fennel").metadata:setall(seq["reverse-sort-by"], "fnl/arglist", {"s", "key-fn"}, "fnl/docstring", "Sort sequence `s` in reverse order by the key function `key-fn`.") end)
seq.interpose = function(s, elem)
  local len = #s
  local ret = {}
  for i, v in ipairs(s) do
    table.insert(ret, v)
    if (i < len) then
      table.insert(ret, elem)
    else
    end
  end
  return ret
end
pcall(function() require("fennel").metadata:setall(seq.interpose, "fnl/arglist", {"s", "elem"}) end)
local function _49_(_241)
  return not _241
end
hof["not"] = _49_
hof.k = function(x)
  local function _50_(_)
    return x
  end
  pcall(function() require("fennel").metadata:setall(_50_, "fnl/arglist", {"_"}) end)
  return _50_
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
  local function _51_(y)
    return (x + y)
  end
  pcall(function() require("fennel").metadata:setall(_51_, "fnl/arglist", {"y"}) end)
  return _51_
end
pcall(function() require("fennel").metadata:setall(hof.adder, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that adds `x` to its argument.") end)
hof.gt = function(x)
  local function _52_(y)
    return (y > x)
  end
  pcall(function() require("fennel").metadata:setall(_52_, "fnl/arglist", {"y"}) end)
  return _52_
end
pcall(function() require("fennel").metadata:setall(hof.gt, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that returns true if its argument is greater than `x`.") end)
hof.lt = function(x)
  local function _53_(y)
    return (y < x)
  end
  pcall(function() require("fennel").metadata:setall(_53_, "fnl/arglist", {"y"}) end)
  return _53_
end
pcall(function() require("fennel").metadata:setall(hof.lt, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that returns true if its argument is less than `x`.") end)
hof.gte = function(x)
  local function _54_(y)
    return (y >= x)
  end
  pcall(function() require("fennel").metadata:setall(_54_, "fnl/arglist", {"y"}) end)
  return _54_
end
pcall(function() require("fennel").metadata:setall(hof.gte, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that returns true if its argument is greater than or equal to `x`.") end)
hof.lte = function(x)
  local function _55_(y)
    return (y <= x)
  end
  pcall(function() require("fennel").metadata:setall(_55_, "fnl/arglist", {"y"}) end)
  return _55_
end
pcall(function() require("fennel").metadata:setall(hof.lte, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that returns true if its argument is less than or equal to `x`.") end)
return {path = path, file = file, tbl = tbl, hof = hof, reload = reload, fold = fold, seq = seq}
