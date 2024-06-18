-- @noindex
local function reload(p)
  package.loaded[p] = nil
  return require(p)
end
pcall(function() require("fennel").metadata:setall(reload, "fnl/arglist", {"p"}, "fnl/docstring", "Reload the given package `p`.") end)
local function clone(x)
  if ("table" == type(x)) then
    if (#x > 0) then
      local tbl_17_auto = {}
      for _, v in ipairs(x) do
        local val_18_auto = clone(v)
        table.insert(tbl_17_auto, val_18_auto)
      end
      return tbl_17_auto
    else
      local tbl_14_auto = {}
      for k, v in pairs(x) do
        local k_15_auto, v_16_auto = k, clone(v)
        if ((k_15_auto ~= nil) and (v_16_auto ~= nil)) then
          tbl_14_auto[k_15_auto] = v_16_auto
        else
        end
      end
      return tbl_14_auto
    end
  else
    return x
  end
end
pcall(function() require("fennel").metadata:setall(clone, "fnl/arglist", {"x"}) end)
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
  local _4_, _5_ = io.open(path0)
  if (nil ~= _4_) then
    local f = _4_
    local content = f:read("*all")
    f:close()
    return content
  elseif ((_4_ == nil) and (nil ~= _5_)) then
    local err_msg = _5_
    return print("Could not open file:", err_msg)
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(file.slurp, "fnl/arglist", {"path"}, "fnl/docstring", "Read the contents of the file at `path`.") end)
file.spit = function(path0, content)
  _G.assert((nil ~= content), "Missing argument content on ./src/fennel/pb-utils.fnl:46")
  _G.assert((nil ~= path0), "Missing argument path on ./src/fennel/pb-utils.fnl:46")
  local _7_, _8_ = io.open(path0, "w")
  if (nil ~= _7_) then
    local f = _7_
    f:write(content)
    return f:close()
  elseif ((_7_ == nil) and (nil ~= _8_)) then
    local err_msg = _8_
    return print("Could not open file:", err_msg)
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(file.spit, "fnl/arglist", {"path", "content"}, "fnl/docstring", "Write `content` to the file at `path`.") end)
tbl.path = function(x)
  if (type(x) == "string") then
    local function _10_()
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
    return (_10_() or {x})
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
  local _15_ = type(m)
  if (_15_ == "function") then
    return m
  elseif (_15_ == "table") then
    local function _16_(t)
      local ok = true
      for km, vm in pairs(m) do
        local function _17_(...)
          local _18_ = t[km]
          if (nil ~= _18_) then
            local vt = _18_
            local _19_ = type(vm)
            if (_19_ == "function") then
              return vm(vt)
            else
              local _ = _19_
              return (vm == vt)
            end
          else
            return nil
          end
        end
        pcall(function() require("fennel").metadata:setall(_17_, "fnl/arglist", {"..."}) end)
        ok = (ok and _17_())
      end
      return ok
    end
    pcall(function() require("fennel").metadata:setall(_16_, "fnl/arglist", {"t"}) end)
    return _16_
  else
    return nil
  end
end
pcall(function() require("fennel").metadata:setall(tbl.matcher, "fnl/arglist", {"m"}, "fnl/docstring", "Return a matcher function for the table `m`.") end)
tbl.getter = function(at)
  local _23_ = type(at)
  if (_23_ == "string") then
    local function _24_(this)
      local t_25_ = this
      if (nil ~= t_25_) then
        t_25_ = t_25_[at]
      else
      end
      return t_25_
    end
    pcall(function() require("fennel").metadata:setall(_24_, "fnl/arglist", {"this"}) end)
    return _24_
  elseif (_23_ == "table") then
    local function _27_(this)
      local this0 = this
      for _, k in ipairs(at) do
        local t_28_ = this0
        if (nil ~= t_28_) then
          t_28_ = t_28_[k]
        else
        end
        this0 = t_28_
      end
      return this0
    end
    pcall(function() require("fennel").metadata:setall(_27_, "fnl/arglist", {"this"}) end)
    return _27_
  else
    local _ = _23_
    local function _30_(_0)
      return nil
    end
    pcall(function() require("fennel").metadata:setall(_30_, "fnl/arglist", {"_"}) end)
    return _30_
  end
end
pcall(function() require("fennel").metadata:setall(tbl.getter, "fnl/arglist", {"at"}, "fnl/docstring", "Return a getter function for accessing elements at position `at`.") end)
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
    local _32_ = type(u)
    if (_32_ == "function") then
      t[k] = u(t[k])
    else
      local _ = _32_
      t[k] = u
    end
  end
  return t
end
pcall(function() require("fennel").metadata:setall(tbl["upd-at"], "fnl/arglist", {"t", "k", "u"}, "fnl/docstring", "Update the key `k` in table `t` with the value `u`.") end)
tbl.upd = function(t, u)
  do
    local _34_ = type(u)
    if (_34_ == "table") then
      for k, f in pairs(u) do
        tbl["upd-at"](t, k, f)
      end
    elseif (_34_ == "function") then
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
seq.take = function(s, n)
  local ret = {}
  do
    local max = #s
    local _41_
    if (n > max) then
      _41_ = max
    else
      _41_ = n
    end
    for i = 1, _41_ do
      table.insert(ret, s[i])
    end
  end
  return ret
end
pcall(function() require("fennel").metadata:setall(seq.take, "fnl/arglist", {"s", "n"}) end)
seq["take-nth"] = function(s, n)
  local ret = {}
  for i, v in ipairs(s) do
    if (0 == math.fmod((i - 1), n)) then
      table.insert(ret, s[i])
    else
    end
  end
  return ret
end
pcall(function() require("fennel").metadata:setall(seq["take-nth"], "fnl/arglist", {"s", "n"}) end)
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
  local function _45_(x)
    if f(x) then
      return x
    else
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(_45_, "fnl/arglist", {"x"}) end)
  return seq.keep(s, _45_)
end
pcall(function() require("fennel").metadata:setall(seq.filter, "fnl/arglist", {"s", "f"}, "fnl/docstring", "Filter elements of sequence `s` using the function `f`.") end)
seq.remove = function(s, f)
  local function _47_(x)
    if not f(x) then
      return x
    else
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(_47_, "fnl/arglist", {"x"}) end)
  return seq.keep(s, _47_)
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
    local function _50_(a, b)
      return compare_fn(key_fn(a), key_fn(b))
    end
    pcall(function() require("fennel").metadata:setall(_50_, "fnl/arglist", {"a", "b"}) end)
    table.sort(s, _50_)
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
  local function _52_(a, b)
    return (key_fn(a) < key_fn(b))
  end
  pcall(function() require("fennel").metadata:setall(_52_, "fnl/arglist", {"a", "b"}) end)
  seq["sort-with"](s, _52_)
  return s
end
pcall(function() require("fennel").metadata:setall(seq["sort-by"], "fnl/arglist", {"s", "key-fn"}, "fnl/docstring", "Sort sequence `s` by the key function `key-fn`.") end)
seq["reverse-sort-by"] = function(s, key_fn)
  local function _53_(a, b)
    return (key_fn(a) > key_fn(b))
  end
  pcall(function() require("fennel").metadata:setall(_53_, "fnl/arglist", {"a", "b"}) end)
  seq["sort-with"](s, _53_)
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
tbl.walk = function(x, inner, outer)
  local _55_ = type(x)
  if (_55_ == "table") then
    local function _56_()
      local tbl_14_auto = {}
      for k, v in pairs(x) do
        local k_15_auto, v_16_auto = k, inner(v)
        if ((k_15_auto ~= nil) and (v_16_auto ~= nil)) then
          tbl_14_auto[k_15_auto] = v_16_auto
        else
        end
      end
      return tbl_14_auto
    end
    return outer(_56_())
  else
    local _ = _55_
    return outer(x)
  end
end
pcall(function() require("fennel").metadata:setall(tbl.walk, "fnl/arglist", {"x", "inner", "outer"}) end)
tbl.postwalk = function(x, f)
  local function _59_(y)
    return tbl.postwalk(y, f)
  end
  pcall(function() require("fennel").metadata:setall(_59_, "fnl/arglist", {"y"}) end)
  return tbl.walk(x, _59_, f)
end
pcall(function() require("fennel").metadata:setall(tbl.postwalk, "fnl/arglist", {"x", "f"}) end)
tbl.prewalk = function(x, f)
  local function _60_(y)
    return tbl.prewalk(y, f)
  end
  pcall(function() require("fennel").metadata:setall(_60_, "fnl/arglist", {"y"}) end)
  local function _61_(y)
    return y
  end
  pcall(function() require("fennel").metadata:setall(_61_, "fnl/arglist", {"y"}) end)
  return tbl.walk(f(x), _60_, _61_)
end
pcall(function() require("fennel").metadata:setall(tbl.prewalk, "fnl/arglist", {"x", "f"}) end)
tbl["indexed-walk"] = function(x, at, inner, outer)
  if ("table" == type(x)) then
    local function _62_()
      local tbl_14_auto = {}
      for k, v in pairs(x) do
        local k_15_auto, v_16_auto = k, inner(seq.append(clone(at), k), v)
        if ((k_15_auto ~= nil) and (v_16_auto ~= nil)) then
          tbl_14_auto[k_15_auto] = v_16_auto
        else
        end
      end
      return tbl_14_auto
    end
    return outer(at, _62_())
  else
    return outer(at, x)
  end
end
pcall(function() require("fennel").metadata:setall(tbl["indexed-walk"], "fnl/arglist", {"x", "at", "inner", "outer"}) end)
tbl["indexed-postwalk"] = function(x, f)
  local function postwalk(x0, at, f0)
    local function _65_(at0, y)
      return postwalk(y, at0, f0)
    end
    pcall(function() require("fennel").metadata:setall(_65_, "fnl/arglist", {"at", "y"}) end)
    return tbl["indexed-walk"](x0, at, _65_, f0)
  end
  pcall(function() require("fennel").metadata:setall(postwalk, "fnl/arglist", {"x", "at", "f"}) end)
  return postwalk(x, {}, f)
end
pcall(function() require("fennel").metadata:setall(tbl["indexed-postwalk"], "fnl/arglist", {"x", "f"}) end)
tbl["indexed-prewalk"] = function(x, f)
  local function prewalk(x0, at, f0)
    local function _66_(at0, y)
      return prewalk(y, at0, f0)
    end
    pcall(function() require("fennel").metadata:setall(_66_, "fnl/arglist", {"at", "y"}) end)
    local function _67_(_, y)
      return y
    end
    pcall(function() require("fennel").metadata:setall(_67_, "fnl/arglist", {"_", "y"}) end)
    return tbl["indexed-walk"](f0(at, x0), at, _66_, _67_)
  end
  pcall(function() require("fennel").metadata:setall(prewalk, "fnl/arglist", {"x", "at", "f"}) end)
  return prewalk(x, {}, f)
end
pcall(function() require("fennel").metadata:setall(tbl["indexed-prewalk"], "fnl/arglist", {"x", "f"}) end)
local function _68_(_241)
  return not _241
end
hof["not"] = _68_
hof.k = function(x)
  local function _69_(_)
    return x
  end
  pcall(function() require("fennel").metadata:setall(_69_, "fnl/arglist", {"_"}) end)
  return _69_
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
  local function _70_(y)
    return (x + y)
  end
  pcall(function() require("fennel").metadata:setall(_70_, "fnl/arglist", {"y"}) end)
  return _70_
end
pcall(function() require("fennel").metadata:setall(hof.adder, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that adds `x` to its argument.") end)
hof.gt = function(x)
  local function _71_(y)
    return (y > x)
  end
  pcall(function() require("fennel").metadata:setall(_71_, "fnl/arglist", {"y"}) end)
  return _71_
end
pcall(function() require("fennel").metadata:setall(hof.gt, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that returns true if its argument is greater than `x`.") end)
hof.lt = function(x)
  local function _72_(y)
    return (y < x)
  end
  pcall(function() require("fennel").metadata:setall(_72_, "fnl/arglist", {"y"}) end)
  return _72_
end
pcall(function() require("fennel").metadata:setall(hof.lt, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that returns true if its argument is less than `x`.") end)
hof.gte = function(x)
  local function _73_(y)
    return (y >= x)
  end
  pcall(function() require("fennel").metadata:setall(_73_, "fnl/arglist", {"y"}) end)
  return _73_
end
pcall(function() require("fennel").metadata:setall(hof.gte, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that returns true if its argument is greater than or equal to `x`.") end)
hof.lte = function(x)
  local function _74_(y)
    return (y <= x)
  end
  pcall(function() require("fennel").metadata:setall(_74_, "fnl/arglist", {"y"}) end)
  return _74_
end
pcall(function() require("fennel").metadata:setall(hof.lte, "fnl/arglist", {"x"}, "fnl/docstring", "Return a function that returns true if its argument is less than or equal to `x`.") end)
return {path = path, file = file, tbl = tbl, hof = hof, reload = reload, seq = seq, clone = clone}
