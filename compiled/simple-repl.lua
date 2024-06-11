-- @noindex
local utils = require("fennel.utils")
local parser = require("fennel.parser")
local compiler = require("fennel.compiler")
local specials = require("fennel.specials")
local view = require("fennel.view")
local function completer(env, scope, text)
  local max_items = 2000
  local seen = {}
  local matches = {}
  local input_fragment = text:gsub(".*[%s)(]+", "")
  local stop_looking_3f = false
  local function add_partials(input, tbl, prefix)
    local scope_first_3f = ((tbl == env) or (tbl == env.___replLocals___))
    local tbl_17_auto = matches
    local function _1_()
      if scope_first_3f then
        return scope.manglings
      else
        return tbl
      end
    end
    for k, is_mangled in utils.allpairs(_1_()) do
      if (max_items <= #matches) then break end
      local val_18_auto
      do
        local lookup_k
        if scope_first_3f then
          lookup_k = is_mangled
        else
          lookup_k = k
        end
        if ((type(k) == "string") and (input == k:sub(0, #input)) and not seen[k] and ((":" ~= prefix:sub(-1)) or ("function" == type(tbl[lookup_k])))) then
          seen[k] = true
          val_18_auto = (prefix .. k)
        else
          val_18_auto = nil
        end
      end
      table.insert(tbl_17_auto, val_18_auto)
    end
    return tbl_17_auto
  end
  pcall(function() require("fennel").metadata:setall(add_partials, "fnl/arglist", {"input", "tbl", "prefix"}) end)
  local function descend(input, tbl, prefix, add_matches, method_3f)
    local splitter
    if method_3f then
      splitter = "^([^:]+):(.*)"
    else
      splitter = "^([^.]+)%.(.*)"
    end
    local head, tail = input:match(splitter)
    local raw_head = (scope.manglings[head] or head)
    if (type(tbl[raw_head]) == "table") then
      stop_looking_3f = true
      if method_3f then
        return add_partials(tail, tbl[raw_head], (prefix .. head .. ":"))
      else
        return add_matches(tail, tbl[raw_head], (prefix .. head))
      end
    else
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(descend, "fnl/arglist", {"input", "tbl", "prefix", "add-matches", "method?"}) end)
  local function add_matches(input, tbl, prefix)
    local prefix0
    if prefix then
      prefix0 = (prefix .. ".")
    else
      prefix0 = ""
    end
    if (not input:find("%.") and input:find(":")) then
      return descend(input, tbl, prefix0, add_matches, true)
    elseif not input:find("%.") then
      return add_partials(input, tbl, prefix0)
    else
      return descend(input, tbl, prefix0, add_matches, false)
    end
  end
  pcall(function() require("fennel").metadata:setall(add_matches, "fnl/arglist", {"input", "tbl", "prefix"}) end)
  for _, source in ipairs({scope.specials, scope.macros, (env.___replLocals___ or {}), env, env._G}) do
    if stop_looking_3f then break end
    add_matches(input_fragment, source)
  end
  return matches
end
pcall(function() require("fennel").metadata:setall(completer, "fnl/arglist", {"env", "scope", "text"}) end)
local function splice_save_locals(env, lua_source, scope)
  local saves
  do
    local tbl_19_auto = {}
    local i_20_auto = 0
    for name in pairs(env.___replLocals___) do
      local val_21_auto = ("local %s = ___replLocals___[%q]"):format((scope.manglings[name] or name), name)
      if (nil ~= val_21_auto) then
        i_20_auto = (i_20_auto + 1)
        do end (tbl_19_auto)[i_20_auto] = val_21_auto
      else
      end
    end
    saves = tbl_19_auto
  end
  local binds
  do
    local tbl_19_auto = {}
    local i_20_auto = 0
    for raw, name in pairs(scope.manglings) do
      local val_21_auto
      if not scope.gensyms[name] then
        val_21_auto = ("___replLocals___[%q] = %s"):format(raw, name)
      else
        val_21_auto = nil
      end
      if (nil ~= val_21_auto) then
        i_20_auto = (i_20_auto + 1)
        do end (tbl_19_auto)[i_20_auto] = val_21_auto
      else
      end
    end
    binds = tbl_19_auto
  end
  local gap
  if lua_source:find("\n") then
    gap = "\n"
  else
    gap = " "
  end
  local function _13_()
    if next(saves) then
      return (table.concat(saves, " ") .. gap)
    else
      return ""
    end
  end
  local function _16_()
    local _14_, _15_ = lua_source:match("^(.*)[\n ](return .*)$")
    if ((nil ~= _14_) and (nil ~= _15_)) then
      local body = _14_
      local _return = _15_
      return (body .. gap .. table.concat(binds, " ") .. gap .. _return)
    else
      local _ = _14_
      return lua_source
    end
  end
  return (_13_() .. _16_())
end
pcall(function() require("fennel").metadata:setall(splice_save_locals, "fnl/arglist", {"env", "lua-source", "scope"}) end)
local function resolve(identifier, _18_, scope)
  local _arg_19_ = _18_
  local ___replLocals___ = _arg_19_["___replLocals___"]
  local env = _arg_19_
  local e
  local function _20_(_241, _242)
    return (___replLocals___[scope.unmanglings[_242]] or env[_242])
  end
  e = setmetatable({}, {__index = _20_})
  local function _21_(...)
    local _22_, _23_ = ...
    if ((_22_ == true) and (nil ~= _23_)) then
      local code = _23_
      local function _24_(...)
        local _25_, _26_ = ...
        if ((_25_ == true) and (nil ~= _26_)) then
          local val = _26_
          return val
        else
          local _ = _25_
          return nil
        end
      end
      pcall(function() require("fennel").metadata:setall(_24_, "fnl/arglist", {"..."}) end)
      return _24_(pcall(specials["load-code"](code, e)))
    else
      local _ = _22_
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(_21_, "fnl/arglist", {"..."}) end)
  return _21_(pcall(compiler["compile-string"], tostring(identifier), {scope = scope}))
end
pcall(function() require("fennel").metadata:setall(resolve, "fnl/arglist", {"identifier", "{:___replLocals___ ___replLocals___ &as env}", "scope"}) end)
local function doc(env, scope, name)
  local path = (utils["multi-sym?"](name) or {name})
  local ok_3f, target = nil, nil
  local function _29_()
    return (utils["get-in"](scope.specials, path) or utils["get-in"](scope.macros, path) or resolve(name, env, scope))
  end
  ok_3f, target = pcall(_29_)
  if ok_3f then
    return {value = specials.doc(target, name)}
  else
    return {error = {type = "Repl", message = ("Could not find " .. name .. " for docs.")}}
  end
end
pcall(function() require("fennel").metadata:setall(doc, "fnl/arglist", {"env", "scope", "name"}) end)
local function repl()
  local old_root_options = utils.root.options
  local _let_31_ = utils.copy({})
  local _3ffennelrc = _let_31_["fennelrc"]
  local opts = _let_31_
  local _
  opts.fennelrc = nil
  _ = nil
  local _0
  if _3ffennelrc then
    _0 = _3ffennelrc()
  else
    _0 = nil
  end
  local env = specials["wrap-env"](_G)
  opts.env, opts.scope = env, compiler["make-scope"]()
  local function newindex(t, k, v)
    if opts.scope.manglings[k] then
      return rawset(t, k, v)
    else
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(newindex, "fnl/arglist", {"t", "k", "v"}) end)
  env.___replLocals___ = setmetatable({}, {__newindex = newindex})
  opts.useMetadata = true
  local _35_
  do
    local _34_ = opts.scope
    local function _36_(...)
      return completer(env, _34_, ...)
    end
    pcall(function() require("fennel").metadata:setall(_36_, "fnl/arglist", {"..."}) end)
    _35_ = _36_
  end
  local _38_
  do
    local _37_ = opts.scope
    local function _39_(...)
      return doc(env, _37_, ...)
    end
    pcall(function() require("fennel").metadata:setall(_39_, "fnl/arglist", {"..."}) end)
    _38_ = _39_
  end
  local function _40_(code_str)
    local ok, parser_not_eof_3f, form = pcall(parser.parser(code_str))
    if not ok then
      return {error = {type = "parse", data = {parser_not_eof_3f, form}}}
    else
      local function _41_(...)
        local _42_, _43_ = ...
        if ((_42_ == true) and (nil ~= _43_)) then
          local src = _43_
          local src0 = splice_save_locals(env, src, opts.scope)
          local ok_3f, f = pcall(specials["load-code"], src0, env)
          if ok_3f then
            return {value = f()}
          else
            return {error = {type = "load", message = f}}
          end
        elseif ((_42_ == false) and (nil ~= _43_)) then
          local msg = _43_
          return {error = {type = "compile", message = msg}}
        else
          return nil
        end
      end
      pcall(function() require("fennel").metadata:setall(_41_, "fnl/arglist", {"..."}) end)
      local function _46_()
        opts["source"] = code_str
        return opts
      end
      return _41_(pcall(compiler.compile, form, _46_()))
    end
  end
  pcall(function() require("fennel").metadata:setall(_40_, "fnl/arglist", {"code-str"}) end)
  return {complete = _35_, doc = _38_, eval = _40_}
end
pcall(function() require("fennel").metadata:setall(repl, "fnl/arglist", {}) end)
--[[ (let [{:complete complete :eval eval} (repl)] ((. (eval "(do (local x 3) (+ x x))") "return"))) (let [{:complete complete :eval eval} (repl)] ((. (eval "(global x 3)") "return")) ((. (eval "(+ x x)") "return"))) ]]
return repl()
