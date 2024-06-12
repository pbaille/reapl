-- @noindex
local utils = require("fennel.utils")
local parser = require("fennel.parser")
local compiler = require("fennel.compiler")
local specials = require("fennel.specials")
local view = require("fennel.view")
local function complete(_1_, text)
  local _arg_2_ = _1_
  local opts = _arg_2_
  local env = _arg_2_["env"]
  local scope = _arg_2_["scope"]
  local max_items = 2000
  local seen = {}
  local matches = {}
  local input_fragment = text:gsub(".*[%s)(]+", "")
  local stop_looking_3f = false
  local function add_partials(input, tbl, prefix)
    local scope_first_3f = ((tbl == env) or (tbl == env.___replLocals___))
    local tbl_17_auto = matches
    local function _3_()
      if scope_first_3f then
        return scope.manglings
      else
        return tbl
      end
    end
    for k, is_mangled in utils.allpairs(_3_()) do
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
pcall(function() require("fennel").metadata:setall(complete, "fnl/arglist", {"{:env env :scope scope &as opts}", "text"}) end)
local function splice_save_locals(_11_, lua_source)
  local _arg_12_ = _11_
  local env = _arg_12_["env"]
  local scope = _arg_12_["scope"]
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
  local function _17_()
    if next(saves) then
      return (table.concat(saves, " ") .. gap)
    else
      return ""
    end
  end
  local function _20_()
    local _18_, _19_ = lua_source:match("^(.*)[\n ](return .*)$")
    if ((nil ~= _18_) and (nil ~= _19_)) then
      local body = _18_
      local _return = _19_
      return (body .. gap .. table.concat(binds, " ") .. gap .. _return)
    else
      local _ = _18_
      return lua_source
    end
  end
  return (_17_() .. _20_())
end
pcall(function() require("fennel").metadata:setall(splice_save_locals, "fnl/arglist", {"{:env env :scope scope}", "lua-source"}) end)
local function resolve(_22_, identifier)
  local _arg_23_ = _22_
  local env = _arg_23_["env"]
  local scope = _arg_23_["scope"]
  local e
  local function _24_(_241, _242)
    return (env.___replLocals___[scope.unmanglings[_242]] or env[_242])
  end
  e = setmetatable({}, {__index = _24_})
  local function _25_(...)
    local _26_, _27_ = ...
    if ((_26_ == true) and (nil ~= _27_)) then
      local code = _27_
      local function _28_(...)
        local _29_, _30_ = ...
        if ((_29_ == true) and (nil ~= _30_)) then
          local val = _30_
          return val
        else
          local _ = _29_
          return nil
        end
      end
      pcall(function() require("fennel").metadata:setall(_28_, "fnl/arglist", {"..."}) end)
      return _28_(pcall(specials["load-code"](code, e)))
    else
      local _ = _26_
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(_25_, "fnl/arglist", {"..."}) end)
  return _25_(pcall(compiler["compile-string"], tostring(identifier), {scope = scope}))
end
pcall(function() require("fennel").metadata:setall(resolve, "fnl/arglist", {"{:env env :scope scope}", "identifier"}) end)
local function doc(_33_, name)
  local _arg_34_ = _33_
  local opts = _arg_34_
  local env = _arg_34_["env"]
  local scope = _arg_34_["scope"]
  local path = (utils["multi-sym?"](name) or {name})
  local ok_3f, target = nil, nil
  local function _35_()
    return (utils["get-in"](scope.specials, path) or utils["get-in"](scope.macros, path) or resolve(opts, name))
  end
  ok_3f, target = pcall(_35_)
  if ok_3f then
    return {value = specials.doc(target, name)}
  else
    return {error = {type = "repl", message = ("Could not find " .. name .. " for docs.")}}
  end
end
pcall(function() require("fennel").metadata:setall(doc, "fnl/arglist", {"{:env env :scope scope &as opts}", "name"}) end)
local function info(_37_, name)
  local _arg_38_ = _37_
  local opts = _arg_38_
  local env = _arg_38_["env"]
  local scope = _arg_38_["scope"]
  local _39_ = name
  if (nil ~= _39_) then
    local _40_ = resolve(_39_, opts)
    if (nil ~= _40_) then
      return debug.getinfo(_40_)
    else
      return _40_
    end
  else
    return _39_
  end
end
pcall(function() require("fennel").metadata:setall(info, "fnl/arglist", {"{:env env :scope scope &as opts}", "name"}) end)
local function find(_43_, name)
  local _arg_44_ = _43_
  local opts = _arg_44_
  local env = _arg_44_["env"]
  local scope = _arg_44_["scope"]
  local _45_ = info(opts, name)
  if ((_G.type(_45_) == "table") and (_45_.what == "Lua") and (nil ~= _45_.source) and (nil ~= _45_.linedefined) and (nil ~= _45_.short_src)) then
    local source = _45_.source
    local line = _45_.linedefined
    local src = _45_.short_src
    local fnlsrc
    do
      local t_46_ = compiler.sourcemap
      if (nil ~= t_46_) then
        t_46_ = t_46_[source]
      else
      end
      if (nil ~= t_46_) then
        t_46_ = t_46_[line]
      else
      end
      if (nil ~= t_46_) then
        t_46_ = t_46_[2]
      else
      end
      fnlsrc = t_46_
    end
    return {value = string.format("%s:%s", src, (fnlsrc or line))}
  elseif (_45_ == nil) then
    return {error = {type = "repl", message = "Unknown value"}}
  else
    local _ = _45_
    return {error = {type = "repl", message = "No source info"}}
  end
end
pcall(function() require("fennel").metadata:setall(find, "fnl/arglist", {"{:env env :scope scope &as opts}", "name"}) end)
--[[ (find {:env (specials.wrap-env _G) :scope (compiler.make-scope)} "pairs") (global foo 43) (debug.getinfo (resolve {:env (specials.wrap-env _G) :scope (compiler.make-scope)} "foo")) (utils.sym? "iop") ]]
local function repl()
  local scope = compiler["make-scope"]()
  local env = specials["wrap-env"](_G)
  local useMetadata = true
  local opts = {env = env, scope = scope, useMetadata = useMetadata}
  local function _51_(t, k, v)
    if opts.scope.manglings[k] then
      return rawset(t, k, v)
    else
      return nil
    end
  end
  pcall(function() require("fennel").metadata:setall(_51_, "fnl/arglist", {"t", "k", "v"}) end)
  opts.env.___replLocals___ = setmetatable({}, {__newindex = _51_})
  local function _53_(...)
    return complete(opts, ...)
  end
  pcall(function() require("fennel").metadata:setall(_53_, "fnl/arglist", {"..."}) end)
  local function _54_(...)
    return doc(opts, ...)
  end
  pcall(function() require("fennel").metadata:setall(_54_, "fnl/arglist", {"..."}) end)
  local function _55_(code_str)
    local ok, parser_not_eof_3f, form = pcall(parser.parser(code_str))
    if not ok then
      return {error = {type = "parse", data = {parser_not_eof_3f, form}}}
    else
      local function _56_(...)
        local _57_, _58_ = ...
        if ((_57_ == true) and (nil ~= _58_)) then
          local src = _58_
          local src0 = splice_save_locals(opts, src)
          local ok_3f, f = pcall(specials["load-code"], src0, opts.env)
          if ok_3f then
            local value = f()
            return {value = value, ["value-str"] = view(value)}
          else
            return {error = {type = "load", message = f}}
          end
        elseif ((_57_ == false) and (nil ~= _58_)) then
          local msg = _58_
          return {error = {type = "compile", message = msg}}
        else
          return nil
        end
      end
      pcall(function() require("fennel").metadata:setall(_56_, "fnl/arglist", {"..."}) end)
      local function _61_()
        opts["source"] = code_str
        return opts
      end
      return _56_(pcall(compiler.compile, form, _61_()))
    end
  end
  pcall(function() require("fennel").metadata:setall(_55_, "fnl/arglist", {"code-str"}) end)
  return {complete = _53_, doc = _54_, eval = _55_}
end
pcall(function() require("fennel").metadata:setall(repl, "fnl/arglist", {}) end)
--[[ (let [{:complete complete :eval eval} (repl)] ((. (eval "(do (local x 3) (+ x x))") "return"))) (let [{:complete complete :eval eval} (repl)] ((. (eval "(global x 3)") "return")) ((. (eval "(+ x x)") "return"))) (?. {:foo {:bar 2}} "foo" "bar") ]]
return repl()
