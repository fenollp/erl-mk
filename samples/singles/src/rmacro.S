{module, rmacro}.  %% version = 0

{exports, [{a,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 7}.


{function, a, 0, 2}.
  {label,1}.
    {line,[{location,"src/rmacro.erl",21}]}.
    {func_info,{atom,rmacro},{atom,a},0}.
  {label,2}.
    {move,{literal,[[ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok]]},{x,1}}.
    {move,{literal,"~p\n"},{x,0}}.
    {line,[{location,"src/rmacro.erl",22}]}.
    {call_ext_only,2,{extfunc,io,format,2}}.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,rmacro},{atom,module_info},0}.
  {label,4}.
    {move,{atom,rmacro},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,rmacro},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,rmacro},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
