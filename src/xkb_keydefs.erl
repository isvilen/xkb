-module(xkb_keydefs).
-export([parse_transform/2]).

-include_lib("syntax_tools/include/merl.hrl").


parse_transform(Forms, _Opts) ->
    Forms ++ erl_syntax:revert_forms([xkb1_keydefs_fun()]).


xkb1_keydefs_fun() ->
    Data = #{},
    ?Q(["xkb1_keydefs(KeySym) ->"
       ,"    maps:get(KeySym, _@Data@, undefined)."
       ]).
