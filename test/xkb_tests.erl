-module(xkb_tests).
-include_lib("xkb/include/xkb_keydefs.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LatQ,24).

-define(EVDEV_OFFSET,8).


comprehensive_plus_geom_test_() ->
  {with, keymap("comprehensive-plus-geom"),
   [ fun (Keymap) ->
         ?assertMatch([ {pressed, {char, $q}}
                      , {released,{char, $Q}}
                      , {pressed, {char, $Q}}
                      , {released,{char, $Q}}
                      , {pressed, {char, $q}}]
                      , play_keys([ {press,  ?LatQ}
                                  , shift_on
                                  , {release,?LatQ}
                                  , {press,  ?LatQ}
                                  , {release,?LatQ}
                                  , shift_off
                                  , {press,  ?LatQ}
                                  ], Keymap))
     end
]}.


play_keys(Keys, Keymap) ->
    {Vs, _} = lists:foldl(fun play_key/2, {[], xkb:init(xkb_v1,Keymap)}, Keys),
    Vs.

play_key(shift_on, {Vs, S}) ->
    {Vs, xkb:update_modifiers(1,0,0,0,S)};

play_key(shift_off, {Vs, S}) ->
    {Vs, xkb:update_modifiers(0,0,0,0,S)};

play_key({press, K}, {Vs0, S0}) ->
    {Vs1, S1} = xkb:key_pressed(K - ?EVDEV_OFFSET, S0),
    {Vs0 ++ [{pressed, V} || V <- Vs1], S1};

play_key({release, K}, {Vs0, S0}) ->
    {Vs1, S1} = xkb:key_released(K - ?EVDEV_OFFSET, S0),
    {Vs0 ++ [{released, V} || V <- Vs1], S1}.


keymap(KeymapName) ->
    {_, _, ModuleFile} = code:get_object_code(?MODULE),
    Base = filename:dirname(ModuleFile),
    FileName = filename:join([Base, "data", KeymapName ++ ".xkb"]),
    case file:read_file(FileName) of
        {ok, Data}     -> Data;
        {error, Error} -> error(Error)
    end.
