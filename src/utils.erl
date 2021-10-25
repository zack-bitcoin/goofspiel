-module(utils).
-export([off/0]).

off() ->
    goofspiel_sup:stop(),
    ok = application:stop(goofspiel).
