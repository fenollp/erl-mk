-module(b_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test2/1]).

all() ->
     [test2].

test2(_Config) ->
    A = 0,
    1/A.
