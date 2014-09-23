-module(a_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test1/1]).

all() ->
     [test1].

test1(_Config) ->
1 = 1.
