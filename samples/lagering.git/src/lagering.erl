-module(lagering).
-compile({parse_transform, lager_transform}).

%% lagering: lagering library's entry point.

-export([my_func/0]).


%% API

my_func() ->
    lager:info("~s is ~s!", [lager, cool]),
    lager:warning("but pay ~s!", [attention]),
    lager:error("there is always some ~s", [error]),
    ok().

%% Internals

ok() ->
    ok.

%% End of Module.
