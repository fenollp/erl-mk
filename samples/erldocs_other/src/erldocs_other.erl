%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(erldocs_other).

%% erldocs_other: escript rendering a repos' erldocs.

-export([ main/1 ]).

-record(conf, { dest = ""
              , url  = ""
              , base = "./"
              , ga = "UA-54292016-1" }).

%% API

main ([]) ->
    ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: \n\t~s  -o ‹output dir› ‹repo URL›\n", [Arg0]),
    halt(1);
main (Args) ->
    parse(Args, #conf{}).

%% Internals

parse ([], Conf) ->
    Dest = Conf#conf.dest,
    URL  = Conf#conf.url,
    case (Dest == "") or (URL == "") of
        true  -> main([]);
        false -> run([ {dest, Dest}
                     , {url,  URL}
                     , {base, Conf#conf.base}
                     , {ga,   Conf#conf.ga} ])
    end;

parse (["-o",     Dest | Rest], Conf) ->
    parse(Rest, Conf#conf{dest = Dest});
parse (["--base", Base | Rest], Conf) ->
    parse(Rest, Conf#conf{base = Base});
parse (["--ga",     GA | Rest], Conf) ->
    parse(Rest, Conf#conf{ga   = GA  });
parse ([URL            | Rest], Conf) ->
    parse(Rest, Conf#conf{url  = URL }).


run (Args) ->
    try erldocs_other_core:main(Args)
    catch Type:Error ->
            io:format("Error running ~p:\n\t~p\n~p\n",
                      [?MODULE, erlang:get_stacktrace(), {Type,Error}]),
            halt(2)
    end.

%% End of Module.
