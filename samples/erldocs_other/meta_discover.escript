#!/usr/bin/env escript
%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-

%% discover: 

-mode(compile).

%% API

main ([Dir]) ->
    F = fun (File, _Acc) ->
                io:format(" -> ~p\n", [File]),
                {ok, Terms} = file:consult(File),
                case lists:keyfind(discover, 1, Terms) of

                    {_, TitleNFilesFound} ->
                        Discovered = [{Title,discover_urls(FilesFound)}
                                      || {Title,FilesFound} <- TitleNFilesFound],
                        print(Discovered),
                        NewTerms = proplists:delete(discover, Terms)
                            ++ [{discovered, Discovered}],
                        to_file(File, NewTerms, []);

                    false ->
                        case lists:keyfind(discovered, 1, Terms) of
                            {_, Discovered} ->
                                print(Discovered);
                            false -> ok
                        end
                end
        end,
    filelib:fold_files(Dir, "meta\\.txt", true, F, ignore);

main (_) ->
    usage().

%% Internals

print (L)
  when is_list(L) ->
    Urls = lists:usort(lists:append([Urls || {_,Urls} <- L])),
    lists:foreach(fun (X) -> io:format("~s\n", [X]) end, Urls).

discover_urls (NameNContentsPairs) ->
    F = fun ({File, Contents}) ->
                case lists:suffix("Makefile", File) of
                    true  -> discover_urls("\\s\"'", Contents);
                    false ->
                        case lists:suffix(".gitmodules", File) of
                            true  -> discover_urls("\\s=", Contents);
                            false -> discover_urls("\"", Contents)
                        end
                end
        end,
    UrlsFound = lists:flatmap(F, NameNContentsPairs),
    lists:filtermap(fun url/1, UrlsFound).

discover_urls (Seps, Bin) ->
    RegExp = [ "[", Seps, "]([^", Seps, "]+://[^", Seps, "]+)[", Seps, "]" ],
    case re:run(Bin, lists:flatten(RegExp),
                [{capture,all_but_first,list}, global]) of
        {match, Urls} -> lists:append(Urls);
        nomatch -> []
    end.

url (URL0) ->
    Url = string:to_lower(URL0),
    case re:run(Url, "(github.com|bitbucket.org)[:/]([^:/]+)/([^/]+)",
                [{capture,all_but_first,list}]) of
        {match, [Site,User,Name]} ->
            {true, "https://"++Site++"/"++User++"/"++trim_dotgit(Name)};
        nomatch -> false
    end.

trim_dotgit (Str) ->
    filename:basename(Str, ".git").


to_file (Path, Data, Options) ->
    Str = [io_lib:fwrite("~p.\n",[Datum]) || Datum <- Data],
    file:write_file(Path, Str, Options).

usage () ->
    ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  ‹other.erldocs.com's dir›\n",
              [filename:basename(Arg0)]),
    halt(1).

%% End of Module.
