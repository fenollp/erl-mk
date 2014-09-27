%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(erldocs_other_core).

%% erldocs_other_core: main logic of the erldocs_other module.

-export([ main/1
        , to_file/2
        ]).

-define(LOG(Str, Args), io:format(" :: "++ Str, Args)).
-define(LOG(Str),       io:format(" :: "++ Str)).

-define(u, erldocs_other_utils).

%% API

main (Conf) ->
    TimeBegin = utc(),
    URL0         = kf(Conf, url),
    Destination0 = kf(Conf, dest),
    {true,Url} = url(URL0),
    Method     = method(Url),
    RepoName = repo_name(Url),

    Dest     = filename:absname(Destination0),
    mkdir(Dest),
    TmpDir   = filename:join(Dest, RepoName),
    mkdir(TmpDir),
    DocsRoot = filename:join(Dest, "repo"),
    mkdir(DocsRoot),
    MetaFile = filename:join(Dest, "meta.terms"),

    ?LOG("Cloning repo ~p into ~p\n", [Url,TmpDir]),
    ok = clone_repo(Method, Url, TmpDir),

    ?LOG("Extracting meta information\n"),
    Meta = extract_info(Method, Url, TimeBegin, TmpDir),
    ?LOG("Writing meta to ~p\n", [MetaFile]),
    to_file(MetaFile, Meta),

    ?LOG("Preparing repo for docs generation\n"),
    Tags     = kf(Meta, tags),
    Branches = kf(Meta, branches),
    TBs = Tags ++ Branches,
    main(TBs, Method, RepoName, TmpDir,
         Conf, Meta, MetaFile, DocsRoot, Dest, []).

main ([{Commit,Title}|TBs], Method, RepoName, TmpDir,
      Conf, Meta, MetaFile, DocsRoot, Dest, Acc) ->
    ?LOG("Processing\trepo:~s\ttitle:~s\tcommit:~s\n", [RepoName,Title,Commit]),
    TitledPath = copy_repo(Method, {Commit,Title}, RepoName, Dest),

    ?LOG("Getting dependencies\n"),
    get_deps(TitledPath),
    %%FIXME `make` cloned repo (using shell's redirection & sandbox)

    %%FIXME think about rmrf TitlePath/.git/, deps/*/.git/ & submodules'.
    erldocs(Conf, DocsRoot, Title, TitledPath),

    ?LOG("Discovering other repos\n"),
    %%del_deps(TitledPath),
    Treasure = repo_discovery(Title, TitledPath),
    case Treasure of
        {_, []} -> Treasures = Acc;
        _ ->       Treasures = [Treasure|Acc]
    end,

    ?u:rmrf(filename:dirname(TitledPath)),  %% rm titled repo
    main(TBs, Method, RepoName, TmpDir,
         Conf, Meta, MetaFile, DocsRoot, Dest, Treasures);

main ([], _, _, TmpDir,
      Conf, Meta, MetaFile, DocsRoot, _, Treasures) ->
    ?LOG("Erldocs finishing up.\n"),
    MetaRest = [{discovered,Treasures}, {time_end,utc()}],
    to_file(MetaFile, MetaRest, [append]),
    ?u:rmrf(TmpDir),
    put_repo_index(Conf, DocsRoot, Meta).

%% Internals

html_index (DocsRoot, Meta) ->
    Tags     = kf(Meta, tags),
    Branches = kf(Meta, branches),
    "<h3 id=\"tags\">Tags</h3>"
        ++ "\n\t<p>" ++ list_titles(DocsRoot,Tags) ++ "</p>"
        ++ "<br/>"
        ++ "\n\t<h3 id=\"branches\">Branches</h3>"
        ++ "\n\t<p>" ++ list_titles(DocsRoot,Branches) ++ "</p>".

list_titles (DocsRoot, Titles) ->
    Items = [ begin
                  case path_exists([DocsRoot, Branch, "index.html"]) of
                      true  ->
                          "<a href=\""++Branch++"\">"++Branch++"</a>";
                      false ->
                          ?u:rmrf(filename:join(DocsRoot, Branch)),
                          Branch
                  end
              end || {_,Branch} <- Titles ],
    case Items of
        [] -> "(none)";
        _  ->
            Spaces = lists:duplicate(3, "&nbsp;"),
            string:join(Items, Spaces)
    end.

put_repo_index (Conf, DocsRoot, Meta) ->
    Args = [ {title,   kf(Meta, target_path)}
           , {url,     kf(Meta, url)}
           , {content, html_index(DocsRoot, Meta)}
           , {base,    kf(Conf, base)}
           , {ga,      kf(Conf, ga)} ],
    {ok, HTML} = html_dtl:render(Args),
    ok = file:write_file(filename:join(DocsRoot,"index.html"), HTML),
    {ok, CSS}  = css_dtl:render([]),
    ok = file:write_file(filename:join(DocsRoot,"repo.css"), CSS).

repo_discovery (Title, RepoPath) ->
    FilesFound = filelib:wildcard("rebar.config*", RepoPath)
        ++ [ File || File <- [ "Makefile"
                             , ".gitmodules" ],
                     path_exists([RepoPath,File]) ],
    UrlsFound = search_files(RepoPath, FilesFound),
    {Title, lists:usort(lists:filtermap(fun url/1, UrlsFound))}.

search_files (RepoPath, Files) ->
    lists:flatmap(
      fun (File) ->
              FilePath = filename:join(RepoPath, File),
              {ok, Contents} = file:read_file(FilePath),
              case File of
                  "Makefile" ->
                      discover_urls("\\s\"'();,", Contents);
                  ".gitmodules" ->
                      discover_urls("\\s=",       Contents)
                   ++ discover_urls("\\s\"", "@", Contents);
                  "rebar.config"++_ ->
                      discover_urls("\\s\"",      Contents)
                   ++ discover_urls("\\s\"", "@", Contents)
              end
      end, Files).

discover_urls (Seps, Bin) ->
    discover_urls (Seps, "://", Bin).
discover_urls (Seps, Mid, Bin) ->
    RegExp = [ "[",Seps,"]([^", Seps, "]+", Mid, "[^", Seps, "]+)[",Seps,"]" ],
    case re:run(Bin, lists:flatten(RegExp),
                [{capture,all_but_first,list}, global]) of
        {match, Urls} -> lists:append(Urls);
        nomatch -> []
    end.

erldocs (Conf, DocsRoot, Branch, Path) ->
    DocsDest = filename:join(DocsRoot, Branch),
    ?LOG("Generating erldocs into ~s\n", [DocsDest]),
    mkdir(DocsDest),
    Args = [ Path
           , "-o",     DocsDest
           , "--base", kf(Conf,base)
           , "--ga",   kf(Conf,ga)
           ]
        ++ list_abs(Path, "apps/*")
        ++ list_abs(Path, "applications/*")
        ++ lists:flatmap(fun (Dir) -> ["-I", Dir] end,
                         find_dirs("\\.hrl$", Path)),
    %% FIXME add non-deps containing src/
    %% ++ [ filename:dirname(Dir) || Dir <- find_dirs(".+", Path)
    %%                                   lists:suffix("/src", Dir) ],
    erldocs:main(Args).

find_dirs (FilePattern, Path) ->
    AccDirs = fun (File, Acc) ->
                      [filename:dirname(File)|Acc]
              end,
    Dirs = filelib:fold_files(Path, FilePattern, true, AccDirs, []),
    lists:usort(Dirs).

list_abs (Path, Wildcard) ->
    Pattern = filename:join(Path, Wildcard),
    filelib:wildcard(Pattern).

kf (Conf, Key) ->
    {Key, Value} = lists:keyfind(Key, 1, Conf),
    Value.

copy_repo (git, {Commit,Title}, RepoName, DestDir) ->
    Name = make_name(RepoName, Commit, Title),
    TitledPath = filename:join([DestDir, Name, RepoName]),
    mkdir(filename:join(DestDir, Name)),
    %% cd DestDir && cp -pr RepoName TitledPath
    ?u:cp(DestDir, RepoName, TitledPath),
    ?u:git_changeto(TitledPath, Commit),
    ?u:rmr_symlinks(TitledPath),
    TitledPath.

get_deps (Path) ->
    case path_exists([Path, "rebar.config"]) of
        true  -> ?u:rebar_get_deps(Path);
        false -> ok
    end,
    case path_exists([Path, ".gitmodules"]) of
        true  -> ?u:git_get_submodules(Path);
        false -> ok
    end.

del_deps (Path) ->
    case path_exists([Path, "rebar.config"]) of
        true  -> ?u:rebar_delete_deps(Path);
        false -> ok
    end,
    case path_exists([Path, ".gitmodules"]) of
        true  -> ?u:delete_submodules(Path);
        false -> ok
    end,
    ?u:rmrf(filename:join(Path, "deps")).

path_exists (PathToJoin) ->
    Path = filename:join(PathToJoin),
    filelib:is_file(Path).

mkdir (Dir) ->
    ok = filelib:ensure_dir(Dir ++ "/").

make_name (RepoName, Commit, Branch) ->
    string:join([RepoName, Commit, Branch], "-").

repo_name (Url) ->
    lists:last(string:tokens(Url, "/")).

repo_local_path (Url) ->
    Exploded = string:tokens(Url, "/"),
    filename:join(tl(Exploded)).

extract_info (git, Url, TimeBegin, TmpDir) ->
    [ {name, repo_name(Url)}
    , {target_path, repo_local_path(Url)}
    , {url, Url}
    , {size_of_repo, ?u:du(TmpDir)}
    , {time_begin, TimeBegin}
    , {method, git}
    , {branches, ?u:git_branches(TmpDir)}
    , {tags, ?u:git_tags(TmpDir)} ];
extract_info (Other, _, _, _) ->
    throw({badmethod, Other}).

utc () ->
    calendar:universal_time().

clone_repo (git, Url, TmpDir) ->
    ?u:git_clone(Url, TmpDir);
clone_repo (Other, Url, _) ->
    throw({badmethod, Other, url, Url}).


method ("https://github.com/"++_) -> git;
method ("https://bitbucket.org/"++_) -> git.

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


to_file (Path, Data) ->
    to_file (Path, Data, []).
to_file (Path, Data, Options) ->
    Str = [io_lib:fwrite("~p.\n",[Datum]) || Datum <- Data],
    file:write_file(Path, Str, Options).

%% End of Module.
