-module(deeeps).

-export([ update_source1/2]).
-export([download_source/2]).

-define(FMT(Str,L), io_lib:format(Str,L)).

update_source1(AppDir, {git, Url}) ->
    update_source1(AppDir, {git, Url, {branch, "HEAD"}});
update_source1(AppDir, {git, Url, ""}) ->
    update_source1(AppDir, {git, Url, {branch, "HEAD"}});
update_source1(AppDir, {git, _Url, {branch, Branch}}) ->
    ShOpts = [{cd, AppDir}],
    rebar_utils:sh("git fetch origin", ShOpts),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Branch]), ShOpts),
    rebar_utils:sh(?FMT("git pull --ff-only --no-rebase -q origin ~s", [Branch]), ShOpts);
update_source1(AppDir, {git, _Url, {tag, Tag}}) ->
    ShOpts = [{cd, AppDir}],
    rebar_utils:sh("git fetch --tags origin", ShOpts),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Tag]), ShOpts);
update_source1(AppDir, {git, _Url, Refspec}) ->
    ShOpts = [{cd, AppDir}],
    rebar_utils:sh("git fetch origin", ShOpts),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Refspec]), ShOpts);
update_source1(AppDir, {svn, _Url, Rev}) ->
    rebar_utils:sh(?FMT("svn up -r ~s", [Rev]), [{cd, AppDir}]);
update_source1(AppDir, {hg, _Url, Rev}) ->
    rebar_utils:sh(?FMT("hg pull -u -r ~s", [Rev]), [{cd, AppDir}]);
update_source1(AppDir, {bzr, _Url, Rev}) ->
    rebar_utils:sh(?FMT("bzr update -r ~s", [Rev]), [{cd, AppDir}]);
update_source1(AppDir, {rsync, Url}) ->
    rebar_utils:sh(?FMT("rsync -az --delete ~s/ ~s",[Url,AppDir]),[]);
update_source1(AppDir, {fossil, Url}) ->
    update_source1(AppDir, {fossil, Url, ""});
update_source1(AppDir, {fossil, Url, latest}) ->
    update_source1(AppDir, {fossil, Url, ""});
update_source1(AppDir, {fossil, _Url, Version}) ->
    ok = file:set_cwd(AppDir),
    rebar_utils:sh("fossil pull", [{cd, AppDir}]),
    rebar_utils:sh(?FMT("fossil update ~s", [Version]), []).

download_source(AppDir, {hg, Url, Rev}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("hg clone -U ~s ~s", [Url, filename:basename(AppDir)]),
                   [{cd, filename:dirname(AppDir)}]),
    rebar_utils:sh(?FMT("hg update ~s", [Rev]), [{cd, AppDir}]);
download_source(AppDir, {git, Url}) ->
    download_source(AppDir, {git, Url, {branch, "HEAD"}});
download_source(AppDir, {git, Url, ""}) ->
    download_source(AppDir, {git, Url, {branch, "HEAD"}});
download_source(AppDir, {git, Url, {branch, Branch}}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(AppDir)]),
                   [{cd, filename:dirname(AppDir)}]),
    rebar_utils:sh(?FMT("git checkout -q origin/~s", [Branch]), [{cd, AppDir}]);
download_source(AppDir, {git, Url, {tag, Tag}}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(AppDir)]),
                   [{cd, filename:dirname(AppDir)}]),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Tag]), [{cd, AppDir}]);
download_source(AppDir, {git, Url, Rev}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(AppDir)]),
                   [{cd, filename:dirname(AppDir)}]),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Rev]), [{cd, AppDir}]);
download_source(AppDir, {bzr, Url, Rev}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("bzr branch -r ~s ~s ~s",
                        [Rev, Url, filename:basename(AppDir)]),
                   [{cd, filename:dirname(AppDir)}]);
download_source(AppDir, {svn, Url, Rev}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("svn checkout -r ~s ~s ~s",
                        [Rev, Url, filename:basename(AppDir)]),
                   [{cd, filename:dirname(AppDir)}]);
download_source(AppDir, {rsync, Url}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("rsync -az --delete ~s/ ~s", [Url, AppDir]), []);
download_source(AppDir, {fossil, Url}) ->
    download_source(AppDir, {fossil, Url, ""});
download_source(AppDir, {fossil, Url, latest}) ->
    download_source(AppDir, {fossil, Url, ""});
download_source(AppDir, {fossil, Url, Version}) ->
    Repository = filename:join(AppDir, filename:basename(AppDir) ++ ".fossil"),
    ok = filelib:ensure_dir(Repository),
    ok = file:set_cwd(AppDir),
    rebar_utils:sh(?FMT("fossil clone ~s ~s", [Url, Repository]),
                   [{cd, AppDir}]),
    rebar_utils:sh(?FMT("fossil open ~s ~s --nested", [Repository, Version]),
                   []).
