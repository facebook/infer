#!/usr/bin/env escript
% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
%
%
% Usage:
%     erlang.escript [ast_out_dir] -- rebar3 [args ...]
%     erlang.escript [ast_out_dir] -- erlc [args ...]
%
% This script produces a bash command that makes rebar3 or erlc
% to execute with [args ...], and in addition to write the JSON
% representation of the Erlang AST for each file compiled
% in [ast_out_dir] or - if not provided - in the build
% directory next to the corresponding compiled beam.

main([]) ->
    usage();
main(Args) ->
    {SArgs, Cmd} = split_args(Args),
    OutDir =
        case SArgs of
            [] -> false;
            [Dir] -> Dir;
            _ -> usage()
        end,
    ScriptDir = filename:dirname(escript:script_name()),
    ParseTransformDir = filename:join(ScriptDir, "infer_parse_transform"),
    case run("rebar3 compile", ParseTransformDir) of
        0 ->
            ok;
        ExitStatus ->
            io:format("error: `rebar3 compile` in `~s` returned exit code ~p~n", [
                ParseTransformDir,
                ExitStatus
            ]),
            halt(1)
    end,
    LibPath = filename:join(ParseTransformDir, "_build/default/lib"),
    case Cmd of
        ["rebar3" | _] ->
            rebar3(LibPath, OutDir, Cmd);
        ["erlc" | _] ->
            erlc(LibPath, OutDir, Cmd);
        _ ->
            io:format("error: unrecognized command ~s~n", [string:join(Cmd, " ")]),
            halt(1)
    end.

usage() ->
    io:format("valid arguments:~n"),
    io:format("  [ast_out_dir] -- rebar3 [args] ...~n"),
    io:format("  [ast_out_dir] -- erlc [args] ...~n"),
    halt(1).

load_config_from_list([]) ->
    false;
load_config_from_list([H | T]) ->
    case load_config(H) of
        {ok, Config} -> Config;
        _ -> load_config_from_list(T)
    end.

load_config(ConfigPath) when is_list(ConfigPath) ->
    case lists:suffix(".script", ConfigPath) of
        true ->
            BaseConfigPath = filename:rootname(ConfigPath, ".script"),
            BaseConfig =
                case load_config(BaseConfigPath) of
                    {ok, Config} -> Config;
                    _ -> []
                end,
            file:script(ConfigPath, [{'CONFIG', BaseConfig}, {'SCRIPT', ConfigPath}]);
        false ->
            file:consult(ConfigPath)
    end;
load_config(_) ->
    false.

split_args(Args) ->
    try
        split_args_rec(Args, [])
    catch
        _:_ -> usage()
    end.

split_args_rec(["--" | RebarCmd], Args) -> {Args, RebarCmd};
split_args_rec([H | T], Args) -> split_args_rec(T, Args ++ [H]).

run(Command, Dir) ->
    Port = erlang:open_port(
        {spawn, Command},
        [exit_status, {cd, Dir}]
    ),
    receive
        {Port, {exit_status, Status}} -> Status
    end.

rebar3(LibPath, OutDir, Cmd) ->
    ConfigPaths = [os:getenv("REBAR_CONFIG"), "rebar.config.script", "rebar.config"],
    Original =
        case load_config_from_list(ConfigPaths) of
            false ->
                io:format("error: no rebar3 config found~n"),
                halt(1);
            Config ->
                Config
        end,
    Altered = inject_parse_transform(Original, OutDir),
    AltConfigPath = string:trim(os:cmd("mktemp --suffix .script")),
    file:write_file(AltConfigPath, io_lib:fwrite("~p.~n", [Altered])),

    io:format("ERL_LIBS=\"~s:$ERL_LIBS\" REBAR_CONFIG=\"~s\" ~s~n", [
        LibPath,
        AltConfigPath,
        string:join(Cmd, " ")
    ]).

erlc(LibPath, OutDir, Cmd) ->
    [{erl_opts, Options}] = inject_parse_transform([], OutDir),
    OptionList = ["+'" ++ io_lib:format("~p", [Item]) ++ "'" || Item <- Options],
    [ErlC | Args] = Cmd,
    io:format("ERL_LIBS=\"~s:$ERL_LIBS\" ~s ~s ~s~n", [
        LibPath,
        ErlC,
        string:join(OptionList, " "),
        string:join(Args, " ")
    ]).

inject_parse_transform(Original, OutDir) ->
    ErlOpts =
        case lists:keyfind(erl_opts, 1, Original) of
            {erl_opts, Opts} -> Opts;
            false -> []
        end,
    ErlOpts1 =
        ErlOpts ++
            [{parse_transform, infer_parse_transform}] ++
            if
                OutDir =/= false ->
                    [{ast_outdir, OutDir}];
                true ->
                    []
            end,
    lists:keystore(erl_opts, 1, Original, {erl_opts, ErlOpts1}).
