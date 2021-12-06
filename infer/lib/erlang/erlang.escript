#!/usr/bin/env escript
% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

usage() ->
    Usage =
        "Usage:\n"
        "    erlang.escript <ast_out_dir> -- rebar3 [args ...]\n"
        "    erlang.escript <ast_out_dir> -- erlc [args ...]\n"
        "\n"
        "This script produces a bash command that makes rebar3 or erlc\n"
        "to execute with [args ...], and in addition to write in \n"
        "<ast_out_dir> the Erlang AST in JSON format for each file compiled\n",
    io:format("~s", [Usage]),
    halt(1).

main([]) ->
    usage();
main(Args) ->
    {SArgs, Cmd} = split_args(Args),
    OutDir =
        case SArgs of
            [Dir] -> Dir;
            _ -> usage()
        end,
    ScriptDir = filename:dirname(escript:script_name()),
    ParseTransformDir = filename:join(ScriptDir, "infer_parse_transform"),
    case run("rebar3 compile", ParseTransformDir) of
        {0, _} ->
            ok;
        {ExitStatus, Output} ->
            io:format(
                standard_error,
                "error: `rebar3 compile` in `~s` returned exit code ~p.~n Output was~n",
                [
                    ParseTransformDir,
                    ExitStatus
                ]
            ),
            lists:foreach(
                fun(Line) -> io:format(standard_error, "~s", [Line]) end,
                Output
            ),
            halt(1)
    end,
    CompiledListPath = mktemp(".list"),
    OutputCmd =
        case Cmd of
            ["rebar3" | _] ->
                rebar3(Cmd, CompiledListPath);
            ["erlc" | _] ->
                erlc(Cmd, CompiledListPath);
            _ ->
                io:format("error: unrecognized command ~s~n", [string:join(Cmd, " ")]),
                halt(1)
        end,
    LibPath = filename:join(ParseTransformDir, "_build/default/lib"),
    io:format(
        "export ERL_LIBS=\"~s:$ERL_LIBS\"; ~s && ~s/extract.escript ~s ~s~n",
        [LibPath, OutputCmd, ScriptDir, CompiledListPath, OutDir]
    ).

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
        [exit_status, {cd, Dir}, use_stdio, stream, stderr_to_stdout]
    ),
    run_manager(Port, []).

run_manager(Port, Output) ->
    receive
        {Port, {data, Line}} -> run_manager(Port, [Line | Output]);
        {Port, {exit_status, Status}} -> {Status, lists:reverse(Output)}
    end.

mktemp(Suffix) ->
    TempFilePath = string:trim(os:cmd("mktemp")),
    WithSuffix = TempFilePath ++ Suffix,
    os:cmd(io_lib:format("mv ~s ~s", [TempFilePath, WithSuffix])),
    WithSuffix.

rebar3(Cmd, CompiledListPath) ->
    ConfigPaths = [os:getenv("REBAR_CONFIG"), "rebar.config.script", "rebar.config"],
    Original =
        case load_config_from_list(ConfigPaths) of
            false ->
                io:format("error: no rebar3 config found~n"),
                halt(1);
            Config ->
                Config
        end,
    Altered = inject_parse_transform(Original, CompiledListPath),
    AltConfigPath = mktemp(".script"),
    file:write_file(AltConfigPath, io_lib:fwrite("~p.~n", [Altered])),

    io_lib:format("export REBAR_CONFIG=\"~s\"; ~s", [
        AltConfigPath,
        string:join(Cmd, " ")
    ]).

erlc(Cmd, CompiledListPath) ->
    [{erl_opts, Options}] = inject_parse_transform([], CompiledListPath),
    OptionList = ["+'" ++ io_lib:format("~p", [Item]) ++ "'" || Item <- Options],
    [ErlC | Args] = Cmd,
    io_lib:format("~s ~s ~s", [
        ErlC,
        string:join(OptionList, " "),
        string:join(Args, " ")
    ]).

inject_parse_transform(Original, CompiledListPath) ->
    ErlOpts =
        case lists:keyfind(erl_opts, 1, Original) of
            {erl_opts, Opts} -> Opts;
            _ -> []
        end,
    ErlOpts1 =
        case lists:member(debug_info, ErlOpts) of
            true -> ErlOpts;
            false -> ErlOpts ++ [debug_info]
        end,
    ErlOpts2 =
        ErlOpts1 ++
            [
                {parse_transform, infer_parse_transform},
                {infer_compiled_list_path, CompiledListPath}
            ],
    lists:keystore(erl_opts, 1, Original, {erl_opts, ErlOpts2}).
