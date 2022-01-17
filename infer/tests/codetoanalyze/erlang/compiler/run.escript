#!/usr/bin/env escript
% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-mode(compile).

usage() ->
    Usage =
        "Usage:\n"
        "    run.escript <directories>\n"
        "\n"
        "This script runs all test functions of all Erlang files\n"
        "in the directories specified as arguments and prints the\n"
        "outcome (ok/fail).\n",
    io:format("~s", [Usage]),
    halt(1).

run_test(Module, Function) ->
    io:format("~s:~s/0: ", [Module, Function]),
    try
        Module:Function(),
        io:format("ok~n")
    catch
        error:Error -> io:format("~w~n", [Error])
    end.

run_tests(File) ->
    {ok, Module} = compile:file(File, []),
    io:format("----- ~s -----~n", [Module]),
    Exports = Module:module_info(exports),
    Tests = lists:filter(
        fun({F, A}) ->
            Fn = atom_to_list(F),
            (A =:= 0) and
                (lists:prefix("test", Fn) or lists:prefix("fp_", Fn) or lists:prefix("fn_", Fn))
        end,
        Exports
    ),
    lists:foreach(
        fun({F, 0}) -> run_test(Module, F) end,
        lists:sort(Tests)
    ),
    io:format("~n").

process_path(Path) ->
    {ok, Filenames} = file:list_dir(Path),
    ErlFiles = lists:filtermap(
        fun(F) ->
            case lists:suffix(".erl", F) of
                true -> {true, filename:join(Path, F)};
                _ -> false
            end
        end,
        Filenames
    ),
    lists:foreach(fun run_tests/1, lists:sort(ErlFiles)).

main([H|T]) ->
    lists:foreach(fun process_path/1, lists:sort([H|T]));
main(_) ->
    usage().
