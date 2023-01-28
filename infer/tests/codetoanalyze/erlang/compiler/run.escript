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
        % Ignore details, we don't need them and might change across versions
        error:{Reason, _Details} -> io:format("~w~n", [Reason]);
        error:Error -> io:format("~w~n", [Error])
    end.

is_test_func(Name) ->
    lists:prefix("test_", Name) or lists:prefix("fp_test_", Name) or lists:prefix("fn_test_", Name).

run_tests(File) ->
    case compile:file(File, []) of
        {ok, Module} ->
            io:format("----- ~s -----~n", [Module]),
            Exports = Module:module_info(exports),
            Tests = lists:filter(
                fun({F, A}) -> (A =:= 0) and is_test_func(atom_to_list(F)) end,
                Exports
            ),
            lists:foreach(
                fun({F, 0}) -> run_test(Module, F) end,
                lists:sort(Tests)
            );
        error ->
            io:format("Compile error for file ~s.~n", [File])
    end,
    io:format("~n").

process_path(Path) ->
    case file:list_dir(Path) of
        {ok, Filenames} ->
            ErlFiles = lists:filtermap(
                fun(F) ->
                    case lists:suffix(".erl", F) of
                        true -> {true, filename:join(Path, F)};
                        _ -> false
                    end
                end,
                Filenames
            ),
            lists:foreach(fun run_tests/1, lists:sort(ErlFiles));
        {error, enoent} ->
            io:format("Directory '~s' does not exist.~n", [Path]);
        {error, eacces} ->
            io:format("Missing permissions for directory '~s'.~n", [Path]);
        {error, Reason} ->
            io:format("Error while reading directory '~s': ~w.~n", [Path, Reason])
    end.

main([H | T]) ->
    lists:foreach(fun process_path/1, lists:sort([H | T]));
main(_) ->
    usage().
