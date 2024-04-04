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
    Me = self(),
    Ref = make_ref(),
    io:format("~s:~s/0: ", [Module, Function]),
    spawn(fun() ->
        Result =
            try
                Module:Function(),
                ok
            catch
                % Ignore details, we don't need them and might change across versions
                error:{Reason, _Details} -> Reason;
                error:Error -> Error
            end,
        Me ! {Ref, Result}
    end),
    receive
        {Ref, Result} -> io:format("~w~n", [Result])
    after 10000 -> io:format("timeout~n")
    end.

% See ../README.md for naming conventions
is_test_func("test_" ++ _) -> true;
is_test_func("fp_test_" ++ _) -> true;
is_test_func("fn_test_" ++ _) -> true;
is_test_func("fpl_test_" ++ _) -> true;
is_test_func("fnl_test_" ++ _) -> true;
is_test_func(_) -> false.

run_tests(File) ->
    case compile:file(File, []) of
        {ok, Module} ->
            io:format("----- ~s -----~n", [Module]),
            Exports = Module:module_info(exports),
            Tests = lists:filter(
                fun
                    ({F, 0}) -> is_test_func(atom_to_list(F));
                    (_) -> false
                end,
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
    ErlFiles = filelib:wildcard("*.erl", Path),
    FullPaths = [filename:join(Path, F) || F <- ErlFiles],
    lists:foreach(fun run_tests/1, lists:sort(FullPaths)).

main([H | T]) ->
    lists:foreach(fun process_path/1, lists:sort([H | T]));
main(_) ->
    usage().
