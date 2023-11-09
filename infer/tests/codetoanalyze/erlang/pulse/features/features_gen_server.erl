% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_gen_server).
-include("../../common.hrl").

-export([
    test_call_and_cast_Ok/0,
    test_call1_Bad/0,
    test_call2_Bad/0,
    test_call3_Bad/0,
    test_call4_Bad/0,
    test_call5_Bad/0,
    test_cast_Bad/0,
    test_call_callee_Bad/0,
%% Not exported due to sporadic timeout in direct_erlang_compiler_test T169335380
%    test_start_link1_Bad/0,
    test_start_link2_Ok/0,
    test_start_link3_Ok/0
%% Not exported due to sporadic timeout in direct_erlang_compiler_test T169335380
%    test_start_link4_Bad/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

get_pid() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

call(Pid, What) ->
    gen_server:call(Pid, What).

run_test(Test) ->
    error_logger:tty(false),
    spawn_monitor(Test),
    receive
        {'DOWN', _, process, _, normal} ->
            ok;
        {'DOWN', _, process, _, {Error, _}} ->
            % propagate error occured in the process
            erlang:error(Error)
    end.

test_call_and_cast_Ok() ->
    Test = fun() ->
        {ok, Pid} = gen_server:start_link(?MODULE, [], []),
        expected = gen_server:call(Pid, expected),
        expected = gen_server:call(Pid, expected, 1000),
        ok = gen_server:cast(Pid, expected),
        Pid1 = get_pid(),
        expected = gen_server:call(Pid1, expected),
        expected = gen_server:call(Pid1, expected, 1000),
        ok = gen_server:cast(Pid1, expected),
        ok
    end,
    run_test(Test).

test_call1_Bad() ->
    Test = fun() ->
        {ok, Pid} = gen_server:start_link(?MODULE, [], []),
        gen_server:call(Pid, oops)
    end,
    run_test(Test).

test_call2_Bad() ->
    Test = fun() ->
        Pid = get_pid(),
        gen_server:call(Pid, oops)
    end,
    run_test(Test).

test_call3_Bad() ->
    Test = fun() ->
        Pid = get_pid(),
        gen_server:call(Pid, oops, 1000)
    end,
    run_test(Test).

test_call4_Bad() ->
    Test = fun() ->
        Pid = get_pid(),
        Result = gen_server:call(Pid, silent, 1),
        ?CRASH_IF_EQUAL(any, Result)
    end,
    run_test(Test).

test_call5_Bad() ->
    Test = fun() ->
        {ok, Pid} = gen_server:start_link(?MODULE, [], []),
        register(?MODULE, Pid),
        gen_server:call(?MODULE, oops)
    end,
    run_test(Test).

test_cast_Bad() ->
    Test = fun() ->
        Pid = get_pid(),
        gen_server:cast(Pid, oops),
        % cast is async, sleep is there to let process crash
        timer:sleep(1000)
    end,
    run_test(Test).

test_call_callee_Bad() ->
    Test = fun() ->
        Pid = get_pid(),
        call(Pid, oops)
    end,
    run_test(Test).

test_start_link1_Bad() ->
    {ok, _} = gen_server:start_link(?MODULE, do_crash, []).

test_start_link2_Ok() ->
    ignore = gen_server:start_link(?MODULE, do_ignore, []).

test_start_link3_Ok() ->
    {error, _} = gen_server:start_link(?MODULE, do_error, []).

test_start_link4_Bad() ->
    {ok, _} = gen_server:start_link(?MODULE, oops, []).

% server

init([]) -> {ok, []};
init(do_ignore) -> ignore;
init(do_error) -> {error, []};
init(do_crash) -> ?EXPECTED_CRASH.

handle_call(Request, _From, State) ->
    case Request of
        expected -> {reply, Request, State};
        oops -> ?EXPECTED_CRASH;
        silent -> {noreply, State}
    end.

handle_cast(Request, State) ->
    ?CRASH_IF_EQUAL(oops, Request),
    ?ASSERT_EQUAL(expected, Request),
    {noreply, State}.
