% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(topl_taint).

-export([
    test_a_Bad/0,
    test_b_Ok/0,
    test_c_Bad/0,
    test_c2_Ok/0,
    test_d_Bad/0,
    test_e_Ok/0,
    test_f_Bad/0,
    test_g_Ok/0,
    test_h_Bad/0,
    test_i_Ok/0,
    test_j_Bad/0,
    tito_process/0,
    sanitizer_process/0,
    test_send1_Ok/0,
    fnl_test_send2_Bad/0,
    test_send3_Ok/0,
    fpl_test_send4_Ok/0,
    test_send5_Ok/0,
    fnl_test_send6_Bad/0,
    test_send7_Ok/0,
    fpl_test_send8_Ok/0
]).

test_a_Bad() ->
    sink(source()).

test_b_Ok() ->
    sink(1).

test_c_Bad() ->
    X = source(),
    sink(X).

test_c2_Ok() ->
    _X = source(),
    sink(notdirty).

test_d_Bad() ->
    call_sink_indirectly(tito(source())).

test_e_Ok() ->
    call_sink_indirectly(tito(ok)).

test_f_Bad() ->
    case one() + one() - two() =:= 0 of
        true -> sink(dirty_if_argument_nil([]));
        false -> sink(dirty_if_argument_nil([1]))
    end.

test_g_Ok() ->
    case one() + one() - two() =/= 0 of
        true -> sink(dirty_if_argument_nil([]));
        false -> sink(dirty_if_argument_nil([1]))
    end.

test_h_Bad() ->
    case one() + one() of
        1 -> sink(dirty_if_argument_nil([ok]));
        2 -> sink(dirty_if_argument_nil([]))
    end.

test_i_Ok() ->
    case two() of
        1 -> sink(dirty_if_argument_nil(bad));
        2 -> sink(dirty_if_argument_nil([ok, ok, ok]))
    end.

% Bad because no case for 3 (not a topl property)
test_j_Bad() ->
    case two() + two() - one() of
        1 -> sink(dirty_if_argument_nil(bad));
        2 -> sink(dirty_if_argument_nil([ok, ok, ok]))
    end.

%% Tests with message passing

tito_process() ->
    receive
        {From, X} -> From ! X
    end.

sanitizer_process() ->
    receive
        {From, _} -> From ! not_dirty
    end.

test_send1_Ok() ->
    Pid = spawn(topl_taint, tito_process, []),
    Pid ! {self(), not_dirty},
    receive
        X -> sink(X)
    end.

fnl_test_send2_Bad() ->
    Pid = spawn(topl_taint, tito_process, []),
    Pid ! {self(), source()},
    receive
        X -> sink(X)
    end.

test_send3_Ok() ->
    Pid = spawn(topl_taint, sanitizer_process, []),
    Pid ! {self(), not_dirty},
    receive
        X -> sink(X)
    end.

fpl_test_send4_Ok() ->
    Pid = spawn(topl_taint, sanitizer_process, []),
    Pid ! {self(), source()},
    receive
        X -> sink(X)
    end.

test_send5_Ok() ->
    Pid = spawn(fun() -> tito_process() end),
    Pid ! {self(), not_dirty},
    receive
        X -> sink(X)
    end.

% The violation from the test below is not reported
fnl_test_send6_Bad() ->
    Pid = spawn(fun() -> tito_process() end),
    Pid ! {self(), source()},
    receive
        X -> sink(X)
    end.

test_send7_Ok() ->
    Pid = spawn(fun() -> sanitizer_process() end),
    Pid ! {self(), not_dirty},
    receive
        X -> sink(X)
    end.

fpl_test_send8_Ok() ->
    Pid = spawn(fun() -> sanitizer_process() end),
    Pid ! {self(), source()},
    receive
        X -> sink(X)
    end.

%%

% tito = Taint-In Taint-Out
tito(X) -> X.
call_sink_indirectly(X) -> call_sink(X).
call_sink(X) -> sink(X).
dirty_if_argument_nil([]) -> source();
dirty_if_argument_nil([X | _]) -> X.
one() -> 1.
two() -> 2.

%%
source() -> dirty.
% This should be something that crashes runtime (for our compiler tests),
% but is not reported by Pulse (so that we get TOPL error in TOPL tests).
sink(dirty) -> erlang:error(taint_error);
sink(_) -> ok.
