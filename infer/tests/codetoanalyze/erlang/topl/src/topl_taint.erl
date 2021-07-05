% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(topl_taint).

-export([
    test_a_Bad/0,
    test_b_Ok/0,
    fn_test_c_Bad/0,
    test_d_Bad/0,
    test_e_Ok/0,
    fp_test_f_Bad/0,
    fp_test_g_Ok/0,
    test_h_Bad/0,
    test_i_Ok/0,
    test_j_Bad/0
]).

test_a_Bad() ->
    sink(source()).

test_b_Ok() ->
    sink(1).

% FN because match not yet translated
fn_test_c_Bad() ->
    X = source(),
    sink(X).

test_d_Bad() ->
    call_sink_indirectly(tito(source())).

test_e_Ok() ->
    call_sink_indirectly(tito(ok)).

% FP: T94670024 (bad, but wrong warning reported)
fp_test_f_Bad() ->
    case one() + one() - two() =:= 0 of
        true -> sink(dirty_if_argument_nil([]));
        false -> sink(dirty_if_argument_nil([1]))
    end.

% FP: T94670024
fp_test_g_Ok() ->
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
sink(_) -> ok.
