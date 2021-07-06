% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch).

-export([
    list_match_test_tail1_Ok/0,
    list_match_test_tail2_Ok/0,
    list_match_test_tail3_Bad/0,
    list_match_test_empty1_Ok/0,
    list_match_test_empty2_Bad/0,
    list_match_test_empty3_Bad/0,
    fp_list_match_test_secondnil1_Ok/0,
    fp_list_match_test_secondnil2_Ok/0,
    list_match_test_secondnil3_Bad/0,
    match_test_a_Ok/0,
    match_test_b_Bad/0,
    fp_match_test_c_Ok/0,
    fp_match_test_d_Ok/0,
    match_test_e_Bad/0,
    match_test_f_Ok/0,
    match_test_g_Bad/0,
    case_test_simple1_Ok/0,
    case_test_simple2_Ok/0,
    case_test_simple3_Bad/0,
    case_test_tail1_Ok/0,
    case_test_tail2_Ok/0,
    case_test_tail3_Bad/0
]).

tail([_ | Xs]) -> Xs.
assert_empty([]) -> ok.
assert_second_is_nil([_, [] | _]) -> ok.

list_match_test_tail1_Ok() ->
    tail([1, 2]).
list_match_test_tail2_Ok() ->
    tail([1]).
list_match_test_tail3_Bad() ->
    tail([]).

list_match_test_empty1_Ok() ->
    assert_empty([]).
list_match_test_empty2_Bad() ->
    assert_empty([1]).
list_match_test_empty3_Bad() ->
    assert_empty([1, 2]).

% FP (T94492137)
fp_list_match_test_secondnil1_Ok() ->
    assert_second_is_nil([1, [], 2]).
% FP (T94492137)
fp_list_match_test_secondnil2_Ok() ->
    assert_second_is_nil([1, []]).
list_match_test_secondnil3_Bad() ->
    assert_second_is_nil([1, [2], 3]).

match_test_a_Ok() ->
    _X = two().

match_test_b_Bad() ->
    [_ | _] = two().

% FP (T94492137)
fp_match_test_c_Ok() ->
    [_X, _Y] = [1, 2].

% FP (T94492137)
fp_match_test_d_Ok() ->
    [_ | Xs] = [1, 2],
    tail(Xs).

match_test_e_Bad() ->
    [_ | Xs] = [1],
    tail(Xs).

match_test_f_Ok() ->
    X = (Y = 1),
    only_accepts_one(X),
    only_accepts_one(Y).

match_test_g_Bad() ->
    X = 2,
    only_accepts_one(X).

case_simple(X) ->
    case X of
        0 -> zero;
        1 -> one
    end.

tail_with_case(X) ->
    case X of
        [_|T] -> T
    end.

case_test_simple1_Ok() ->
    case_simple(0).
case_test_simple2_Ok() ->
    case_simple(1).
case_test_simple3_Bad() ->
    case_simple(2).

case_test_tail1_Ok() ->
    tail_with_case([1, 2]).
case_test_tail2_Ok() ->
    tail_with_case([1]).
case_test_tail3_Bad() ->
    tail_with_case([]).

%% internal
%% These functions are used to fool the compiler, which would warn if these were inlined.

only_accepts_one(1) -> ok.
two() -> 2.
