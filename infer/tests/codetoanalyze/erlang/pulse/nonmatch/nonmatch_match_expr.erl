% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_match_expr).

-export([
    match_test_a_Ok/0,
    match_test_b_Bad/0,
    match_test_c_Ok/0,
    match_test_d_Ok/0,
    match_test_e_Bad/0,
    match_test_f_Ok/0,
    match_test_g_Bad/0,
    match_in_pattern_test_a_Ok/0,
    match_in_pattern_test_b_Bad/0,
    match_in_pattern_test_c_Bad/0,
    match_in_pattern_test_d_Bad/0,
    match_in_pattern_test_e_Ok/0,
    match_in_pattern_test_f_Bad/0,
    match_nested1_Ok/0,
    match_nested2_Bad/0,
    match_nested3_Bad/0
]).

tail([_ | Xs]) -> Xs.

match_test_a_Ok() ->
    _X = two().

match_test_b_Bad() ->
    [_ | _] = two().

match_test_c_Ok() ->
    [_X, _Y] = [1, 2].

match_test_d_Ok() ->
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

match_in_pattern_test_a_Ok() ->
    X = 2,
    case X of
        2 = 2 -> ok
    end.

match_in_pattern_test_b_Bad() ->
    X = 3,
    case X of
        2 = 2 -> ok
    end.

match_in_pattern_test_c_Bad() ->
    X = 2,
    case X of
        2 = 3 -> ok
    end.

match_in_pattern_test_d_Bad() ->
    X = 3,
    case X of
        3 = 2 -> ok
    end.

match_in_pattern_test_e_Ok() ->
    X = 2,
    case X of
        Y = 2 -> ok
    end,
    case Y of
        2 -> ok
    end.

match_in_pattern_test_f_Bad() ->
    X = 1,
    case X of
        Y = 2 -> ok
    end.

match_nested1_Ok() ->
    (1 = X) = 1,
    only_accepts_one(X).

match_nested2_Bad() ->
    (1 = _X) = 2.

match_nested3_Bad() ->
    (2 = _X) = 1.

%% internal
%% These functions are used to fool the compiler, which would warn if these were inlined.

only_accepts_one(1) -> ok.
two() -> 2.
