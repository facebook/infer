% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch).

-export([
    list_match_test_a/0,
    list_match_test_b/0,
    list_match_test_c/0,
    match_test_a_Ok/0,
    match_test_b_Bad/0,
    fp_match_test_c_Ok/0,
    fp_match_test_d_Ok/0,
    match_test_e_Bad/0,
    match_test_f_Ok/0,
    match_test_g_Bad/0
]).

tail([_ | Xs]) -> Xs.
assert_empty([]) -> ok.
assert_second_is_nil([_, [] | _]) -> ok.

list_match_test_a() ->
    tail([1, 2]),
    tail([1]),
    tail([]).

list_match_test_b() ->
    assert_empty([]),
    assert_empty([1]),
    assert_empty([1, 2]).

list_match_test_c() ->
    % FP (T94492137)
    assert_second_is_nil([1, [], 2]),
    assert_second_is_nil([1, []]),
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

%% internal
%% These functions are used to fool the compiler, which would warn if these were inlined.

only_accepts_one(1) -> ok.
two() -> 2.
