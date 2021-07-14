% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(match).

-export([
    match_test_a_Ok/0,
    match_test_b_Bad/0,
    match_test_c_Ok/0,
    match_test_d_Ok/0,
    match_test_e_Bad/0,
    match_test_f_Ok/0,
    match_test_g_Bad/0
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

%% internal
%% These functions are used to fool the compiler, which would warn if these were inlined.

only_accepts_one(1) -> ok.
two() -> 2.
