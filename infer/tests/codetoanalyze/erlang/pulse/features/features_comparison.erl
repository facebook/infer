% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_comparison).

-export([
    test_equal_Ok/0,
    fp_test_equal_Ok2/0,
    test_equal_Bad/0,
    test_exactly_equal_Ok/0,
    test_exactly_equal_Bad/0,
    test_exactly_equal_Bad2/0,
    test_not_equal_Ok/0,
    test_not_equal_Bad/0,
    test_not_equal_Bad2/0,
    test_exactly_not_equal_Ok/0,
    fp_test_exactly_not_equal_Ok2/0,
    test_exactly_not_equal_Bad/0,
    test_greater_Ok/0,
    test_greater_Bad/0,
    test_greater_Bad2/0,
    test_less_Ok/0,
    test_less_Bad/0,
    test_less_Bad2/0,
    test_atleast_Ok/0,
    test_atleast_Ok2/0,
    test_atleast_Bad/0,
    test_atmost_Ok/0,
    test_atmost_Ok2/0,
    test_atmost_Bad/0
]).

test_equal_Ok() ->
    X = 2,
    Y = 2,
    if
        X == Y -> ok
    end.

% FP (T95767672)
fp_test_equal_Ok2() ->
    X = 2,
    Y = 2.0,
    if
        X == Y -> ok
    end.

test_equal_Bad() ->
    X = 2,
    Y = 3,
    if
        X == Y -> ok
    end.

test_exactly_equal_Ok() ->
    X = 2,
    Y = 2,
    if
        X =:= Y -> ok
    end.

test_exactly_equal_Bad() ->
    X = 2,
    Y = 3,
    if
        X =:= Y -> ok
    end.

test_exactly_equal_Bad2() ->
    X = 2,
    Y = 2.0,
    if
        X =:= Y -> ok
    end.

test_not_equal_Ok() ->
    X = 2,
    Y = 3,
    if
        X /= Y -> ok
    end.

test_not_equal_Bad() ->
    X = 2,
    Y = 2,
    if
        X /= Y -> ok
    end.

test_not_equal_Bad2() ->
    X = 2,
    Y = 2.0,
    if
        X /= Y -> ok
    end.

test_exactly_not_equal_Ok() ->
    X = 2,
    Y = 3,
    if
        X =/= Y -> ok
    end.

% FP (T95767672)
fp_test_exactly_not_equal_Ok2() ->
    X = 2,
    Y = 2.0,
    if
        X =/= Y -> ok
    end.

test_exactly_not_equal_Bad() ->
    X = 2,
    Y = 2,
    if
        X =/= Y -> ok
    end.

test_greater_Ok() ->
    X = 3,
    Y = 2,
    if
        X > Y -> ok
    end.

test_greater_Bad() ->
    X = 2,
    Y = 3,
    if
        X > Y -> ok
    end.

test_greater_Bad2() ->
    X = 2,
    Y = 2,
    if
        X > Y -> ok
    end.

test_less_Ok() ->
    X = 2,
    Y = 3,
    if
        X < Y -> ok
    end.

test_less_Bad() ->
    X = 3,
    Y = 2,
    if
        X < Y -> ok
    end.

test_less_Bad2() ->
    X = 2,
    Y = 2,
    if
        X < Y -> ok
    end.

test_atleast_Ok() ->
    X = 2,
    Y = 2,
    if
        X >= Y -> ok
    end.

test_atleast_Ok2() ->
    X = 3,
    Y = 2,
    if
        X >= Y -> ok
    end.

test_atleast_Bad() ->
    X = 2,
    Y = 3,
    if
        X >= Y -> ok
    end.

test_atmost_Ok() ->
    X = 2,
    Y = 2,
    if
        X =< Y -> ok
    end.

test_atmost_Ok2() ->
    X = 2,
    Y = 3,
    if
        X =< Y -> ok
    end.

test_atmost_Bad() ->
    X = 3,
    Y = 2,
    if
        X =< Y -> ok
    end.
