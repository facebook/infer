% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(features_comparison).

-export([
    test_equal_Ok/0,
    fp_test_equal2_Ok/0,
    test_equal_Bad/0,
    test_neg_equal_Ok/0,
    test_neg_equal_Bad/0,
    test_equal_int_atom_Bad/0,
    test_neg_equal_int_atom_Ok/0,
    fp_test_equal_atom_Ok/0,
    test_equal_atom_Bad/0,
    fp_test_neg_equal_atom_Ok/0,
    test_neg_equal_atom_Bad/0,
    test_exactly_equal_Ok/0,
    test_exactly_equal_Bad/0,
    test_exactly_equal2_Bad/0,
    test_not_equal_Ok/0,
    test_not_equal_Bad/0,
    test_not_equal2_Bad/0,
    test_exactly_not_equal_Ok/0,
    fp_test_exactly_not_equal2_Ok/0,
    test_exactly_not_equal_Bad/0,
    test_greater_Ok/0,
    test_greater_Bad/0,
    test_greater2_Bad/0,
    test_less_Ok/0,
    test_less_Bad/0,
    test_less2_Bad/0,
    test_atleast_Ok/0,
    test_atleast2_Ok/0,
    test_atleast_Bad/0,
    test_atmost_Ok/0,
    test_atmost2_Ok/0,
    test_atmost_Bad/0
]).

% Call this method with false to trigger a warning to expect
assert(true) -> ok.

test_equal_Ok() ->
    X = 2,
    Y = 2,
    assert(X == Y).

% FP (T95767672)
fp_test_equal2_Ok() ->
    X = 2,
    Y = 2.0,
    assert(X == Y).

test_equal_Bad() ->
    X = 2,
    Y = 3,
    assert(X == Y).

test_neg_equal_Ok() ->
    X = 0,
    Y = 42,
    assert(not (X == Y)).

test_neg_equal_Bad() ->
    X = 0,
    Y = 0,
    assert(not (X == Y)).

test_equal_int_atom_Bad() ->
    X = zero,
    Y = 0,
    assert(X == Y).

test_neg_equal_int_atom_Ok() ->
    X = zero,
    Y = 0,
    assert(not (X == Y)).

fp_test_equal_atom_Ok() ->
    X = foo,
    Y = foo,
    assert(X == Y).

test_equal_atom_Bad() ->
    X = foo,
    Y = bar,
    assert(X == Y).

fp_test_neg_equal_atom_Ok() ->
    X = foo,
    Y = bar,
    assert(not (X == Y)).

test_neg_equal_atom_Bad() ->
    X = foo,
    Y = foo,
    assert(not (X == Y)).

test_exactly_equal_Ok() ->
    X = 2,
    Y = 2,
    assert(X =:= Y).

test_exactly_equal_Bad() ->
    X = 2,
    Y = 3,
    assert(X =:= Y).

test_exactly_equal2_Bad() ->
    X = 2,
    Y = 2.0,
    assert(X =:= Y).

test_not_equal_Ok() ->
    X = 2,
    Y = 3,
    assert(X /= Y).

test_not_equal_Bad() ->
    X = 2,
    Y = 2,
    assert(X /= Y).

test_not_equal2_Bad() ->
    X = 2,
    Y = 2.0,
    assert(X /= Y).

test_exactly_not_equal_Ok() ->
    X = 2,
    Y = 3,
    assert(X =/= Y).

% FP (T95767672)
fp_test_exactly_not_equal2_Ok() ->
    X = 2,
    Y = 2.0,
    assert(X =/= Y).

test_exactly_not_equal_Bad() ->
    X = 2,
    Y = 2,
    assert(X =/= Y).

test_greater_Ok() ->
    X = 3,
    Y = 2,
    assert(X > Y).

test_greater_Bad() ->
    X = 2,
    Y = 3,
    assert(X > Y).

test_greater2_Bad() ->
    X = 2,
    Y = 2,
    assert(X > Y).

test_less_Ok() ->
    X = 2,
    Y = 3,
    assert(X < Y).

test_less_Bad() ->
    X = 3,
    Y = 2,
    assert(X < Y).

test_less2_Bad() ->
    X = 2,
    Y = 2,
    assert(X < Y).

test_atleast_Ok() ->
    X = 2,
    Y = 2,
    assert(X >= Y).

test_atleast2_Ok() ->
    X = 3,
    Y = 2,
    assert(X >= Y).

test_atleast_Bad() ->
    X = 2,
    Y = 3,
    assert(X >= Y).

test_atmost_Ok() ->
    X = 2,
    Y = 2,
    assert(X =< Y).

test_atmost2_Ok() ->
    X = 2,
    Y = 3,
    assert(X =< Y).

test_atmost_Bad() ->
    X = 3,
    Y = 2,
    assert(X =< Y).
