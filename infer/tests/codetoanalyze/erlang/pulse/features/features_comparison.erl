% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(features_comparison).
-include("../../common.hrl").

-export([
    test_equal_Ok/0,
    fp_test_equal2_Ok/0,
    test_equal_Bad/0,
    test_neg_equal_Ok/0,
    test_neg_equal_Bad/0,
    test_equal_in_fun_call_Ok/0,
    test_neg_equal_in_fun_call_Ok/0,
    test_equal_int_atom_Bad/0,
    test_neg_equal_int_atom_Ok/0,
    test_equal_atom_Ok/0,
    test_equal_atom_Bad/0,
    test_equal_int_any_Latent/1,
    test_equal_int_any_2_Latent/1,
    test_equal_atom_any_Latent/1,
    test_equal_atom_any_2_Latent/1,
    fp_test_equal_string_any_Latent/1,
    fp_test_equal_string_any_2_Latent/1,
    fp_test_equal_any_any_Latent/2,
    fp_test_equal_any_any_2_Latent/2,
    test_neg_equal_atom_Ok/0,
    test_neg_equal_atom_Bad/0,
    test_exactly_equal_Ok/0,
    test_exactly_equal_Bad/0,
    test_exactly_equal2_Bad/0,
    test_not_equal_Ok/0,
    test_not_equal_Bad/0,
    test_not_equal2_Bad/0,
    test_not_equal_atom_Ok/0,
    test_not_equal_atom_Bad/0,
    test_not_equal_int_atom_Ok/0,
    test_exactly_not_equal_Ok/0,
    fp_test_exactly_not_equal2_Ok/0,
    test_exactly_not_equal_Bad/0,
    test_exactly_not_equal_atom_Ok/0,
    test_exactly_not_equal_atom_Bad/0,
    test_exactly_not_equal_int_atom_Ok/0,
    test_greater_Ok/0,
    test_greater_Bad/0,
    test_greater2_Bad/0,
    test_greater_int_atom_Ok/0,
    test_greater_int_atom_Bad/0,
    test_less_Ok/0,
    test_less_Bad/0,
    test_less2_Bad/0,
    test_less_int_atom_Ok/0,
    test_less_int_atom_Bad/0,
    test_atleast_Ok/0,
    test_atleast2_Ok/0,
    test_atleast_Bad/0,
    test_atleast_int_atom_Ok/0,
    test_atleast_int_atom_Bad/0,
    test_atmost_Ok/0,
    test_atmost2_Ok/0,
    test_atmost_Bad/0,
    test_atmost_int_atom_Ok/0,
    test_atmost_int_atom_Bad/0
]).

test_equal_Ok() ->
    X = 2,
    Y = 2,
    ?ASSERT_EQUAL(true, X == Y).

% FP (T95767672)
fp_test_equal2_Ok() ->
    X = 2,
    Y = 2.0,
    ?ASSERT_EQUAL(true, X == Y).

test_equal_Bad() ->
    X = 2,
    Y = 3,
    ?CRASH_IF_EQUAL(false, X == Y).

test_neg_equal_Ok() ->
    X = 0,
    Y = 42,
    ?ASSERT_EQUAL(true, not (X == Y)).

test_neg_equal_Bad() ->
    X = 0,
    Y = 0,
    ?CRASH_IF_EQUAL(false, not (X == Y)).

is_zero(X) -> X == 0.

% Used to be FP due to imprecise function summary because of absent type information
% Now a regression test on this behaviour
test_equal_in_fun_call_Ok() ->
    ?ASSERT_EQUAL(true, is_zero(0)).

% Used to be FP due to imprecise function summary because of absent type information
% Now a regression test on this behaviour
test_neg_equal_in_fun_call_Ok() ->
    ?ASSERT_EQUAL(false, is_zero(1)).

test_equal_int_atom_Bad() ->
    X = zero,
    Y = 0,
    ?CRASH_IF_EQUAL(false, X == Y).

test_neg_equal_int_atom_Ok() ->
    X = zero,
    Y = 0,
    ?ASSERT_EQUAL(true, not (X == Y)).

test_equal_atom_Ok() ->
    X = foo,
    Y = foo,
    ?ASSERT_EQUAL(true, X == Y).

test_equal_atom_Bad() ->
    X = foo,
    Y = bar,
    ?CRASH_IF_EQUAL(false, X == Y).

test_equal_int_any_Latent(X) ->
    ?CRASH_IF_EQUAL(true, X == 42).

test_equal_int_any_2_Latent(X) ->
    ?CRASH_IF_EQUAL(false, X == 42).

test_equal_atom_any_Latent(X) ->
    ?CRASH_IF_EQUAL(true, X == foo).

test_equal_atom_any_2_Latent(X) ->
    ?CRASH_IF_EQUAL(false, X == foo).

fp_test_equal_string_any_Latent(X) ->
    ?CRASH_IF_EQUAL(true, X == "foo").

fp_test_equal_string_any_2_Latent(X) ->
    ?CRASH_IF_EQUAL(false, X == "foo").

fp_test_equal_any_any_Latent(X, Y) ->
    ?CRASH_IF_EQUAL(true, X == Y).

fp_test_equal_any_any_2_Latent(X, Y) ->
    ?CRASH_IF_EQUAL(false, X == Y).

test_neg_equal_atom_Ok() ->
    X = foo,
    Y = bar,
    ?ASSERT_EQUAL(true, not (X == Y)).

test_neg_equal_atom_Bad() ->
    X = foo,
    Y = foo,
    ?CRASH_IF_EQUAL(false, not (X == Y)).

test_exactly_equal_Ok() ->
    X = 2,
    Y = 2,
    ?ASSERT_EQUAL(true, X =:= Y).

test_exactly_equal_Bad() ->
    X = 2,
    Y = 3,
    ?CRASH_IF_EQUAL(false, X =:= Y).

test_exactly_equal2_Bad() ->
    X = 2,
    Y = 2.0,
    ?CRASH_IF_EQUAL(false, X =:= Y).

test_not_equal_Ok() ->
    X = 2,
    Y = 3,
    ?ASSERT_EQUAL(true, X /= Y).

test_not_equal_Bad() ->
    X = 2,
    Y = 2,
    ?CRASH_IF_EQUAL(false, X /= Y).

test_not_equal2_Bad() ->
    X = 2,
    Y = 2.0,
    ?CRASH_IF_EQUAL(false, X /= Y).

test_not_equal_atom_Ok() ->
    X = foo,
    Y = bar,
    ?ASSERT_EQUAL(true, X /= Y).

test_not_equal_atom_Bad() ->
    X = foo,
    Y = foo,
    ?CRASH_IF_EQUAL(false, X /= Y).

test_not_equal_int_atom_Ok() ->
    X = zero,
    Y = 0,
    ?ASSERT_EQUAL(true, X /= Y).

test_exactly_not_equal_Ok() ->
    X = 2,
    Y = 3,
    ?ASSERT_EQUAL(true, X =/= Y).

% FP (T95767672)
fp_test_exactly_not_equal2_Ok() ->
    X = 2,
    Y = 2.0,
    ?ASSERT_EQUAL(true, X =/= Y).

test_exactly_not_equal_Bad() ->
    X = 2,
    Y = 2,
    ?CRASH_IF_EQUAL(false, X =/= Y).

test_exactly_not_equal_atom_Ok() ->
    X = foo,
    Y = bar,
    ?ASSERT_EQUAL(true, X =/= Y).

test_exactly_not_equal_atom_Bad() ->
    X = foo,
    Y = foo,
    ?CRASH_IF_EQUAL(false, X =/= Y).

test_exactly_not_equal_int_atom_Ok() ->
    X = zero,
    Y = 0,
    ?ASSERT_EQUAL(true, X =/= Y).

test_greater_Ok() ->
    X = 3,
    Y = 2,
    ?ASSERT_EQUAL(true, X > Y).

test_greater_Bad() ->
    X = 2,
    Y = 3,
    ?CRASH_IF_EQUAL(false, X > Y).

test_greater2_Bad() ->
    X = 2,
    Y = 2,
    ?CRASH_IF_EQUAL(false, X > Y).

test_greater_int_atom_Ok() ->
    X = zero,
    Y = 0,
    ?ASSERT_EQUAL(true, X > Y).

test_greater_int_atom_Bad() ->
    X = 0,
    Y = zero,
    ?CRASH_IF_EQUAL(false, X > Y).

test_less_Ok() ->
    X = 2,
    Y = 3,
    ?ASSERT_EQUAL(true, X < Y).

test_less_Bad() ->
    X = 3,
    Y = 2,
    ?CRASH_IF_EQUAL(false, X < Y).

test_less_int_atom_Ok() ->
    X = 0,
    Y = zero,
    ?ASSERT_EQUAL(true, X < Y).

test_less_int_atom_Bad() ->
    X = zero,
    Y = 0,
    ?CRASH_IF_EQUAL(false, X < Y).

test_less2_Bad() ->
    X = 2,
    Y = 2,
    ?CRASH_IF_EQUAL(false, X < Y).

test_atleast_Ok() ->
    X = 2,
    Y = 2,
    ?ASSERT_EQUAL(true, X >= Y).

test_atleast2_Ok() ->
    X = 3,
    Y = 2,
    ?ASSERT_EQUAL(true, X >= Y).

test_atleast_Bad() ->
    X = 2,
    Y = 3,
    ?CRASH_IF_EQUAL(false, X >= Y).

test_atleast_int_atom_Ok() ->
    X = zero,
    Y = 0,
    ?ASSERT_EQUAL(true, X >= Y).

test_atleast_int_atom_Bad() ->
    X = 0,
    Y = zero,
    ?CRASH_IF_EQUAL(false, X >= Y).

test_atmost_Ok() ->
    X = 2,
    Y = 2,
    ?ASSERT_EQUAL(true, X =< Y).

test_atmost2_Ok() ->
    X = 2,
    Y = 3,
    ?ASSERT_EQUAL(true, X =< Y).

test_atmost_Bad() ->
    X = 3,
    Y = 2,
    ?CRASH_IF_EQUAL(false, X =< Y).

test_atmost_int_atom_Ok() ->
    X = 0,
    Y = zero,
    ?ASSERT_EQUAL(true, X =< Y).

test_atmost_int_atom_Bad() ->
    X = zero,
    Y = 0,
    ?CRASH_IF_EQUAL(false, X =< Y).
