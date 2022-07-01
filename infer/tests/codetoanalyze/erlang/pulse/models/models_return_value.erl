% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(models_return_value).
-include("../../common.hrl").

-export([
    test_return_true_Ok/0,
    test_return_true_Bad/0,
    test_return_one_Ok/0,
    test_return_one_Bad/0,
    test_return_tuple_Ok/0,
    test_return_tuple_Bad/0,
    fp_test_return_nondet_Ok/0,
    test_return_nondet_Bad/0,
    test_nondet_atom_Ok/0,
    test_nondet_atom_Bad/0,
    test_nested_Ok/0,
    test_nested_Bad/0,
    test_select_arity0_Ok/0,
    test_select_arity0_Bad/0,
    test_select_arity1_Ok/0,
    test_select_arity1_Bad/0,
    test_select_arity2_Ok/0,
    test_select_arity2_Bad/0,
    test_select_arity3_Ok/0,
    test_select_arity3_Bad/0,
    test_select_arity4_Ok/0,
    test_select_arity4_Bad/0,
    fp_test_select_arity5_Ok/0,
    fn_test_select_arity5_Bad/0
]).

%%% Exported functions first.

test_return_true_Ok() ->
    ?ASSERT_EQUAL(true, get_true()).
test_return_true_Bad() ->
    ?CRASH_IF_EQUAL(true, get_true()).

test_return_one_Ok() ->
    ?ASSERT_EQUAL(1, get_one()).
test_return_one_Bad() ->
    ?CRASH_IF_EQUAL(1, get_one()).

test_return_tuple_Ok() ->
    ?ASSERT_EQUAL({true, _X}, get_pair_true_nondet()).
test_return_tuple_Bad() ->
    ?CRASH_IF_EQUAL({true, _X}, get_pair_true_nondet()).

% FP: We should angelically assume that the returned value does not lead to error.
fp_test_return_nondet_Ok() ->
    case get_pair_true_nondet() of
        {_, 1} -> ?UNEXPECTED_CRASH;
        _ -> ok
    end.
test_return_nondet_Bad() ->
    case get_pair_true_nondet() of
        {_, _} -> ?EXPECTED_CRASH;
        _ -> ok
    end.

test_nondet_atom_Ok() ->
    case get_nondet_atom() of
        X when is_atom(X) -> ok;
        _ -> ?UNEXPECTED_CRASH
    end.
test_nondet_atom_Bad() ->
    case get_nondet_atom() of
        X when is_atom(X) -> ?EXPECTED_CRASH;
        _ -> ok
    end.

test_nested_Ok() ->
    ?ASSERT_EQUAL({12345678901234567890, [], [_], [one, 2, {{}, -3}]}, get_nested()).
test_nested_Bad() ->
    ?CRASH_IF_EQUAL({12345678901234567890, [], [_], [one, 2, {{}, -3}]}, get_nested()).

test_select_arity0_Ok() ->
    ?ASSERT_EQUAL(0, get_arity()).
test_select_arity0_Bad() ->
    ?CRASH_IF_EQUAL(0, get_arity()).

test_select_arity1_Ok() ->
    ?ASSERT_EQUAL(1, get_arity(1)).
test_select_arity1_Bad() ->
    ?CRASH_IF_EQUAL(1, get_arity(1)).

test_select_arity2_Ok() ->
    ?ASSERT_EQUAL(2, get_arity(1, 2)).
test_select_arity2_Bad() ->
    ?CRASH_IF_EQUAL(2, get_arity(1, 2)).

test_select_arity3_Ok() ->
    ?ASSERT_EQUAL(3, get_arity(1, 2, 3)).
test_select_arity3_Bad() ->
    ?CRASH_IF_EQUAL(3, get_arity(1, 2, 3)).

test_select_arity4_Ok() ->
    ?ASSERT_EQUAL(4, get_arity(1, 2, 3, 4)).
test_select_arity4_Bad() ->
    ?CRASH_IF_EQUAL(4, get_arity(1, 2, 3, 4)).

%FP: arity problem in T110841433
fp_test_select_arity5_Ok() ->
    ?ASSERT_EQUAL(5, get_arity(1, 2, 3, 4, 5)).

%FN: arity problem in T110841433
fn_test_select_arity5_Bad() ->
    ?CRASH_IF_EQUAL(5, get_arity(1, 2, 3, 4, 5)).

%%% Internal functions follow.

% Model in .inferconfig overrides with 'true'.
get_true() -> false.

% Model in .inferconfig overrides with '1'.
get_one() -> 0.

% Model in .inferconfig overrides with '{true,_}'.
get_pair_true_nondet() -> false.

% Model in .inferconfig overrides with a value known to be atom BUT unknown which atom.
get_nondet_atom() -> 1.

% Model in .inferconfig overrides with '{12345678901234567890,[],[_],[one,2,{{},-3}]}'.
get_nested() -> 0.

% Models in .inferconfig override with the proper arity (0, 1, 2, ...).
get_arity() -> bad.
get_arity(_) -> bad.
get_arity(_, _) -> bad.
get_arity(_, _, _) -> bad.
get_arity(_, _, _, _) -> bad.
get_arity(_, _, _, _, _) -> bad.
