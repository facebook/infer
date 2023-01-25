% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(models_arguments_return).
-include("../../common.hrl").

-export([
    test_range0_Ok/0,
    test_range0_Bad/0,
    test_range1_Ok/0,
    test_range1_Bad/0,
    test_range2_Ok/0,
    test_range2_Bad/0,
    test_range3_Ok/0,
    test_range3_Bad/0,
    test_swap_Ok/0,
    test_swap_Bad/0,
    test_doop_Ok/0,
    test_small_list0_Ok/0,
    test_small_list0_Bad/0,
    test_small_list1a_Ok/0,
    test_small_list1a_Bad/0,
    test_small_list1b_Ok/0,
    test_small_list1b_Bad/0,
    test_small_list2_Ok/0,
    test_one_or_two1_Ok/0,
    test_one_or_two2_Ok/0,
    test_one_or_two3_Bad/0,
    test_one_or_two4_Bad/0,
    test_one_or_two5_Ok/0
]).

%%% Exported functions first.

test_range0_Ok() ->
    ?ASSERT_EQUAL([], get_range(0)).
test_range0_Bad() ->
    ?CRASH_IF_EQUAL([], get_range(0)).

test_range1_Ok() ->
    ?ASSERT_EQUAL([0], get_range(1)).
test_range1_Bad() ->
    ?CRASH_IF_EQUAL([0], get_range(1)).

test_range2_Ok() ->
    ?ASSERT_EQUAL([0, 1], get_range(2)).
test_range2_Bad() ->
    ?CRASH_IF_EQUAL([0, 1], get_range(2)).

test_range3_Ok() ->
    ?ASSERT_EQUAL([0, 1, 2], get_range(3)).
test_range3_Bad() ->
    ?CRASH_IF_EQUAL([0, 1, 2], get_range(3)).

test_swap_Ok() ->
    ?ASSERT_EQUAL([{1}], doop(swap, {[1]})).
test_swap_Bad() ->
    ?CRASH_IF_EQUAL([{1}], doop(swap, {[1]})).

% Since `doop(nop, ...)` is not modeled, code containing it is not explored and therefore "Ok".
test_doop_Ok() ->
    ?ASSERT_EQUAL(not_explored, doop(nop, {[1]})).

test_small_list0_Ok() ->
    ?ASSERT_EQUAL(true, is_small([])).
test_small_list0_Bad() ->
    ?CRASH_IF_EQUAL(true, is_small([])).

test_small_list1a_Ok() ->
    ?ASSERT_EQUAL(true, is_small([1])).
test_small_list1a_Bad() ->
    ?CRASH_IF_EQUAL(true, is_small([1])).

test_small_list1b_Ok() ->
    ?ASSERT_EQUAL(true, is_small([foo])).
test_small_list1b_Bad() ->
    ?CRASH_IF_EQUAL(true, is_small([foo])).

% A list of len>1 should cause symbolic execution to stop.
test_small_list2_Ok() ->
    ?CRASH_IF_EQUAL(true, is_small([0, 1])).

test_one_or_two1_Ok() ->
    ?CRASH_IF_EQUAL(0, one_or_two()).
test_one_or_two2_Ok() ->
    ?CRASH_IF_EQUAL(3, one_or_two()).
test_one_or_two3_Bad() ->
    ?ASSERT_EQUAL(0, one_or_two()).
test_one_or_two4_Bad() ->
    ?ASSERT_EQUAL(1, one_or_two()).
test_one_or_two5_Ok() ->
    case one_or_two() of
        1 -> ok;
        2 -> ok;
        _ -> ?UNEXPECTED_CRASH
    end.


%%% Internal functions follow.

% Model in .inferconfig overwrides with [0, 1, ..., N], for each N in {0,1,2,3}.
get_range(_) -> bad.

% Model says doop(swap, {[1]}) -> [{1}].
doop(_op, _val) -> bad.

% Model says `is_small([]) -> true; is_small([_]) -> true.` (and nothing about other cases).
is_small(_) -> bad.

% Model says that the returned value is either `1` or `2`
one_or_two() -> bad.
