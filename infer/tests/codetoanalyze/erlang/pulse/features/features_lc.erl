% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_lc).
-include("../../common.hrl").

-export([
    test_empty_Ok/0,
    test_empty_Bad/0,
    test_simple1_Ok/0,
    test_simple1a_Bad/0,
    test_simple1_Bad/0,
    test_simple2_Ok/0,
    test_simple2_Bad/0,
    test_simple3_Ok/0,
    test_simple3_Bad/0,
    test_simple4_Ok/0,
    fn_test_simple4_Bad/0,
    test_filtered1_Ok/0,
    test_filtered1_Bad/0,
    test_filtered2_Ok/0,
    test_filtered2_Bad/0,
    test_two_filters_Ok/0,
    test_two_filters_Bad/0,
    test_two_gen1_Ok/0,
    test_two_gen1_Bad/0,
    test_two_gen2_Ok/0,
    test_two_gen2_Bad/0,
    test_bad_gen_Bad/0,
    test_gen_after_filter_Ok/0,
    test_gen_after_filter_Bad/0,
    test_scoping1_Ok/0,
    test_scoping2_Ok/0
]).

test_empty_Ok() ->
    L = [X || X <- []],
    ?ASSERT_EQUAL([], L).

test_empty_Bad() ->
    L = [X || X <- []],
    ?CRASH_IF_EQUAL([], L).

test_simple1_Ok() ->
    L = [X + 1 || X <- [2]],
    ?ASSERT_EQUAL([3], L).

test_simple1a_Bad() ->
    L = [X || X <- [3]],
    case L of
        [4] -> ok
    end.

test_simple1_Bad() ->
    L = [X + 1 || X <- [2]],
    ?CRASH_IF_EQUAL([3], L).

test_simple2_Ok() ->
    L = [X + 1 || X <- [1, 2]],
    ?ASSERT_EQUAL([2, 3], L).

test_simple2_Bad() ->
    L = [X + 1 || X <- [1, 2]],
    ?CRASH_IF_EQUAL([2, 3], L).

test_simple3_Ok() ->
    L = [X + 1 || X <- [1, 2, 3]],
    ?ASSERT_EQUAL([2, 3, 4], L).

test_simple3_Bad() ->
    L = [X + 1 || X <- [1, 2, 3]],
    ?CRASH_IF_EQUAL([2, 3, 4], L).

test_simple4_Ok() ->
    L = [X + 1 || X <- [1, 2, 3, 4]],
    ?ASSERT_EQUAL([2, 3, 4, 5], L).

% Known FN due to loop unrolling limit
fn_test_simple4_Bad() ->
    L = [X + 1 || X <- [1, 2, 3, 4]],
    ?CRASH_IF_EQUAL([2, 3, 4, 5], L).

test_filtered1_Ok() ->
    L = [X + 1 || X <- [1, 2], X > 1],
    ?ASSERT_EQUAL([3], L).

test_filtered1_Bad() ->
    L = [X + 1 || X <- [1, 2], X > 1],
    ?CRASH_IF_EQUAL([3], L).

test_filtered2_Ok() ->
    L = [X + 1 || X <- [1, 2], X > 5],
    ?ASSERT_EQUAL([], L).

test_filtered2_Bad() ->
    L = [X + 1 || X <- [1, 2], X > 5],
    ?CRASH_IF_EQUAL([], L).

test_two_filters_Ok() ->
    L = [X + 1 || X <- [1, 2, 3], X > 1, X + 1 < 4],
    ?ASSERT_EQUAL([3], L).

test_two_filters_Bad() ->
    L = [X + 1 || X <- [1, 2, 3], X > 1, X + 1 < 4],
    ?CRASH_IF_EQUAL([3], L).

test_two_gen1_Ok() ->
    L = [X + Y || X <- [2, 3], Y <- [4]],
    ?ASSERT_EQUAL([6, 7], L).

test_two_gen1_Bad() ->
    L = [X + Y || X <- [2, 3], Y <- [4]],
    ?CRASH_IF_EQUAL([6, 7], L).

test_two_gen2_Ok() ->
    L = [Y || X <- [2, 3], Y <- [X]],
    ?ASSERT_EQUAL([2, 3], L).

test_two_gen2_Bad() ->
    L = [Y || X <- [2, 3], Y <- [X]],
    ?CRASH_IF_EQUAL([2, 3], L).

test_bad_gen_Bad() ->
    [X || X <- #{}].


accepts_one(1) -> [ok].

test_gen_after_filter_Ok() ->
    L = [X || X <- [1,2], X < 2, _Y <- accepts_one(X)],
    ?ASSERT_EQUAL([1], L).

test_gen_after_filter_Bad() ->
    L = [X || X <- [1,2], X < 2, _Y <- accepts_one(X)],
    ?CRASH_IF_EQUAL([1], L).

test_scoping1_Ok() ->
    _ = [X || X <- [1]],
    % This X is different from the one in the list comprehension
    X = 2,
    _ = X.

test_scoping2_Ok() ->
    % Both Xs are different ones
    _ = [X || X <- [1]],
    _ = [X || X <- [2]].
