% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_tuples).
-include("../../common.hrl").

-export([
    test_first_Ok/0,
    test_first_Bad/0,
    test_second_Ok/0,
    test_second_Bad/0,
    test_third_Ok/0,
    test_third_Bad/0,
    test_nested_Ok/0,
    test_nested_Bad/0
]).

first({X, _, _}) -> X.
second({_, Y, _}) -> Y.
third({_, _, Z}) -> Z.

test_first_Ok() ->
    N = first({1, 2, 3}),
    ?ASSERT_EQUAL(1, N).

test_first_Bad() ->
    N = first({1, 2, 3}),
    ?CRASH_IF_EQUAL(1, N).

test_second_Ok() ->
    N = second({1, 2, 3}),
    ?ASSERT_EQUAL(2, N).

test_second_Bad() ->
    N = second({1, 2, 3}),
    ?CRASH_IF_EQUAL(2, N).

test_third_Ok() ->
    N = third({1, 2, 3}),
    ?ASSERT_EQUAL(3, N).

test_third_Bad() ->
    N = third({1, 2, 3}),
    ?CRASH_IF_EQUAL(3, N).

test_nested_Ok() ->
    N = first(second({1, {2, 3, 4}, 5})),
    ?ASSERT_EQUAL(2, N).

test_nested_Bad() ->
    N = first(second({1, {2, 3, 4}, 5})),
    ?CRASH_IF_EQUAL(2, N).
