% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_if_expr).
-include("../../common.hrl").

-export([
    test_if_Ok/0,
    test_if_Bad/0,
    test_if_in_function_Ok/0,
    test_if_in_function_Bad/0,
    test_if_in_function2_Ok/0,
    test_if_in_function2_Bad/0
]).

test_if_Ok() ->
    Y = 3,
    X =
        if
            Y == 3 -> 1;
            not (Y == 3) -> 0
        end,
    ?ASSERT_EQUAL(1, X).

test_if_Bad() ->
    Y = 3,
    X =
        if
            Y == 3 -> 1;
            not (Y == 3) -> 0
        end,
    ?CRASH_IF_EQUAL(1, X).

if_in_function(X) ->
    if
        X -> 2;
        not X -> 3
    end.

test_if_in_function_Ok() ->
    ?ASSERT_EQUAL(2, if_in_function(true)).

test_if_in_function_Bad() ->
    ?CRASH_IF_EQUAL(2, if_in_function(true)).

test_if_in_function2_Ok() ->
    ?ASSERT_EQUAL(3, if_in_function(false)).

test_if_in_function2_Bad() ->
    ?CRASH_IF_EQUAL(3, if_in_function(false)).
