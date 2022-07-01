% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_functions).
-include("../../common.hrl").

-export([
    test_override_Bad/0,
    test_override_Ok/0,
    test_call1_Ok/0,
    test_call1_Bad/0,
    test_call2_Ok/0,
    test_call2_Bad/0,
    test_call3_Ok/0,
    test_call3_Bad/0,
    test_call4_Ok/0,
    test_call4_Bad/0
]).

f(1) -> 1;
f(_) -> 0.

first(X, _) -> X.

second(_, Y) -> Y.

test_call1_Ok() ->
    ?ASSERT_EQUAL(0, f(5)).

test_call1_Bad() ->
    ?CRASH_IF_EQUAL(0, f(5)).

test_call2_Ok() ->
    ?ASSERT_EQUAL(1, f(1)).

test_call2_Bad() ->
    ?CRASH_IF_EQUAL(1, f(1)).

test_call3_Ok() ->
    ?ASSERT_EQUAL(1, first(1, 2)).

test_call3_Bad() ->
    ?CRASH_IF_EQUAL(1, first(1, 2)).

test_call4_Ok() ->
    ?ASSERT_EQUAL(2, second(1, 2)).

test_call4_Bad() ->
    ?CRASH_IF_EQUAL(2, second(1, 2)).

% We check that this overrides erlang:is_map
is_map(_) -> true.

test_override_Ok() ->
    ?ASSERT_EQUAL(true, is_map(1)).

test_override_Bad() ->
    ?CRASH_IF_EQUAL(true, is_map(1)).
