% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_variables).
-include("../../common.hrl").

-export([
    test_vars1_Ok/0,
    test_vars2_Bad/0,
    test_vars3_Bad/0,
    test_vars4_Ok/0,
    test_vars5_Bad/0
]).

test_vars1_Ok() ->
    X = 2,
    Y = X,
    Z = 3,
    ?ASSERT_EQUAL(2, Y),
    ?ASSERT_EQUAL(3, Z).

test_vars2_Bad() ->
    X = 2,
    Y = X,
    Z = 3,
    ?CRASH_IF_EQUAL(2, Y),
    ?ASSERT_EQUAL(3, Z).

test_vars3_Bad() ->
    X = 2,
    Y = X,
    Z = 3,
    ?ASSERT_EQUAL(2, Y),
    ?CRASH_IF_EQUAL(3, Z).

test_vars4_Ok() ->
    X = 2,
    2 = X.

test_vars5_Bad() ->
    X = 2,
    3 = X.
