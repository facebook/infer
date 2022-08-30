% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_block).
-include("../../common.hrl").

-export([
    test_block_Ok/0,
    test_block_Bad/0,
    test_block_in_function_Ok/0,
    test_block_in_function_Bad/0
]).

test_block_Ok() ->
    ?ASSERT_EQUAL(3, begin
        X = 3,
        Y = X,
        Z = Y,
        Z
    end).

test_block_Bad() ->
    ?CRASH_IF_EQUAL(3, begin
        X = 3,
        Y = X,
        Z = Y,
        Z
    end).

block_in_function() ->
    begin
        1,
        2,
        3
    end.

test_block_in_function_Ok() ->
    X = block_in_function(),
    ?ASSERT_EQUAL(3, X).

test_block_in_function_Bad() ->
    X = block_in_function(),
    ?CRASH_IF_EQUAL(3, X).
