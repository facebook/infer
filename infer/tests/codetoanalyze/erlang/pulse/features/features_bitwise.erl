% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_bitwise).
-include("../../common.hrl").

-export([
    test_band1_Ok/0,
    test_band1_Bad/0,
    test_band2_Ok/0,
    test_band2_Bad/0,
    test_bor1_Ok/0,
    test_bor1_Bad/0,
    test_bor2_Ok/0,
    test_bor2_Bad/0,
    test_bxor1_Ok/0,
    test_bxor1_Bad/0,
    test_bxor2_Ok/0,
    test_bxor2_Bad/0,
    test_bsl_Ok/0,
    test_bsl_Bad/0,
    test_bsr1_Ok/0,
    test_bsr1_Bad/0,
    test_bsr2_Ok/0,
    test_bsr2_Bad/0,
    test_ubnot1_Ok/0,
    test_ubnot1_Bad/0,
    test_ubnot2_Ok/0,
    test_ubnot2_Bad/0
]).

test_band1_Ok() ->
    X = 6,
    Y = 3,
    ?ASSERT_EQUAL(2, X band Y).

test_band1_Bad() ->
    X = 6,
    Y = 3,
    ?CRASH_IF_EQUAL(2, X band Y).

test_band2_Ok() ->
    X = 2347,
    Y = 8465,
    ?ASSERT_EQUAL(257, X band Y).

test_band2_Bad() ->
    X = 2347,
    Y = 8465,
    ?CRASH_IF_EQUAL(257, X band Y).

test_bor1_Ok() ->
    X = 6,
    Y = 3,
    ?ASSERT_EQUAL(7, X bor Y).

test_bor1_Bad() ->
    X = 6,
    Y = 3,
    ?CRASH_IF_EQUAL(7, X bor Y).

test_bor2_Ok() ->
    X = 2347,
    Y = 8465,
    ?ASSERT_EQUAL(10555, X bor Y).

test_bor2_Bad() ->
    X = 2347,
    Y = 8465,
    ?CRASH_IF_EQUAL(10555, X bor Y).

test_bxor1_Ok() ->
    X = 6,
    Y = 3,
    ?ASSERT_EQUAL(5, X bxor Y).

test_bxor1_Bad() ->
    X = 6,
    Y = 3,
    ?CRASH_IF_EQUAL(5, X bxor Y).

test_bxor2_Ok() ->
    X = 2347,
    Y = 8465,
    ?ASSERT_EQUAL(10298, X bxor Y).

test_bxor2_Bad() ->
    X = 2347,
    Y = 8465,
    ?CRASH_IF_EQUAL(10298, X bxor Y).

test_bsl_Ok() ->
    X = 1234,
    Y = 3,
    ?ASSERT_EQUAL(9872, X bsl Y).

test_bsl_Bad() ->
    X = 1234,
    Y = 3,
    ?CRASH_IF_EQUAL(9872, X bsl Y).

test_bsr1_Ok() ->
    X = 9872,
    Y = 3,
    ?ASSERT_EQUAL(1234, X bsr Y).

test_bsr1_Bad() ->
    X = 9872,
    Y = 3,
    ?CRASH_IF_EQUAL(1234, X bsr Y).

test_bsr2_Ok() ->
    X = 127,
    Y = 7,
    ?ASSERT_EQUAL(0, X bsr Y).

test_bsr2_Bad() ->
    X = 127,
    Y = 7,
    ?CRASH_IF_EQUAL(0, X bsr Y).

test_ubnot1_Ok() ->
    X = 1,
    ?ASSERT_EQUAL(-2, bnot X).

test_ubnot1_Bad() ->
    X = 1,
    ?CRASH_IF_EQUAL(-2, bnot X).

test_ubnot2_Ok() ->
    X = 11,
    ?ASSERT_EQUAL(-12, bnot X).

test_ubnot2_Bad() ->
    X = 11,
    ?CRASH_IF_EQUAL(-12, bnot X).
