% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_bitwise).

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

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test_band1_Ok() ->
    X = 6,
    Y = 3,
    case X band Y of
        2 -> ok
    end.

test_band1_Bad() ->
    X = 6,
    Y = 3,
    case X band Y of
        2 -> warn(1)
    end.

test_band2_Ok() ->
    X = 2347,
    Y = 8465,
    case X band Y of
        257 -> ok
    end.

test_band2_Bad() ->
    X = 2347,
    Y = 8465,
    case X band Y of
        257 -> warn(1)
    end.

test_bor1_Ok() ->
    X = 6,
    Y = 3,
    case X bor Y of
        7 -> ok
    end.

test_bor1_Bad() ->
    X = 6,
    Y = 3,
    case X bor Y of
        7 -> warn(1)
    end.

test_bor2_Ok() ->
    X = 2347,
    Y = 8465,
    case X bor Y of
        10555 -> ok
    end.

test_bor2_Bad() ->
    X = 2347,
    Y = 8465,
    case X bor Y of
        10555 -> warn(1)
    end.

test_bxor1_Ok() ->
    X = 6,
    Y = 3,
    case X bxor Y of
        5 -> ok
    end.

test_bxor1_Bad() ->
    X = 6,
    Y = 3,
    case X bxor Y of
        5 -> warn(1)
    end.

test_bxor2_Ok() ->
    X = 2347,
    Y = 8465,
    case X bxor Y of
        10298 -> ok
    end.

test_bxor2_Bad() ->
    X = 2347,
    Y = 8465,
    case X bxor Y of
        10298 -> warn(1)
    end.

test_bsl_Ok() ->
    X = 1234,
    Y = 3,
    case X bsl Y of
        9872 -> ok
    end.

test_bsl_Bad() ->
    X = 1234,
    Y = 3,
    case X bsl Y of
        9872 -> warn(1)
    end.

test_bsr1_Ok() ->
    X = 9872,
    Y = 3,
    case X bsr Y of
        1234 -> ok
    end.

test_bsr1_Bad() ->
    X = 9872,
    Y = 3,
    case X bsr Y of
        1234 -> warn(1)
    end.

test_bsr2_Ok() ->
    X = 127,
    Y = 7,
    case X bsr Y of
        0 -> ok
    end.

test_bsr2_Bad() ->
    X = 127,
    Y = 7,
    case X bsr Y of
        0 -> warn(1)
    end.

test_ubnot1_Ok() ->
    X = 1,
    case bnot X of
        -2 -> ok
    end.

test_ubnot1_Bad() ->
    X = 1,
    case bnot X of
        -2 -> warn(1)
    end.

test_ubnot2_Ok() ->
    X = 11,
    case bnot X of
        -12 -> ok
    end.

test_ubnot2_Bad() ->
    X = 11,
    case bnot X of
        -12 -> warn(1)
    end.
