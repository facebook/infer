% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_arithmetic).

-export([
    test_add1_Ok/0,
    test_add1_Bad/0,
    test_add2_Ok/0,
    test_add2_Bad/0,
    test_sub1_Ok/0,
    test_sub1_Bad/0,
    test_sub2_Ok/0,
    test_sub2_Bad/0,
    test_mul1_Ok/0,
    test_mul1_Bad/0,
    test_mul2_Ok/0,
    test_mul2_Bad/0,
    test_mul3_Ok/0,
    test_mul3_Bad/0,
    test_idiv1_Ok/0,
    test_idiv1_Bad/0,
    test_idiv2_Ok/0,
    test_idiv2_Bad/0,
    test_rem_Ok/0,
    test_rem_Bad/0,
    test_multiple_Ok/0,
    test_multiple_Bad/0,
    test_uminus1_Ok/0,
    test_uminus1_Bad/0,
    test_uminus2_Ok/0,
    test_uminus2_Bad/0
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test_add1_Ok() ->
    X = 2,
    Y = 3,
    case X + Y of
        5 -> ok;
        _ -> warn(1)
    end.
test_add1_Bad() ->
    X = 2,
    Y = 3,
    case X + Y of
        5 -> warn(1);
        _ -> ok
    end.

test_add2_Ok() ->
    X = 2,
    Y = -3,
    case X + Y of
        -1 -> ok;
        _ -> warn(1)
    end.
test_add2_Bad() ->
    X = 2,
    Y = -3,
    case X + Y of
        -1 -> warn(1);
        _ -> ok
    end.

test_sub1_Ok() ->
    X = 5,
    Y = 3,
    case X - Y of
        2 -> ok;
        _ -> warn(1)
    end.
test_sub1_Bad() ->
    X = 5,
    Y = 3,
    case X - Y of
        2 -> warn(1);
        _ -> ok
    end.

test_sub2_Ok() ->
    X = 3,
    Y = 5,
    case X - Y of
        -2 -> ok;
        _ -> warn(1)
    end.
test_sub2_Bad() ->
    X = 3,
    Y = 5,
    case X - Y of
        -2 -> warn(1);
        _ -> ok
    end.

test_mul1_Ok() ->
    X = 5,
    Y = 3,
    case X * Y of
        15 -> ok;
        _ -> warn(1)
    end.
test_mul1_Bad() ->
    X = 5,
    Y = 3,
    case X * Y of
        15 -> warn(1);
        _ -> ok
    end.

test_mul2_Ok() ->
    X = -5,
    Y = 3,
    case X * Y of
        -15 -> ok;
        _ -> warn(1)
    end.
test_mul2_Bad() ->
    X = -5,
    Y = 3,
    case X * Y of
        -15 -> warn(1);
        _ -> ok
    end.

test_mul3_Ok() ->
    X = -5,
    Y = -3,
    case X * Y of
        15 -> ok;
        _ -> warn(1)
    end.
test_mul3_Bad() ->
    X = -5,
    Y = -3,
    case X * Y of
        15 -> warn(1);
        _ -> ok
    end.

test_idiv1_Ok() ->
    X = 21,
    Y = 3,
    case X div Y of
        7 -> ok;
        _ -> warn(1)
    end.
test_idiv1_Bad() ->
    X = 21,
    Y = 3,
    case X div Y of
        7 -> warn(1);
        _ -> ok
    end.

test_idiv2_Ok() ->
    X = 22,
    Y = 3,
    case X div Y of
        7 -> ok;
        _ -> warn(1)
    end.

test_idiv2_Bad() ->
    X = 22,
    Y = 3,
    case X div Y of
        7 -> warn(1);
        _ -> ok
    end.

test_rem_Ok() ->
    X = 5,
    Y = 3,
    case X rem Y of
        2 -> ok;
        _ -> warn(1)
    end.
test_rem_Bad() ->
    X = 5,
    Y = 3,
    case X rem Y of
        2 -> warn(1);
        _ -> ok
    end.

test_multiple_Ok() ->
    case (8 + 4) div 2 * 5 of
        30 -> ok;
        _ -> warn(1)
    end.
test_multiple_Bad() ->
    case (8 + 4) div 2 * 5 of
        30 -> warn(1);
        _ -> ok
    end.

test_uminus1_Ok() ->
    X = -3,
    case -X of
        3 -> ok;
        _ -> warn(1)
    end.
test_uminus1_Bad() ->
    X = -3,
    case -X of
        3 -> warn(1);
        _ -> ok
    end.

test_uminus2_Ok() ->
    X = 5,
    case -X of
        -5 -> ok;
        _ -> warn(1)
    end.
test_uminus2_Bad() ->
    X = 5,
    case -X of
        -5 -> warn(1);
        _ -> ok
    end.
