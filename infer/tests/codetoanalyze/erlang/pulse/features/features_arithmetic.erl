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
    fn_test_idiv_by_zero_Bad/0,
    fn_test_idiv_zero_by_zero_Bad/0,
    test_rem_Ok/0,
    test_rem_Bad/0,
    fn_test_rem_zero_Bad/0,
    fn_test_rem_zero_by_zero_Bad/0,
    test_rem_neg1_Ok/0,
    test_rem_neg1_Bad/0,
    test_rem_neg2_Ok/0,
    test_rem_neg2_Bad/0,
    test_rem_neg3_Ok/0,
    test_rem_neg3_Bad/0,
    test_multiple_Ok/0,
    test_multiple_Bad/0,
    test_uminus1_Ok/0,
    test_uminus1_Bad/0,
    test_uminus2_Ok/0,
    test_uminus2_Bad/0,
    test_big_Ok/0,
    test_big_Bad/0
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

%TODO: T95472386
fn_test_idiv_by_zero_Bad() ->
    X = 22,
    Y = 0,
    X div Y.

%TODO: T95472386
fn_test_idiv_zero_by_zero_Bad() ->
    X = 0,
    Y = 0,
    X div Y.

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

test_rem_neg1_Ok() ->
    X = 5,
    Y = -2,
    case X rem Y of
        1 -> ok;
        _ -> warn(1)
    end.
test_rem_neg1_Bad() ->
    X = 5,
    Y = -2,
    case X rem Y of
        1 -> warn(1);
        _ -> ok
    end.

test_rem_neg2_Ok() ->
    X = -5,
    Y = 2,
    case X rem Y of
        -1 -> ok;
        _ -> warn(1)
    end.
test_rem_neg2_Bad() ->
    X = -5,
    Y = 2,
    case X rem Y of
        -1 -> warn(1);
        _ -> ok
    end.

test_rem_neg3_Ok() ->
    X = -5,
    Y = -2,
    case X rem Y of
        -1 -> ok;
        _ -> warn(1)
    end.
test_rem_neg3_Bad() ->
    X = -5,
    Y = -2,
    case X rem Y of
        -1 -> warn(1);
        _ -> ok
    end.

%TODO: T95472386
fn_test_rem_zero_Bad() ->
    X = 42,
    Y = 0,
    X rem Y.

%TODO: T95472386
fn_test_rem_zero_by_zero_Bad() ->
    X = 0,
    Y = 0,
    X rem Y.

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

test_big_Ok() ->
    Pow_2_10 = 1024,
    Pow_2_20 = Pow_2_10 * Pow_2_10,
    Pow_2_40 = Pow_2_20 * Pow_2_20,
    Pow_2_80 = Pow_2_40 * Pow_2_40,
    Pow_2_100 = Pow_2_80 * Pow_2_20,
    % We check that we can go back to 1 by dividing successively by
    % "small" (less than 2^32) numbers. This would check that no
    % overflow happen neither in the computation of 2^100 nor in the
    % direct parsing of that same number (otherwise the successive
    % divisions could yield 0 or a negative number)
    One =
        Pow_2_100 div
            Pow_2_20 div
            Pow_2_20 div
            Pow_2_20 div
            Pow_2_20 div
            Pow_2_20,
    case {Pow_2_100, One} of
        {1267650600228229401496703205376, 1} -> ok;
        _ -> warn(1)
    end.

test_big_Bad() ->
    Pow_2_10 = 1024,
    Pow_2_20 = Pow_2_10 * Pow_2_10,
    Pow_2_40 = Pow_2_20 * Pow_2_20,
    Pow_2_80 = Pow_2_40 * Pow_2_40,
    Pow_2_100 = Pow_2_80 * Pow_2_20,
    % cf test_big_Ok
    One =
        Pow_2_100 div
            Pow_2_20 div
            Pow_2_20 div
            Pow_2_20 div
            Pow_2_20 div
            Pow_2_20,
    case {Pow_2_100, One} of
        {1267650600228229401496703205376, 1} -> warn(1);
        _ -> ok
    end.
