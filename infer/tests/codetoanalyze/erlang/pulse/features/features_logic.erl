% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_logic).

-export([
    test_false2_Bad/0,
    test_true2_Ok/0,
    test_true_Ok/0,
    test_true_Bad/0,
    test_false_Ok/0,
    test_false_Bad/0,
    test_and00_Bad/0,
    test_and01_Bad/0,
    test_and10_Bad/0,
    test_and11_Ok/0,
    test_andalso00_Bad/0,
    test_andalso01_Bad/0,
    test_andalso10_Bad/0,
    test_andalso11_Ok/0,
    test_or00_Bad/0,
    test_or01_Ok/0,
    test_or10_Ok/0,
    test_or11_Ok/0,
    test_orelse00_Bad/0,
    test_orelse01_Ok/0,
    test_orelse10_Ok/0,
    test_orelse11_Ok/0,
    test_unot_Ok/0,
    test_unot_Bad/0,
    test_xor00_Bad/0,
    test_xor01_Ok/0,
    test_xor10_Ok/0,
    test_xor11_Bad/0
]).

test_true_Ok() ->
    case 1 == 1 of
        true -> ok
    end.

test_true_Bad() ->
    case 1 == 1 of
        false -> ok
    end.

test_true2_Ok() ->
    if
        true -> ok
    end.

test_false_Ok() ->
    case 1 == 0 of
        false -> ok
    end.

test_false_Bad() ->
    case 1 == 0 of
        true -> ok
    end.

test_false2_Bad() ->
    if
        false -> ok
    end.

test_and00_Bad() ->
    if
        false and false -> ok
    end.

test_and01_Bad() ->
    if
        false and true -> ok
    end.

test_and10_Bad() ->
    if
        true and false -> ok
    end.

test_and11_Ok() ->
    if
        true and true -> ok
    end.

test_andalso00_Bad() ->
    if
        false andalso false -> ok
    end.

test_andalso01_Bad() ->
    if
        false andalso true -> ok
    end.

test_andalso10_Bad() ->
    if
        true andalso false -> ok
    end.

test_andalso11_Ok() ->
    if
        true andalso true -> ok
    end.

test_or00_Bad() ->
    if
        false or false -> ok
    end.

test_or01_Ok() ->
    if
        false or true -> ok
    end.

test_or10_Ok() ->
    if
        true or false -> ok
    end.

test_or11_Ok() ->
    if
        true or true -> ok
    end.

test_orelse00_Bad() ->
    if
        false orelse false -> ok
    end.

test_orelse01_Ok() ->
    if
        false orelse true -> ok
    end.

test_orelse10_Ok() ->
    if
        true orelse false -> ok
    end.

test_orelse11_Ok() ->
    if
        true orelse true -> ok
    end.

test_unot_Ok() ->
    if
        not false -> ok
    end.

test_unot_Bad() ->
    if
        not true -> ok
    end.

test_xor00_Bad() ->
    if
        false xor false -> ok
    end.

test_xor01_Ok() ->
    if
        false xor true -> ok
    end.

test_xor10_Ok() ->
    if
        true xor false -> ok
    end.

test_xor11_Bad() ->
    if
        true xor true -> ok
    end.
