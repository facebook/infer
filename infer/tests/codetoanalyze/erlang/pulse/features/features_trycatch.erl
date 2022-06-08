% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_trycatch).
-include("../../common.hrl").

-export([
    test_with_after_Ok/0,
    test_with_after_Bad/0,
    test_with_catch_Ok/0,
    test_with_catch_Bad/0,
    test_with_case_and_after1_Ok/0,
    test_with_case_and_after2_Bad/0,
    test_with_case_and_after3_Bad/0,
    test_with_case_and_catch_Ok/0,
    test_with_case_and_catch_Bad/0,
    test_with_case_and_catch_and_after_Ok/0,
    test_with_case_and_catch_and_after_Bad/0
]).

test_with_catch_Ok() ->
    X = 1,
    Y =
        try
            X
        catch
            _ -> ?EXPECTED_CRASH
        end,
    ?ASSERT_EQUAL(1, Y).

test_with_catch_Bad() ->
    X = 1,
    Y =
        try
            X
        catch
            _ -> ok
        end,
    ?CRASH_IF_EQUAL(1, Y).

test_with_after_Ok() ->
    X = 1,
    Y =
        try
            X
        after
            ok
        end,
    ?ASSERT_EQUAL(1, Y).

test_with_after_Bad() ->
    X = 1,
    _ =
        try
            X
        after
            ?EXPECTED_CRASH
        end.

test_with_case_and_after1_Ok() ->
    X = 1,
    try X of
        1 -> ok;
        _ -> ?EXPECTED_CRASH
    after
        ok
    end.

test_with_case_and_after2_Bad() ->
    X = 1,
    try X of
        1 -> ?EXPECTED_CRASH;
        _ -> ok
    after
        ok
    end.

test_with_case_and_after3_Bad() ->
    X = 1,
    try X of
        1 -> ok;
        _ -> ok
    after
        ?EXPECTED_CRASH
    end.

test_with_case_and_catch_Ok() ->
    X = 1,
    try X of
        1 -> ok;
        _ -> ?EXPECTED_CRASH
    catch
        _ -> ?EXPECTED_CRASH
    end.

test_with_case_and_catch_Bad() ->
    X = 1,
    try X of
        1 -> ?EXPECTED_CRASH;
        _ -> ok
    catch
        _ -> ok
    end.

test_with_case_and_catch_and_after_Ok() ->
    X = 1,
    try X of
        1 -> ok;
        _ -> ?EXPECTED_CRASH
    catch
        _ -> ?EXPECTED_CRASH
    after
        ok
    end.

test_with_case_and_catch_and_after_Bad() ->
    X = 1,
    try X of
        1 -> ok;
        _ -> ok
    catch
        _ -> ok
    after
        ?EXPECTED_CRASH
    end.
