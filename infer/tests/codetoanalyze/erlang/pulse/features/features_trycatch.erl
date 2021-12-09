% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_trycatch).

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

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test_with_catch_Ok() ->
    X = 1,
    Y =
        try
            X
        catch
            _ -> warn(1)
        end,
    case Y of
        1 -> ok;
        _ -> warn(1)
    end.

test_with_catch_Bad() ->
    X = 1,
    Y =
        try
            X
        catch
            _ -> ok
        end,
    case Y of
        1 -> warn(1);
        _ -> ok
    end.

test_with_after_Ok() ->
    X = 1,
    Y =
        try
            X
        after
            ok
        end,
    case Y of
        1 -> ok;
        _ -> warn(1)
    end.

test_with_after_Bad() ->
    X = 1,
    Y =
        try
            X
        after
            warn(1)
        end.

test_with_case_and_after1_Ok() ->
    X = 1,
    try X of
        1 -> ok;
        _ -> warn(1)
    after
        ok
    end.

test_with_case_and_after2_Bad() ->
    X = 1,
    try X of
        1 -> warn(1);
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
        warn(1)
    end.

test_with_case_and_catch_Ok() ->
    X = 1,
    try X of
        1 -> ok;
        _ -> warn(1)
    catch
        _ -> warn(1)
    end.

test_with_case_and_catch_Bad() ->
    X = 1,
    try X of
        1 -> warn(1);
        _ -> ok
    catch
        _ -> ok
    end.

test_with_case_and_catch_and_after_Ok() ->
    X = 1,
    try X of
        1 -> ok;
        _ -> warn(1)
    catch
        _ -> warn(1)
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
        warn(1)
    end.
