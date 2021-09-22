% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(lists).

-export([
    test_nil_Ok/0,
    test_nil_Bad/0,
    test_cons1_Ok/0,
    test_cons1_Bad/0,
    test_cons2_Ok/0,
    test_cons2_Bad/0,
    test_reverse0_Ok/0,
    test_reverse0_Bad/0,
    test_reverse1_Ok/0,
    test_reverse1_Bad/0,
    test_reverse2_Ok/0,
    test_reverse2_Bad/0,
    test_reverse3_Ok/0,
    test_reverse3_Bad/0,
    test_reverse4_Ok/0,
    fn_test_reverse4_Bad/0
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test_nil_Ok() ->
    Nil = [],
    case Nil of
        [] -> ok
    end.

test_nil_Bad() ->
    NotNil = [1],
    case NotNil of
        [] -> ok
    end.

test_cons1_Ok() ->
    Nil = [],
    L2 = [2 | Nil],
    L1 = [1 | L2],
    case L1 of
        [1, 2] -> ok
    end.

test_cons1_Bad() ->
    Nil = [],
    L2 = [3 | Nil],
    L1 = [1 | L2],
    case L1 of
        [1, 2] -> ok
    end.

test_cons2_Ok() ->
    L = [1, 2, 3],
    case L of
        [1 | L2] ->
            case L2 of
                [2 | T] ->
                    case T of
                        [3] -> ok
                    end
            end
    end.

test_cons2_Bad() ->
    L = [1, 100000, 3],
    case L of
        [1 | L2] ->
            case L2 of
                [2 | T] ->
                    case T of
                        [3] -> ok
                    end
            end
    end.

test_reverse0_Ok() ->
    L = lists:reverse([]),
    case L of
        [] -> ok;
        _ -> warn(1)
    end.

test_reverse0_Bad() ->
    L = lists:reverse([]),
    case L of
        [] -> warn(1);
        _ -> ok
    end.

test_reverse1_Ok() ->
    L = lists:reverse([1234]),
    case L of
        [1234] -> ok;
        _ -> warn(1)
    end.

test_reverse1_Bad() ->
    L = lists:reverse([1234]),
    case L of
        [1234] -> warn(1);
        _ -> ok
    end.

test_reverse2_Ok() ->
    L = lists:reverse([1234, 567]),
    case L of
        [567, 1234] -> ok;
        _ -> warn(1)
    end.

test_reverse2_Bad() ->
    L = lists:reverse([1234, 567]),
    case L of
        [567, 1234] -> warn(1);
        _ -> ok
    end.

test_reverse3_Ok() ->
    L = lists:reverse([1234, 567, 89]),
    case L of
        [89, 567, 1234] -> ok;
        _ -> warn(1)
    end.

test_reverse3_Bad() ->
    L = lists:reverse([1234, 567, 89]),
    case L of
        [89, 567, 1234] -> warn(1);
        _ -> ok
    end.

test_reverse4_Ok() ->
    L = lists:reverse([1234, 567, 89, 0]),
    case L of
        [0, 89, 567, 1234] -> ok;
        _ -> warn(1)
    end.

% Known false negative
fn_test_reverse4_Bad() ->
    L = lists:reverse([1234, 567, 89, 0]),
    case L of
        [0, 89, 567, 1234] -> warn(1);
        _ -> ok
    end.
