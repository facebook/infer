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
    test_cons2_Bad/0
]).

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
