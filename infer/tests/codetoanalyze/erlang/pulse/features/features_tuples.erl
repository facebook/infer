% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_tuples).

-export([
    test_first_Ok/0,
    test_first_Bad/0,
    test_second_Ok/0,
    test_second_Bad/0,
    test_third_Ok/0,
    test_third_Bad/0,
    test_nested_Ok/0,
    test_nested_Bad/0
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

first({X, _, _}) -> X.
second({_, Y, _}) -> Y.
third({_, _, Z}) -> Z.

test_first_Ok() ->
    N = first({1, 2, 3}),
    case N of
        1 -> ok
    end.

test_first_Bad() ->
    N = first({1, 2, 3}),
    case N of
        1 -> warn(1)
    end.

test_second_Ok() ->
    N = second({1, 2, 3}),
    case N of
        2 -> ok
    end.

test_second_Bad() ->
    N = second({1, 2, 3}),
    case N of
        2 -> warn(1)
    end.

test_third_Ok() ->
    N = third({1, 2, 3}),
    case N of
        3 -> ok
    end.

test_third_Bad() ->
    N = third({1, 2, 3}),
    case N of
        3 -> warn(1)
    end.

test_nested_Ok() ->
    N = first(second({1, {2, 3, 4}, 5})),
    case N of
        2 -> ok
    end.

test_nested_Bad() ->
    N = first(second({1, {2, 3, 4}, 5})),
    case N of
        2 -> warn(1)
    end.
