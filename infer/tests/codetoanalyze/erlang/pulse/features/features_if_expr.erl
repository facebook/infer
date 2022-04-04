% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_if_expr).

-export([
    test_if_Ok/0,
    test_if_Bad/0,
    test_if_in_function_Ok/0,
    test_if_in_function_Bad/0,
    test_if_in_function2_Ok/0,
    test_if_in_function2_Bad/0
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test_if_Ok() ->
    Y = 3,
    X =
        if
            Y == 3 -> 1;
            not (Y == 3) -> 0
        end,
    case X of
        1 -> ok;
        _ -> warn(1)
    end.

test_if_Bad() ->
    Y = 3,
    X =
        if
            Y == 3 -> 1;
            not (Y == 3) -> 0
        end,
    case X of
        1 -> warn(1);
        _ -> ok
    end.

if_in_function(X) ->
    if
        X -> 2;
        not X -> 3
    end.

test_if_in_function_Ok() ->
    Y = if_in_function(true),
    case Y of
        2 -> ok;
        _ -> warn(1)
    end.

test_if_in_function_Bad() ->
    Y = if_in_function(true),
    case Y of
        2 -> warn(1);
        _ -> ok
    end.

test_if_in_function2_Ok() ->
    Y = if_in_function(false),
    case Y of
        3 -> ok;
        _ -> warn(1)
    end.

test_if_in_function2_Bad() ->
    Y = if_in_function(false),
    case Y of
        3 -> warn(1);
        _ -> ok
    end.
