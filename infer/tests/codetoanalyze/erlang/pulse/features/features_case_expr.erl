% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_case_expr).

-export([
    test_case_Ok/0,
    test_case_Bad/0,
    test_case_in_function_Ok/0,
    test_case_in_function_Bad/0
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test_case_Ok() ->
    Y = 3,
    X =
        case Y of
            3 -> 1;
            _ -> 0
        end,
    case X of
        1 -> ok;
        _ -> warn(1)
    end.

test_case_Bad() ->
    Y = 3,
    X =
        case Y of
            3 -> 1;
            _ -> 0
        end,
    case X of
        1 -> warn(1);
        _ -> ok
    end.

case_in_function(X) ->
    case X of
        1 -> 2;
        _ -> 3
    end.

test_case_in_function_Ok() ->
    Y = case_in_function(1),
    case Y of
        2 -> ok;
        _ -> warn(1)
    end.

test_case_in_function_Bad() ->
    Y = case_in_function(1),
    case Y of
        2 -> warn(1);
        _ -> ok
    end.
