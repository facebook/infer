% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_case_expr).
-include("../../common.hrl").

-export([
    test_case_Ok/0,
    test_case_Bad/0,
    test_case_in_function_Ok/0,
    test_case_in_function_Bad/0
]).

test_case_Ok() ->
    Y = 3,
    X =
        case Y of
            3 -> 1;
            _ -> 0
        end,
    ?ASSERT_EQUAL(1, X).

test_case_Bad() ->
    Y = 3,
    X =
        case Y of
            3 -> 1;
            _ -> 0
        end,
    ?CRASH_IF_EQUAL(1, X).

case_in_function(X) ->
    case X of
        1 -> 2;
        _ -> 3
    end.

test_case_in_function_Ok() ->
    Y = case_in_function(1),
    ?ASSERT_EQUAL(2, Y).

test_case_in_function_Bad() ->
    Y = case_in_function(1),
    ?CRASH_IF_EQUAL(2, Y).
