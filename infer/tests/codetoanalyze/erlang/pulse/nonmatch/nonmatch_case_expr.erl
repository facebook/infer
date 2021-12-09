% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_case_expr).

-export([
    case_test_simple1_Ok/0,
    case_test_simple2_Ok/0,
    case_test_simple3_Bad/0,
    case_test_tail1_Ok/0,
    case_test_tail2_Ok/0,
    case_test_tail3_Bad/0
]).

case_simple(X) ->
    case X of
        0 -> zero;
        1 -> one
    end.

tail_with_case(X) ->
    case X of
        [_ | T] -> T
    end.

case_test_simple1_Ok() ->
    case_simple(0).
case_test_simple2_Ok() ->
    case_simple(1).
case_test_simple3_Bad() ->
    case_simple(2).

case_test_tail1_Ok() ->
    tail_with_case([1, 2]).
case_test_tail2_Ok() ->
    tail_with_case([1]).
case_test_tail3_Bad() ->
    tail_with_case([]).
