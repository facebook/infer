% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_case_expr).

-export([
    test_case_simple1_Ok/0,
    test_case_simple2_Ok/0,
    test_case_simple3_Bad/0,
    test_case_tail1_Ok/0,
    test_case_tail2_Ok/0,
    test_case_tail3_Bad/0
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

test_case_simple1_Ok() ->
    case_simple(0).
test_case_simple2_Ok() ->
    case_simple(1).
test_case_simple3_Bad() ->
    case_simple(2).

test_case_tail1_Ok() ->
    tail_with_case([1, 2]).
test_case_tail2_Ok() ->
    tail_with_case([1]).
test_case_tail3_Bad() ->
    tail_with_case([]).
