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
    test_case_tail3_Bad/0,
    fp_test_arg_Ok/0,
    test_arg_Bad/0,
    test_local_var_Ok/0,
    test_local_var_Bad/0,
    test_constant_vs_arg_Ok/0,
    test_constant_vs_arg_Bad/0
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

% Currently FP because equality model for unknown types
crash_if_different(A, B) ->
    % The matching of A against the bound B should be compiled to an equality check.
    case A of
        B -> ok
    end.

fp_test_arg_Ok() ->
    crash_if_different(0, 0).
test_arg_Bad() ->
    crash_if_different(0, 1).

crash_if_not_one(A) ->
    B = 1,
    % The matching of A against the bound B should be compiled to an equality check.
    case A of
        B -> ok
    end.

test_local_var_Ok() ->
    crash_if_not_one(1).

test_local_var_Bad() ->
    crash_if_not_one(2).

crash_if_not_one_constant(A) ->
    % The matching of 1 against the bound A should be compiled to an equality check.
    case 1 of
        A -> ok
    end.

test_constant_vs_arg_Ok() ->
    crash_if_not_one_constant(1).

test_constant_vs_arg_Bad() ->
    crash_if_not_one_constant(2).
