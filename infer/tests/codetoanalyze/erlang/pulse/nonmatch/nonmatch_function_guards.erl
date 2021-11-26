% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_function_guards).

-export([
    test_accepts_positive_Bad/0,
    test_accepts_positive_Ok/0,
    test_accepts_positive2_Bad/0,
    test_accepts_positive2_Ok/0,
    test_accepts_all_basic_Ok/0,
    test_accepts_all_basic2_Ok/0,
    test_accepts_all_tricky_Ok/0,
    test_accepts_all_tricky2_Ok/0,
    test_accepts_all_tricky3_Ok/0,
    test_possible_exception_Ok/0,
    test_possible_exception_Bad/0,
    fn_test_possible_exception2_Bad/0
]).

accepts_positive(X) when X > 0 -> ok.

accepts_positive2(X) when 1 =:= 1, 1 =:= 0; X > 0 -> ok.

accepts_all_basic(X) when X > 0 -> ok;
accepts_all_basic(_) -> ok.

accepts_all_basic2(X) when X > 0; 1 =:= 1 -> ok.

accepts_all_tricky(X) when X > 0; not (X > 0) -> ok.

accepts_all_tricky2(X) when X > 0 -> ok;
accepts_all_tricky2(X) when not (X > 0) -> ok.

accepts_all_tricky3(X) when X > 0; X =< 0 -> ok.

possible_exception(X) when 1 div X =:= 1 -> ok.

test_accepts_positive_Bad() ->
    accepts_positive(0).

test_accepts_positive_Ok() ->
    accepts_positive(1).

test_accepts_positive2_Bad() ->
    accepts_positive2(0).

test_accepts_positive2_Ok() ->
    accepts_positive2(1).

test_accepts_all_basic_Ok() ->
    accepts_all_basic(0),
    accepts_all_basic(1).

test_accepts_all_basic2_Ok() ->
    accepts_all_basic2(0),
    accepts_all_basic2(1).

test_accepts_all_tricky_Ok() ->
    accepts_all_tricky(0),
    accepts_all_tricky(1).

test_accepts_all_tricky2_Ok() ->
    accepts_all_tricky2(0),
    accepts_all_tricky2(1).

test_accepts_all_tricky3_Ok() ->
    accepts_all_tricky3(0),
    accepts_all_tricky3(1).

test_possible_exception_Ok() ->
    possible_exception(1).

test_possible_exception_Bad() ->
    possible_exception(2).

% FN (T95472386)
fn_test_possible_exception2_Bad() ->
    possible_exception(0).
