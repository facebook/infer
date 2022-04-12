% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_lists).

-export([
    test_tail1_Ok/0,
    test_tail2_Ok/0,
    test_tail3_Bad/0,
    test_empty1_Ok/0,
    test_empty2_Bad/0,
    test_empty3_Bad/0,
    test_secondnil1_Ok/0,
    test_secondnil2_Ok/0,
    test_secondnil3_Bad/0,
    test_primitive_Bad/0
]).

tail([_ | Xs]) -> Xs.
assert_empty([]) -> ok.
assert_second_is_nil([_, [] | _]) -> ok.

test_tail1_Ok() ->
    tail([1, 2]).
test_tail2_Ok() ->
    tail([1]).
test_tail3_Bad() ->
    tail([]).

test_empty1_Ok() ->
    assert_empty([]).
test_empty2_Bad() ->
    assert_empty([1]).
test_empty3_Bad() ->
    assert_empty([1, 2]).

test_secondnil1_Ok() ->
    assert_second_is_nil([1, [], 2]).
test_secondnil2_Ok() ->
    assert_second_is_nil([1, []]).
test_secondnil3_Bad() ->
    assert_second_is_nil([1, [2], 3]).

test_primitive_Bad() ->
    % This fails, should report
    [] = 2,
    % Cannot reach this, shouldn't report
    assert_empty([1]).
