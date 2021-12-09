% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_functions).

-export([
    list_match_test_tail1_Ok/0,
    list_match_test_tail2_Ok/0,
    list_match_test_tail3_Bad/0,
    list_match_test_empty1_Ok/0,
    list_match_test_empty2_Bad/0,
    list_match_test_empty3_Bad/0,
    list_match_test_secondnil1_Ok/0,
    list_match_test_secondnil2_Ok/0,
    list_match_test_secondnil3_Bad/0
]).

tail([_ | Xs]) -> Xs.
assert_empty([]) -> ok.
assert_second_is_nil([_, [] | _]) -> ok.

list_match_test_tail1_Ok() ->
    tail([1, 2]).
list_match_test_tail2_Ok() ->
    tail([1]).
list_match_test_tail3_Bad() ->
    tail([]).

list_match_test_empty1_Ok() ->
    assert_empty([]).
list_match_test_empty2_Bad() ->
    assert_empty([1]).
list_match_test_empty3_Bad() ->
    assert_empty([1, 2]).

list_match_test_secondnil1_Ok() ->
    assert_second_is_nil([1, [], 2]).
list_match_test_secondnil2_Ok() ->
    assert_second_is_nil([1, []]).
list_match_test_secondnil3_Bad() ->
    assert_second_is_nil([1, [2], 3]).
