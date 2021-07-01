% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch).

-export([list_match_test_a/0, list_match_test_b/0, list_match_test_c/0]).

tail([_ | Xs]) -> Xs.
assert_empty([]) -> ok.
assert_second_is_nil([_, [] | _]) -> ok.

list_match_test_a() ->
    tail([1, 2]),
    tail([1]),
    tail([]).

list_match_test_b() ->
    assert_empty([]),
    assert_empty([1]),
    assert_empty([1, 2]).

list_match_test_c() ->
    % FP (T94492137)
    assert_second_is_nil([1, [], 2]),
    assert_second_is_nil([1, []]),
    assert_second_is_nil([1, [2], 3]).
