% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_mc).
-include("../../common.hrl").

-export([
    test_mapcomp_Ok/0,
    test_mapcomp_Bad/0,
    test_mapcomp2_Ok/0,
    fn_test_mapcomp2_Bad/0,
    test_mapgen_empty_Ok/0,
    test_mapgen_empty_Bad/0,
    test_mapgen_nonempty_Ok/0,
    test_mapgen_nonempty_Bad/0,
    fp_test_mapgen_nonempty2_Ok/0,
    fn_test_mapgen_nonempty2_Bad/0,
    test_mapgen_badmap_Bad/0,
    test_mapgen_badmap2_Bad/0,
    test_mapcomp_with_mapgen_Ok/0,
    test_mapcomp_with_mapgen_Bad/0
]).

test_mapcomp_Ok() ->
    M = #{I => I+1 || I <- [1]},
    ?ASSERT_EQUAL(2, maps:get(1, M)).

test_mapcomp_Bad() ->
    M = #{I => I+1 || I <- [1]},
    ?CRASH_IF_EQUAL(2, maps:get(1, M)).

test_mapcomp2_Ok() ->
    M = #{I => I+1 || I <- [1, 2]},
    Vals = [maps:get(1, M), maps:get(2, M)],
    ?ASSERT_EQUAL([2, 3], Vals).

% FN expected due to map recency abstraction
fn_test_mapcomp2_Bad() ->
    M = #{I => I+1 || I <- [1, 2]},
    Vals = [maps:get(1, M), maps:get(2, M)],
    ?CRASH_IF_EQUAL([2, 3], Vals).

test_mapgen_empty_Ok() ->
    M = #{},
    L = [{K, V} || K := V <- M],
    ?ASSERT_EQUAL([], L).

test_mapgen_empty_Bad() ->
    M = #{},
    L = [{K, V} || K := V <- M],
    ?CRASH_IF_EQUAL([], L).

test_mapgen_nonempty_Ok() ->
    M = #{1 => 2},
    L = [{K, V} || K := V <- M],
    ?ASSERT_EQUAL([{1, 2}], L).

test_mapgen_nonempty_Bad() ->
    M = #{1 => 2},
    L = [{K, V} || K := V <- M],
    ?CRASH_IF_EQUAL([{1, 2}], L).

% FP expected due to map recency abstraction
fp_test_mapgen_nonempty2_Ok() ->
    M = #{1 => 2, 3 => 4},
    L = [{K, V} || K := V <- M],
    ?ASSERT_EQUAL([{1, 2}, {3, 4}], L).

% FN expected due to map recency abstraction
fn_test_mapgen_nonempty2_Bad() ->
    M = #{1 => 2, 3 => 4},
    L = [{K, V} || K := V <- M],
    ?CRASH_IF_EQUAL([{1, 2}, {3, 4}], L).

test_mapgen_badmap_Bad() ->
    M = [],
    [{K, V} || K := V <- M].

test_mapgen_badmap2_Bad() ->
    M = 1,
    [{K, V} || K := V <- M].

test_mapcomp_with_mapgen_Ok() ->
    M = #{1 => 2},
    M2 = #{V => K || K := V <- M},
    ?ASSERT_EQUAL(1, maps:get(2, M2)).

test_mapcomp_with_mapgen_Bad() ->
    M = #{1 => 2},
    M2 = #{V => K || K := V <- M},
    ?CRASH_IF_EQUAL(1, maps:get(2, M2)).
