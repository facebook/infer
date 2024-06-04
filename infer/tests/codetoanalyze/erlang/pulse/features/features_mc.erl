% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_mc).
-include("../../common.hrl").

-export([
    test_mapcomp_Ok/0,
    test_mapcomp_Bad/0,
    test_mapgen_empty_Ok/0,
    test_mapgen_empty_Bad/0,
    test_mapgen_nonempty_Ok/0,
    test_mapgen_nonempty_Bad/0,
    test_mapgen_badmap_Bad/0
]).

test_mapcomp_Ok() ->
    M = #{I => I+1 || I <- [1]},
    ?ASSERT_EQUAL(2, maps:get(1, M)).

test_mapcomp_Bad() ->
    M = #{I => I+1 || I <- [1]},
    ?CRASH_IF_EQUAL(2, maps:get(1, M)).

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

test_mapgen_badmap_Bad() ->
    M = [],
    [{K, V} || K := V <- M].
