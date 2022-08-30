% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(otp_maps).

-export([
    test_intersect1_Ok/0,
    test_intersect2_Bad/0,
    test_merge1_Ok/0,
    test_merge2_Bad/0,
    test_filter1_Ok/0,
    test_filter2_Bad/0
]).

test_intersect1_Ok() ->
    M = maps:intersect(#{}, #{}),
    case M of
        #{} -> ok
    end.

test_intersect2_Bad() ->
    M = maps:intersect(#{}, #{}),
    case M of
        rabbit -> ok
    end.

test_merge1_Ok() ->
    M = maps:merge(#{}, #{}),
    case M of
        #{} -> ok
    end.

test_merge2_Bad() ->
    M = maps:merge(#{}, #{}),
    case M of
        rabbit -> ok
    end.

test_filter1_Ok() ->
    M = maps:filter(fun(_, _) -> ok end, #{}),
    case M of
        #{} -> ok
    end.

test_filter2_Bad() ->
    M = maps:filter(fun(_, _) -> ok end, #{}),
    case M of
        rabbit -> ok
    end.
