% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(otp_lists).

-export([
    test_foreach_Ok/0,
    test_foreach_Bad/0,
    test_sort1_Ok/0,
    test_sort2_Bad/0,
    test_sort3_Bad/0,
    test_sort4_Bad/0
]).

test_foreach_Ok() ->
    ok = lists:foreach(fun(_) -> ok end, []).

test_foreach_Bad() ->
    i_will_not_match = lists:foreach(fun(_) -> ok end, []).

test_sort1_Ok() ->
    X = lists:sort([]),
    case X of
        [] -> nil;
        [_ | _] -> cons
    end.

test_sort2_Bad() ->
    X = lists:sort([]),
    case X of
        [_ | _] -> cons
    end.

test_sort3_Bad() ->
    X = lists:sort([1]),
    case X of
        [] -> nil
    end.

test_sort4_Bad() ->
    X = lists:sort([]),
    case X of
        rabbit -> rabbit
    end.
