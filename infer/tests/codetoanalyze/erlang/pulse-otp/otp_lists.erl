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
    test_sort4_Bad/0,
    fp_test_any1_Ok/0,
    test_any2_Bad/0,
    test_any3_Bad/0,
    test_partition1_Ok/0,
    test_partition2_Bad/0,
    test_partition3_Ok/0,
    test_partition4_Bad/0,
    test_partition5_Bad/0,
    test_sub1_Ok/0,
    test_sub2_Ok/0,
    test_sub3_Bad/0,
    test_sub4_Bad/0,
    test_sub5_Bad/0,
    test_sub6_Bad/0,
    test_list_append_with_atom_to_list_Ok/0,
    test_list_append_with_atom_to_binary_Bad/0
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

% T115354480
fp_test_any1_Ok() ->
    X = lists:any(fun (_) -> true end, []),
    case X of
        true -> ok;
        false -> ok
    end.

test_any2_Bad() ->
    X = lists:any(fun (_) -> true end, []),
    case X of
        true -> ok
    end.

test_any3_Bad() ->
    X = lists:any(fun (_) -> true end, [1]),
    case X of
        false -> ok
    end.

test_partition1_Ok() ->
    {_, _} = lists:partition(fun(_) -> true end, []).

test_partition2_Bad() ->
    {_} = lists:partition(fun(_) -> true end, []).

test_partition3_Ok() ->
    {X, Y} = lists:partition(fun(_) -> true end, []),
    true = is_list(X),
    true = is_list(Y).

test_partition4_Bad() ->
    {X, _} = lists:partition(fun(_) -> true end, []),
    false = is_list(X).

test_partition5_Bad() ->
    {_, Y} = lists:partition(fun(_) -> true end, []),
    false = is_list(Y).

test_sub1_Ok() ->
    X = lists:subtract([], []),
    case X of
        [] -> nil;
        [_ | _] -> cons
    end.

test_sub2_Ok() ->
    X = [] -- [],
    case X of
        [] -> nil;
        [_ | _] -> cons
    end.

test_sub3_Bad() ->
    X = lists:subtract([], []),
    case X of
        [_ | _] -> cons
    end.

test_sub4_Bad() ->
    X = [] -- [],
    case X of
        [_ | _] -> cons
    end.

test_sub5_Bad() ->
    X = lists:subtract([1], []),
    case X of
        [] -> nil
    end.

test_sub6_Bad() ->
    X = [1] -- [],
    case X of
        [] -> nil
    end.

test_list_append_with_atom_to_list_Ok() ->
    atom_to_list(checker) ++ "_clean".

test_list_append_with_atom_to_binary_Bad() ->
    atom_to_binary(checker) ++ "_clean".
