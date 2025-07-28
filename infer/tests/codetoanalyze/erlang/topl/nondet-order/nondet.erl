% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(nondet).
-export([
    test1_Bad/0,
    test2_Bad/0,
    test3_Bad/0,
    fn_test4_Bad/0,
    test5_Ok/0,
    test6_Ok/0
]).

sink(_) -> ok.

sink(_, _) -> ok.

id(X) -> X.

test1_Bad() ->
    M = #{1 => 2},
    K1 = maps:keys(M),
    K2 = maps:to_list(M),
    sink(K1, K2).

test2_Bad() ->
    M = #{1 => 2},
    K1 = maps:keys(M),
    K2 = maps:keys(M),
    K3 = id(K2),
    sink(K1, K3).

test3_Bad() ->
    M = #{1 => 2},
    K1 = maps:values(M),
    K2 = maps:keys(M),
    T = {K1, K2},
    sink(T).

fn_test4_Bad() ->
    M = #{1 => 2},
    K1 = maps:keys(M),
    K2 = [K || K := _ <- M],
    sink(K1, K2).

test5_Ok() ->
    M = #{1 => 2},
    K1 = maps:keys(M),
    _ = maps:keys(M),
    sink(K1, ok).

test6_Ok() ->
    M1 = #{1 => 2},
    M2 = #{1 => 3},
    K1 = maps:keys(M1),
    K2 = maps:keys(M2),
    % Different maps, no problem
    sink(K1, K2).
