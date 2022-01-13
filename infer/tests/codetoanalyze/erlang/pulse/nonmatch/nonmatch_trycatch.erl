% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_trycatch).

-export([
    test_try_simple1_Ok/0,
    test_try_simple2_Ok/0,
    test_try_simple3_Bad/0,
    test_try_tail1_Ok/0,
    test_try_tail2_Ok/0,
    test_try_tail3_Bad/0
]).

try_simple(X) ->
    try X of
        0 -> zero;
        1 -> one
    catch
        _ -> ok
    end.

tail_with_try(X) ->
    try X of
        [_ | T] -> T
    catch
        _ -> ok
    end.

test_try_simple1_Ok() ->
    try_simple(0).
test_try_simple2_Ok() ->
    try_simple(1).
test_try_simple3_Bad() ->
    try_simple(2).

test_try_tail1_Ok() ->
    tail_with_try([1, 2]).
test_try_tail2_Ok() ->
    tail_with_try([1]).
test_try_tail3_Bad() ->
    tail_with_try([]).
