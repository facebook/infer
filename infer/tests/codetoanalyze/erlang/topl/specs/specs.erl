% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(specs).
-export([
    fnl_test_arg1_Bad/1,
    test_arg2_Ok/1,
    test_arg3_Ok/1,
    test_ret1_Bad/0,
    test_ret2_Bad/0,
    test_ret3_Ok/0,
    test_ret4_Ok/0,
    source/0
]).

-export_type([dirty/0]).

-type dirty() :: atom().

-spec fnl_test_arg1_Bad(dirty()) -> any().
fnl_test_arg1_Bad(X) ->
    sink(X).

-spec test_arg2_Ok(integer()) -> any().
test_arg2_Ok(X) ->
    sink(X).

-spec test_arg3_Ok(atom()) -> any().
test_arg3_Ok(X) ->
    sink(X).

test_ret1_Bad() ->
    sink(source()).

test_ret2_Bad() ->
    sinksink(source()).

test_ret3_Ok() ->
    nonsink(source()).

test_ret4_Ok() ->
    sink(nonsource()).

sinksink(X) ->
    sink(X).

-spec source() -> dirty().
source() -> dirty.

sink(_) -> ok.

nonsource() -> clean.
nonsink(_) -> ok.
