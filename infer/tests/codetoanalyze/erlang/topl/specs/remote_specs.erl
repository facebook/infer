% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(remote_specs).
-export([
    fnl_test_arg1_Bad/1,
    test_arg2_Ok/1,
    test_ret1_Bad/0,
    test_ret2_Bad/0
]).

-type dirty() :: atom().

-spec fnl_test_arg1_Bad(specs:dirty()) -> any().
fnl_test_arg1_Bad(X) ->
    sink(X).

-spec test_arg2_Ok(dirty()) -> any().
test_arg2_Ok(X) ->
    sink(X).

test_ret1_Bad() ->
    sink(specs:source()).

test_ret2_Bad() ->
    sink(source()).

-spec source() -> specs:dirty().
source() ->
    specs:source().

sink(_) -> ok.
