% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(atom_name).

-export([
    test_1_Ok/0, test_2_Ok/0, test_3_Ok/0, test_4_Ok/0, test_1_Bad/0, test_2_Bad/0]).

sink(not_ok, secret) -> erlang:error(taint_error);
sink(_, _) -> ok.

source() -> secret.

test_1_Ok() ->
    _ = source(),
    sink(not_ok, xxxxxx).

test_2_Ok() ->
    % Ok because `secret` doesn't come from `source()`
    sink(not_ok, secret).

test_3_Ok() ->
    % Ok because `secret` doesn't come from `source()`
    _ = source(),
    sink(not_ok, secret).

test_4_Ok() ->
    % Ok because first argument is different from atom `not_ok`
    sink(its_ok, source()).

test_1_Bad() ->
    X = source(),
    sink(not_ok, X).

test_2_Bad() ->
    sink(not_ok, source()).
