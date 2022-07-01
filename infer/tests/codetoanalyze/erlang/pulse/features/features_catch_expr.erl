% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_catch_expr).

-export([
    test_catch_exit_Bad/0,
    fp_test_catch_exit_Ok/0,
    test_catch_throw_Bad/0,
    fp_test_catch_throw_Ok/0,
    test_catch_ok_Ok/0,
    test_catch_ok_Bad/0
]).

accepts_one(1) -> ok.

test_catch_ok_Ok() ->
    accepts_one(catch 1).

test_catch_ok_Bad() ->
    accepts_one(catch 2).

% TODO: model exception propagation T95448111
fp_test_catch_throw_Ok() ->
    accepts_one(catch throw(1)).

test_catch_throw_Bad() ->
    accepts_one(catch throw(2)).

accepts_exit_one({'EXIT', 1}) -> ok.

% TODO: model exception propagation T95448111
fp_test_catch_exit_Ok() ->
    accepts_exit_one(catch exit(1)).

test_catch_exit_Bad() ->
    accepts_exit_one(catch exit(2)).
