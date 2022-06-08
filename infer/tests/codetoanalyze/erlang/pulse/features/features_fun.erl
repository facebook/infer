% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_fun).
-include("../../common.hrl").

-export([
    one/0,
    test_fun_Ok/0,
    test_fun_Bad/0,
    test_fun_nomodule_Ok/0,
    test_fun_nomodule_Bad/0,
    fn_test_fun_notexists_Bad/0
]).

one() -> 1.

test_fun_Ok() ->
    F = fun features_fun:one/0,
    ?ASSERT_EQUAL(1, F()).

test_fun_Bad() ->
    F = fun features_fun:one/0,
    ?CRASH_IF_EQUAL(1, F()).

test_fun_nomodule_Ok() ->
    F = fun one/0,
    ?ASSERT_EQUAL(1, F()).

test_fun_nomodule_Bad() ->
    F = fun one/0,
    ?CRASH_IF_EQUAL(1, F()).

fn_test_fun_notexists_Bad() ->
    F = fun features_fun:notexists/0,
    % This causes 'exception error: undefined function' but we don't support that yet
    F().
