% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_matchexpr).

-export([
    test_match1_Ok/0,
    test_match2_Bad/0,
    test_match3_Ok/0,
    test_match4_Bad/0
]).

f() -> X = 1.

g() -> _ = 1.

test_match1_Ok() ->
    1 = f().

test_match2_Bad() ->
    2 = f().

test_match3_Ok() ->
    1 = g().

test_match4_Bad() ->
    2 = g().
