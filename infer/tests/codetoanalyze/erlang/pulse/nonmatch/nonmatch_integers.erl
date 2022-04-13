% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_integers).

-export([
    test_match1_Ok/0,
    test_match2_Bad/0,
    test_match3_Bad/0,
    test_match4_Bad/0,
    test_match5_Ok/0,
    test_match6_Bad/0,
    test_match7_Bad/0,
    test_match8_Bad/0
]).

matches_42(42) -> ok.

matches_minus_42(-42) -> ok.

test_match1_Ok() -> matches_42(42).

test_match2_Bad() -> matches_42(0).

test_match3_Bad() -> matches_42(-42).

test_match4_Bad() -> matches_42(fortytwo).

test_match5_Ok() -> matches_minus_42(-42).

test_match6_Bad() -> matches_minus_42(0).

test_match7_Bad() -> matches_minus_42(42).

test_match8_Bad() -> matches_minus_42(minusfortytwo).
