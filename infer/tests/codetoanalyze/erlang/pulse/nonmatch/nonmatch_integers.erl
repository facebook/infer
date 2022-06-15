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
    test_match8_Bad/0,
    test_binop_pattern_add_Ok/0,
    test_binop_pattern_add_Bad/0,
    test_binop_pattern_div_Bad/0,
    test_binop_pattern_div_Ok/0,
    test_binop_pattern_mul_Bad/0,
    test_binop_pattern_mul_Ok/0,
    test_binop_pattern_sub_Bad/0,
    test_binop_pattern_sub_Ok/0,
    test_binop_pattern_rem_Ok/0,
    test_binop_pattern_rem_Bad/0
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

binop_pattern_add(1 + 2) -> ok.

test_binop_pattern_add_Ok() -> binop_pattern_add(3).

test_binop_pattern_add_Bad() -> binop_pattern_add(33333).

binop_pattern_mul(7 * 2) -> ok.

test_binop_pattern_mul_Ok() -> binop_pattern_mul(14).

test_binop_pattern_mul_Bad() -> binop_pattern_mul(14141414).

binop_pattern_sub(8 - 2) -> ok.

test_binop_pattern_sub_Ok() -> binop_pattern_sub(6).

test_binop_pattern_sub_Bad() -> binop_pattern_sub(12345).

binop_pattern_div(5 div 2) -> ok.

test_binop_pattern_div_Ok() -> binop_pattern_div(2).

test_binop_pattern_div_Bad() -> binop_pattern_div(2222222).

binop_pattern_rem(5 rem 2) -> ok.

test_binop_pattern_rem_Ok() -> binop_pattern_rem(1).

test_binop_pattern_rem_Bad() -> binop_pattern_rem(111111).
