% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_maps).

-export([
    test_type1_Ok/0,
    test_type2_Ok/0,
    test_type3_Bad/0,
    test_type4_Bad/0,
    test_key_match1_Ok/0,
    test_key_match2_Bad/0,
    fn_test_key_match3_Bad/0,
    test_key_match4_Ok/0,
    test_key_value_match1_Ok/0,
    test_key_value_match2_Bad/0,
    test_key_value_match3_Bad/0,
    fn_test_key_value_match4_Bad/0,
    test_update_badmap_Bad/0,
    test_update_exact_badkey_Bad/0,
    test_update_exact_Ok/0,
    test_update_exact_Bad/0,
    test_update_arrow_Ok/0,
    test_update_arrow_Bad/0,
    test_update_match_in_function_Ok/0,
    fn_test_update_match_in_function_Bad/0,
    test_update_old_value_Ok/0,
    fn_test_update_old_value_Bad/0,
    test_create_multiple_elements_Ok/0,
    fn_test_create_multiple_elements_Bad/0
]).

accepts_any_map(#{}) -> ok.

test_type1_Ok() ->
    accepts_any_map(#{}).

test_type2_Ok() ->
    accepts_any_map(#{1 => 2, 2 => 3}).

test_type3_Bad() ->
    accepts_any_map({i, am, a, tuple}).

test_type4_Bad() ->
    accepts_any_map([i, am, a, list]).

accepts_map_with_key_one(#{1 := _}) -> ok.

accepts_map_with_key_three(#{3 := _}) -> ok.

accepts_map_with_key_one_value_two(#{1 := 2}) -> ok.

test_key_match1_Ok() ->
    M = #{1 => 2},
    accepts_map_with_key_one(M).

test_key_match2_Bad() ->
    M = #{},
    accepts_map_with_key_one(M).

% Known false negative due to approximation
fn_test_key_match3_Bad() ->
    M = #{2 => 2},
    accepts_map_with_key_one(M).

test_key_match4_Ok() ->
    M = #{2 => 2, 3 => 3},
    accepts_map_with_key_three(M).

test_key_value_match1_Ok() ->
    M = #{1 => 2},
    accepts_map_with_key_one_value_two(M).

test_key_value_match2_Bad() ->
    M = #{1 => 3},
    accepts_map_with_key_one_value_two(M).

test_key_value_match3_Bad() ->
    M = #{},
    accepts_map_with_key_one_value_two(M).

% Known false negative due to approximation
fn_test_key_value_match4_Bad() ->
    M = #{2 => 2},
    accepts_map_with_key_one_value_two(M).

test_update_badmap_Bad() ->
    L = [1, 2, 3],
    L#{1 => 2}.

test_update_exact_badkey_Bad() ->
    M = #{},
    M1 = M#{1 := 2}.

test_update_exact_Ok() ->
    M = #{1 => 3},
    M1 = M#{1 := 2},
    accepts_map_with_key_one_value_two(M1).

test_update_exact_Bad() ->
    M = #{1 => 2},
    M1 = M#{1 := 3},
    accepts_map_with_key_one_value_two(M1).

test_update_arrow_Ok() ->
    M = #{},
    M1 = M#{1 => 2},
    accepts_map_with_key_one_value_two(M1).

test_update_arrow_Bad() ->
    M = #{},
    M1 = M#{1 => 3},
    accepts_map_with_key_one_value_two(M1).

% FP due to function call losing type information needed for equality test.
% See test_update_old_value_Ok
test_update_match_in_function_Ok() ->
    M = #{1 => 2},
    M1 = M#{2 => 3},
    accepts_map_with_key_one_value_two(M1).

% Should be a false negative due to approximation.
% See fn_test_update_old_value_Bad
fn_test_update_match_in_function_Bad() ->
    M = #{1 => 3},
    M1 = M#{2 => 3},
    accepts_map_with_key_one_value_two(M1).

test_update_old_value_Ok() ->
    M = #{1 => 2},
    M1 = M#{2 => 3},
    case M1 of #{1 := 2} -> ok end.

% Known false negative due to approximation.
fn_test_update_old_value_Bad() ->
    M = #{1 => 3},
    M1 = M#{2 => 3},
    case M1 of #{1 := 2} -> ok end.

test_create_multiple_elements_Ok() ->
    M = #{2 => 3, 1 => 2},
    accepts_map_with_key_one_value_two(M).

% Known false negative due to approximation
fn_test_create_multiple_elements_Bad() ->
    M = #{1 => 3, 2 => 2},
    accepts_map_with_key_one_value_two(M).
