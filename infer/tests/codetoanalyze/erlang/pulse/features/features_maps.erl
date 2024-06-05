% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_maps).
-include("../../common.hrl").

-export([
    test_is_key_Ok/0,
    test_is_key_Bad/0,
    test_is_key_badmap_Bad/0,
    test_get_Ok/0,
    test_get_Bad/0,
    test_get_badkey1_Ok/0,
    test_get_badkey2_Bad/0,
    fn_test_get_badkey3_Bad/0,
    test_get_badmap_Bad/0,
    test_put1_Ok/0,
    test_put2_Ok/0,
    test_put3_Ok/0,
    test_put4_Bad/0,
    test_put5_Bad/0,
    test_put6_Bad/0,
    test_new_Ok/0,
    test_new_Bad/0,
    test_key_not_checked_Latent/1,
    test_key_checked_Ok/1,
    test_update_exact1_Ok/0,
    test_update_exact2_Bad/0,
    fn_test_update_exact3_Bad/0,
    test_to_list1_Ok/0,
    test_to_list2_Bad/0,
    test_to_list3_Ok/0,
    test_to_list4_Bad/0,
    fp_test_to_list5_Ok/0,
    fn_test_to_list6_Bad/0
]).

test_is_key_Ok() ->
    M = #{1 => 2},
    ?ASSERT_EQUAL(true, maps:is_key(1, M)).

test_is_key_Bad() ->
    M = #{1 => 2},
    ?CRASH_IF_EQUAL(true, maps:is_key(1, M)).

test_is_key_badmap_Bad() ->
    maps:is_key(1, [not_a_map]).

test_get_Ok() ->
    M = #{1 => 2},
    ?ASSERT_EQUAL(2, maps:get(1, M)).

test_get_Bad() ->
    M = #{1 => 2},
    ?CRASH_IF_EQUAL(2, maps:get(1, M)).

test_get_badkey1_Ok() ->
    M = #{1 => 2},
    maps:get(1, M).

test_get_badkey2_Bad() ->
    M = #{},
    maps:get(1, M).

% Known limitation due to recency abstraction
fn_test_get_badkey3_Bad() ->
    M = #{2 => 3},
    maps:get(1, M).

test_get_badmap_Bad() ->
    maps:get(1, [not_a_map]).

accepts_map_with_key_one_value_two(#{1 := 2}) -> ok.

test_put1_Ok() ->
    M = #{},
    M1 = maps:put(1, 2, M),
    accepts_map_with_key_one_value_two(M1).

test_put2_Ok() ->
    M = #{1 => 3},
    M1 = maps:put(1, 2, M),
    accepts_map_with_key_one_value_two(M1).

test_put3_Ok() ->
    M = #{2 => 3},
    M1 = maps:put(1, 2, M),
    accepts_map_with_key_one_value_two(M1).

test_put4_Bad() ->
    M = #{},
    M1 = maps:put(1, 3, M),
    accepts_map_with_key_one_value_two(M1).

test_put5_Bad() ->
    M = #{1 => 2},
    M1 = maps:put(1, 3, M),
    accepts_map_with_key_one_value_two(M1).

test_put6_Bad() ->
    maps:put(1, 3, [not_a_map]).

test_new_Ok() ->
    M = maps:new(),
    ?ASSERT_EQUAL(false, maps:is_key(1, M)).

test_new_Bad() ->
    M = maps:new(),
    ?CRASH_IF_EQUAL(false, maps:is_key(1, M)).

test_key_not_checked_Latent(M) ->
    if
        % We don't check for the key: BAD
        is_map(M) -> maps:get(key, M);
        true -> nope
    end.

test_key_checked_Ok(M) ->
    if
        is_map(M) ->
            % We first check the key: OK
            case maps:is_key(key, M) of
                true -> maps:get(key, M);
                _ -> nope
            end;
        true -> nope
    end.

test_update_exact1_Ok() ->
    M = #{2 => 1},
    M#{2 := 3}.

test_update_exact2_Bad() ->
    M = #{},
    M#{2 := 3}.

% Known limitation due to recency abstraction
fn_test_update_exact3_Bad() ->
    M = #{1 => 2},
    M#{2 := 3}.

test_to_list1_Ok() ->
    ?ASSERT_EQUAL([], maps:to_list(#{})).

test_to_list2_Bad() ->
    ?CRASH_IF_EQUAL([], maps:to_list(#{})).

test_to_list3_Ok() ->
    ?ASSERT_EQUAL([{1, 2}], maps:to_list(#{1 => 2})).

test_to_list4_Bad() ->
    ?CRASH_IF_EQUAL([{1, 2}], maps:to_list(#{1 => 2})).

% Known limitation due to recency abstraction
fp_test_to_list5_Ok() ->
    ?ASSERT_EQUAL([{1, 2}, {3, 4}], maps:to_list(#{1 => 2, 3 => 4})).

% Known limitation due to recency abstraction
fn_test_to_list6_Bad() ->
    ?CRASH_IF_EQUAL([{1, 2}, {3, 4}], maps:to_list(#{1 => 2, 3 => 4})).
