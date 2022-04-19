% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_maps).

-export([
    test_is_key_Ok/0,
    test_is_key_Bad/0,
    test_is_key_badmap_Bad/0,
    test_get_Ok/0,
    test_get_Bad/0,
    test_get_badkey_Bad/0,
    test_get_badmap_Bad/0,
    test_put1_Ok/0,
    test_put2_Ok/0,
    test_put3_Ok/0,
    test_put4_Bad/0,
    test_put5_Bad/0,
    test_put6_Bad/0,
    test_new_Ok/0,
    test_new_Bad/0,
    test_merge_return_type_Ok/0,
    test_filter_return_type_Ok/0
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test_is_key_Ok() ->
    M = #{1 => 2},
    case maps:is_key(1, M) of
        true -> ok;
        _ -> warn(1)
    end.

test_is_key_Bad() ->
    M = #{1 => 2},
    case maps:is_key(1, M) of
        true -> warn(1);
        _ -> ok
    end.

test_is_key_badmap_Bad() ->
    maps:is_key(1, [not_a_map]).

test_get_Ok() ->
    M = #{1 => 2},
    case maps:get(1, M) of
        2 -> ok;
        _ -> warn(1)
    end.

test_get_Bad() ->
    M = #{1 => 2},
    case maps:get(1, M) of
        2 -> warn(1);
        _ -> ok
    end.

test_get_badkey_Bad() ->
    M = #{},
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
    M1 = maps:put(1, 3, [not_a_map]).

test_new_Ok() ->
    M = maps:new(),
    case maps:is_key(1, M) of
        false -> ok;
        _ -> warn(1)
    end.

test_new_Bad() ->
    M = maps:new(),
    case maps:is_key(1, M) of
        false -> warn(1);
        _ -> ok
    end.

test_merge_return_type_Ok() ->
    M = maps:merge(#{}, #{}),
    % No BAD_MAP should happen here
    maps:put(1, 3, M).

test_filter_return_type_Ok() ->
    M = maps:filter(fun(_, _) -> ok end, #{}),
    % No BAD_MAP should happen here
    maps:put(1, 3, M).
