% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_lc).

-export([
    test_empty_Ok/0,
    test_empty_Bad/0,
    test_simple1_Ok/0,
    test_simple1a_Bad/0,
    test_simple1_Bad/0,
    test_simple2_Ok/0,
    test_simple2_Bad/0,
    test_simple3_Ok/0,
    test_simple3_Bad/0,
    test_simple4_Ok/0,
    fn_test_simple4_Bad/0,
    test_filtered1_Ok/0,
    test_filtered1_Bad/0,
    test_filtered2_Ok/0,
    test_filtered2_Bad/0,
    test_two_filters_Ok/0,
    test_two_filters_Bad/0,
    test_two_gen1_Ok/0,
    test_two_gen1_Bad/0,
    test_two_gen2_Ok/0,
    test_two_gen2_Bad/0,
    test_bad_gen_Bad/0
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test_empty_Ok() ->
    L = [X || X <- []],
    case L of
        [] -> ok;
        _ -> warn(1)
    end.

test_empty_Bad() ->
    L = [X || X <- []],
    case L of
        [] -> warn(1);
        _ -> ok
    end.

test_simple1_Ok() ->
    L = [X + 1 || X <- [2]],
    case L of
        [3] -> ok;
        _ -> warn(1)
    end.

test_simple1a_Bad() ->
    L = [X || X <- [3]],
    case L of
        [4] -> ok
    end.

test_simple1_Bad() ->
    L = [X + 1 || X <- [2]],
    case L of
        [3] -> warn(1);
        _ -> ok
    end.

test_simple2_Ok() ->
    L = [X + 1 || X <- [1, 2]],
    case L of
        [2, 3] -> ok;
        _ -> warn(1)
    end.

test_simple2_Bad() ->
    L = [X + 1 || X <- [1, 2]],
    case L of
        [2, 3] -> warn(1);
        _ -> ok
    end.

test_simple3_Ok() ->
    L = [X + 1 || X <- [1, 2, 3]],
    case L of
        [2, 3, 4] -> ok;
        _ -> warn(1)
    end.

test_simple3_Bad() ->
    L = [X + 1 || X <- [1, 2, 3]],
    case L of
        [2, 3, 4] -> warn(1);
        _ -> ok
    end.

test_simple4_Ok() ->
    L = [X + 1 || X <- [1, 2, 3, 4]],
    case L of
        [2, 3, 4, 5] -> ok;
        _ -> warn(1)
    end.

% Known FN due to loop unrolling limit
fn_test_simple4_Bad() ->
    L = [X + 1 || X <- [1, 2, 3, 4]],
    case L of
        [2, 3, 4, 5] -> warn(1);
        _ -> ok
    end.

test_filtered1_Ok() ->
    L = [X + 1 || X <- [1, 2], X > 1],
    case L of
        [3] -> ok;
        _ -> warn(1)
    end.

test_filtered1_Bad() ->
    L = [X + 1 || X <- [1, 2], X > 1],
    case L of
        [3] -> warn(1);
        _ -> ok
    end.

test_filtered2_Ok() ->
    L = [X + 1 || X <- [1, 2], X > 5],
    case L of
        [] -> ok;
        _ -> warn(1)
    end.

test_filtered2_Bad() ->
    L = [X + 1 || X <- [1, 2], X > 5],
    case L of
        [] -> warn(1);
        _ -> ok
    end.

test_two_filters_Ok() ->
    L = [X + 1 || X <- [1, 2, 3], X > 1, X + 1 < 4],
    case L of
        [3] -> ok;
        _ -> warn(1)
    end.

test_two_filters_Bad() ->
    L = [X + 1 || X <- [1, 2, 3], X > 1, X + 1 < 4],
    case L of
        [3] -> warn(1);
        _ -> ok
    end.

test_two_gen1_Ok() ->
    L = [X + Y || X <- [2, 3], Y <- [4]],
    case L of
        [6, 7] -> ok;
        _ -> warn(1)
    end.

test_two_gen1_Bad() ->
    L = [X + Y || X <- [2, 3], Y <- [4]],
    case L of
        [6, 7] -> warn(1);
        _ -> ok
    end.

test_two_gen2_Ok() ->
    L = [Y || X <- [2, 3], Y <- [X]],
    case L of
        [2, 3] -> ok;
        _ -> warn(1)
    end.

test_two_gen2_Bad() ->
    L = [Y || X <- [2, 3], Y <- [X]],
    case L of
        [2, 3] -> warn(1);
        _ -> ok
    end.

test_bad_gen_Bad() ->
    [X || X <- #{}].
