% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_lists).
-include("../../common.hrl").

-export([
    test_nil_Ok/0,
    test_nil_Bad/0,
    test_cons1_Ok/0,
    test_cons1_Bad/0,
    test_cons2_Ok/0,
    test_cons2_Bad/0,
    test_reverse0_Ok/0,
    test_reverse0_Bad/0,
    test_reverse1_Ok/0,
    test_reverse1_Bad/0,
    test_reverse2_Ok/0,
    test_reverse2_Bad/0,
    test_reverse3_Ok/0,
    test_reverse3_Bad/0,
    test_reverse4_Ok/0,
    fn_test_reverse4_Bad/0,
    test_listadd0_Ok/0,
    test_listadd0_Bad/0,
    test_listadd0a_Ok/0,
    test_listadd0a_Bad/0,
    test_listadd1_Ok/0,
    test_listadd1_Bad/0,
    test_listadd2_Ok/0,
    test_listadd2_Bad/0,
    test_listadd2a_Ok/0,
    test_listadd2a_Bad/0,
    test_listadd3_Ok/0,
    test_listadd3_Bad/0,
    test_listadd4_Ok/0,
    fn_test_listadd4_Bad/0,
    test_listappend0_Ok/0,
    test_listappend0_Bad/0,
    test_listappend0a_Ok/0,
    test_listappend0a_Bad/0,
    test_listappend1_Ok/0,
    test_listappend1_Bad/0,
    test_listappend2_Ok/0,
    test_listappend2_Bad/0,
    test_listappend2a_Ok/0,
    test_listappend2a_Bad/0,
    test_listappend3_Ok/0,
    test_listappend3_Bad/0,
    test_listappend4_Ok/0,
    fn_test_listappend4_Bad/0,
    test_listappend5_Bad/0,
    test_listappend6_Ok/0,
    test_listappend6_Bad/0,
    test_listappend7_Ok/0,
    test_listappend7_Bad/0,
    fp_test_listappend8_Ok/0,
    test_listappend8_Bad/0,
    test_listappend9_Ok/0,
    fp_test_listsub1_Ok/0,
    fp_test_listsub2_Ok/0,
    test_listsub3_Bad/0,
    test_listsub4_Bad/0,
    test_listforeach_returnvalue_Ok/0,
    test_listforeach_returnvalue_Bad/0,
    fp_test_string_Ok/0,
    test_string_Bad/0
]).

test_nil_Ok() ->
    Nil = [],
    case Nil of
        [] -> ok
    end.

test_nil_Bad() ->
    NotNil = [1],
    case NotNil of
        [] -> ok
    end.

test_cons1_Ok() ->
    Nil = [],
    L2 = [2 | Nil],
    L1 = [1 | L2],
    case L1 of
        [1, 2] -> ok
    end.

test_cons1_Bad() ->
    Nil = [],
    L2 = [3 | Nil],
    L1 = [1 | L2],
    case L1 of
        [1, 2] -> ok
    end.

test_cons2_Ok() ->
    L = [1, 2, 3],
    case L of
        [1 | L2] ->
            case L2 of
                [2 | T] ->
                    case T of
                        [3] -> ok
                    end
            end
    end.

test_cons2_Bad() ->
    L = [1, 100000, 3],
    case L of
        [1 | L2] ->
            case L2 of
                [2 | T] ->
                    case T of
                        [3] -> ok
                    end
            end
    end.

test_reverse0_Ok() ->
    L = lists:reverse([]),
    ?ASSERT_EQUAL([], L).

test_reverse0_Bad() ->
    L = lists:reverse([]),
    ?CRASH_IF_EQUAL([], L).

test_reverse1_Ok() ->
    L = lists:reverse([1234]),
    ?ASSERT_EQUAL([1234], L).

test_reverse1_Bad() ->
    L = lists:reverse([1234]),
    ?CRASH_IF_EQUAL([1234], L).

test_reverse2_Ok() ->
    L = lists:reverse([1234, 567]),
    ?ASSERT_EQUAL([567, 1234], L).

test_reverse2_Bad() ->
    L = lists:reverse([1234, 567]),
    ?CRASH_IF_EQUAL([567, 1234], L).

test_reverse3_Ok() ->
    L = lists:reverse([1234, 567, 89]),
    ?ASSERT_EQUAL([89, 567, 1234], L).

test_reverse3_Bad() ->
    L = lists:reverse([1234, 567, 89]),
    ?CRASH_IF_EQUAL([89, 567, 1234], L).

test_reverse4_Ok() ->
    L = lists:reverse([1234, 567, 89, 0]),
    ?ASSERT_EQUAL([0, 89, 567, 1234], L).

% Known false negative (list too long)
fn_test_reverse4_Bad() ->
    L = lists:reverse([1234, 567, 89, 0]),
    ?CRASH_IF_EQUAL([0, 89, 567, 1234], L).

test_listadd0_Ok() ->
    L = [] ++ [1, 2],
    ?ASSERT_EQUAL([1, 2], L).

test_listadd0_Bad() ->
    L = [] ++ [1, 2],
    ?CRASH_IF_EQUAL([1, 2], L).

test_listadd0a_Ok() ->
    L = [] ++ [],
    ?ASSERT_EQUAL([], L).

test_listadd0a_Bad() ->
    L = [] ++ [],
    ?CRASH_IF_EQUAL([], L).

test_listadd1_Ok() ->
    L = [1] ++ [2, 3],
    ?ASSERT_EQUAL([1, 2, 3], L).

test_listadd1_Bad() ->
    L = [1] ++ [2, 3],
    ?CRASH_IF_EQUAL([1, 2, 3], L).

test_listadd2_Ok() ->
    L = [1, 2] ++ [3],
    ?ASSERT_EQUAL([1, 2, 3], L).

test_listadd2_Bad() ->
    L = [1, 2] ++ [3],
    ?CRASH_IF_EQUAL([1, 2, 3], L).

test_listadd2a_Ok() ->
    L = [1, 2] ++ [],
    ?ASSERT_EQUAL([1, 2], L).

test_listadd2a_Bad() ->
    L = [1, 2] ++ [],
    ?CRASH_IF_EQUAL([1, 2], L).

test_listadd3_Ok() ->
    L = [1, 2, 3] ++ [4, 5],
    ?ASSERT_EQUAL([1, 2, 3, 4, 5], L).

test_listadd3_Bad() ->
    L = [1, 2, 3] ++ [4, 5],
    ?CRASH_IF_EQUAL([1, 2, 3, 4, 5], L).

test_listadd4_Ok() ->
    L = [1, 2, 3, 4] ++ [5],
    ?ASSERT_EQUAL([1, 2, 3, 4, 5], L).

% Known false negative (list too long)
fn_test_listadd4_Bad() ->
    L = [1, 2, 3, 4] ++ [5],
    ?CRASH_IF_EQUAL([1, 2, 3, 4, 5], L).

test_listappend0_Ok() ->
    L = lists:append([], [1, 2]),
    ?ASSERT_EQUAL([1, 2], L).

test_listappend0_Bad() ->
    L = lists:append([], [1, 2]),
    ?CRASH_IF_EQUAL([1, 2], L).

test_listappend0a_Ok() ->
    L = lists:append([], []),
    ?ASSERT_EQUAL([], L).

test_listappend0a_Bad() ->
    L = lists:append([], []),
    ?CRASH_IF_EQUAL([], L).

test_listappend1_Ok() ->
    L = lists:append([1], [2, 3]),
    ?ASSERT_EQUAL([1, 2, 3], L).

test_listappend1_Bad() ->
    L = lists:append([1], [2, 3]),
    ?CRASH_IF_EQUAL([1, 2, 3], L).

test_listappend2_Ok() ->
    L = lists:append([1, 2], [3]),
    ?ASSERT_EQUAL([1, 2, 3], L).

test_listappend2_Bad() ->
    L = lists:append([1, 2], [3]),
    ?CRASH_IF_EQUAL([1, 2, 3], L).

test_listappend2a_Ok() ->
    L = lists:append([1, 2], []),
    ?ASSERT_EQUAL([1, 2], L).

test_listappend2a_Bad() ->
    L = lists:append([1, 2], []),
    ?CRASH_IF_EQUAL([1, 2], L).

test_listappend3_Ok() ->
    L = lists:append([1, 2, 3], [4, 5]),
    ?ASSERT_EQUAL([1, 2, 3, 4, 5], L).

test_listappend3_Bad() ->
    L = lists:append([1, 2, 3], [4, 5]),
    ?CRASH_IF_EQUAL([1, 2, 3, 4, 5], L).

test_listappend4_Ok() ->
    L = lists:append([1, 2, 3, 4], [5]),
    ?ASSERT_EQUAL([1, 2, 3, 4, 5], L).

% Known false negative (list too long)
fn_test_listappend4_Bad() ->
    L = lists:append([1, 2, 3, 4], [5]),
    ?CRASH_IF_EQUAL([1, 2, 3, 4, 5], L).

one() -> 1.

test_listappend5_Bad() ->
    one() ++ "?".

test_listappend6_Ok() ->
    R = [] ++ one(),
    ?ASSERT_EQUAL(1, R).

test_listappend6_Bad() ->
    one() ++ [].

test_listappend7_Ok() ->
    L = "sjhdkjxmcxc" ++ [1,2,3,4,5],
    ?ASSERT_EQUAL([115,106,104,100,107,106,120,109,99,120,99,1,2,3,4,5], L).

test_listappend7_Bad() -> sjhdkjxmcxc ++ [1,2,3,4,5].

% FP due to lack of support for strings.
% For now, the model for string creation only computes the type. T93361792
fp_test_listappend8_Ok() ->
    L = "ok" ++ "hello",
    ?ASSERT_EQUAL("okhello", L).

test_listappend8_Bad() ->
    ok ++ hello.

test_listappend9_Ok() ->
    L = [1,2] ++ atom,
    ?ASSERT_EQUAL([1,2|atom], L).

% Not yet supported
fp_test_listsub1_Ok() ->
    L = [1, 2, 3] -- [2],
    ?ASSERT_EQUAL([1, 3], L).

% Not yet supported
fp_test_listsub2_Ok() ->
    L = lists:subtract([1, 2, 3], [2]),
    ?ASSERT_EQUAL([1, 3], L).

test_listsub3_Bad() ->
    L = [1, 2, 3] -- [2],
    ?CRASH_IF_EQUAL([1, 3], L).

test_listsub4_Bad() ->
    L = lists:subtract([1, 2, 3], [2]),
    ?CRASH_IF_EQUAL([1, 3], L).


test_listforeach_returnvalue_Ok() ->
    Y = lists:foreach(fun(X) -> X end, [1, 2, 3]),
    ?ASSERT_EQUAL(ok, Y).

test_listforeach_returnvalue_Bad() ->
    Y = lists:foreach(fun(X) -> X end, [1, 2, 3]),
    ?CRASH_IF_EQUAL(ok, Y).

fp_test_string_Ok() ->
    L = "foo",
    ?ASSERT_EQUAL([$f, $o, $o], L).

test_string_Bad() ->
    L = "foo",
    ?CRASH_IF_EQUAL([$f, $o, $o], L).
