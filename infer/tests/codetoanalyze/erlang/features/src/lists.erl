% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(lists).

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
    fn_test_listappend4_Bad/0
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

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
    case L of
        [] -> ok;
        _ -> warn(1)
    end.

test_reverse0_Bad() ->
    L = lists:reverse([]),
    case L of
        [] -> warn(1);
        _ -> ok
    end.

test_reverse1_Ok() ->
    L = lists:reverse([1234]),
    case L of
        [1234] -> ok;
        _ -> warn(1)
    end.

test_reverse1_Bad() ->
    L = lists:reverse([1234]),
    case L of
        [1234] -> warn(1);
        _ -> ok
    end.

test_reverse2_Ok() ->
    L = lists:reverse([1234, 567]),
    case L of
        [567, 1234] -> ok;
        _ -> warn(1)
    end.

test_reverse2_Bad() ->
    L = lists:reverse([1234, 567]),
    case L of
        [567, 1234] -> warn(1);
        _ -> ok
    end.

test_reverse3_Ok() ->
    L = lists:reverse([1234, 567, 89]),
    case L of
        [89, 567, 1234] -> ok;
        _ -> warn(1)
    end.

test_reverse3_Bad() ->
    L = lists:reverse([1234, 567, 89]),
    case L of
        [89, 567, 1234] -> warn(1);
        _ -> ok
    end.

test_reverse4_Ok() ->
    L = lists:reverse([1234, 567, 89, 0]),
    case L of
        [0, 89, 567, 1234] -> ok;
        _ -> warn(1)
    end.

% Known false negative (list too long)
fn_test_reverse4_Bad() ->
    L = lists:reverse([1234, 567, 89, 0]),
    case L of
        [0, 89, 567, 1234] -> warn(1);
        _ -> ok
    end.

test_listadd0_Ok() ->
    L = [] ++ [1, 2],
    case L of
        [1, 2] -> ok;
        _ -> warn(1)
    end.

test_listadd0_Bad() ->
    L = [] ++ [1, 2],
    case L of
        [1, 2] -> warn(1);
        _ -> ok
    end.

test_listadd0a_Ok() ->
    L = [] ++ [],
    case L of
        [] -> ok;
        _ -> warn(1)
    end.

test_listadd0a_Bad() ->
    L = [] ++ [],
    case L of
        [] -> warn(1);
        _ -> ok
    end.

test_listadd1_Ok() ->
    L = [1] ++ [2, 3],
    case L of
        [1, 2, 3] -> ok;
        _ -> warn(1)
    end.

test_listadd1_Bad() ->
    L = [1] ++ [2, 3],
    case L of
        [1, 2, 3] -> warn(1);
        _ -> ok
    end.

test_listadd2_Ok() ->
    L = [1, 2] ++ [3],
    case L of
        [1, 2, 3] -> ok;
        _ -> warn(1)
    end.

test_listadd2_Bad() ->
    L = [1, 2] ++ [3],
    case L of
        [1, 2, 3] -> warn(1);
        _ -> ok
    end.

test_listadd2a_Ok() ->
    L = [1, 2] ++ [],
    case L of
        [1, 2] -> ok;
        _ -> warn(1)
    end.

test_listadd2a_Bad() ->
    L = [1, 2] ++ [],
    case L of
        [1, 2] -> warn(1);
        _ -> ok
    end.

test_listadd3_Ok() ->
    L = [1, 2, 3] ++ [4, 5],
    case L of
        [1, 2, 3, 4, 5] -> ok;
        _ -> warn(1)
    end.

test_listadd3_Bad() ->
    L = [1, 2, 3] ++ [4, 5],
    case L of
        [1, 2, 3, 4, 5] -> warn(1);
        _ -> ok
    end.

test_listadd4_Ok() ->
    L = [1, 2, 3, 4] ++ [5],
    case L of
        [1, 2, 3, 4, 5] -> ok;
        _ -> warn(1)
    end.

% Known false negative (list too long)
fn_test_listadd4_Bad() ->
    L = [1, 2, 3, 4] ++ [5],
    case L of
        [1, 2, 3, 4, 5] -> warn(1);
        _ -> ok
    end.

test_listappend0_Ok() ->
    L = lists:append([], [1, 2]),
    case L of
        [1, 2] -> ok;
        _ -> warn(1)
    end.

test_listappend0_Bad() ->
    L = lists:append([], [1, 2]),
    case L of
        [1, 2] -> warn(1);
        _ -> ok
    end.

test_listappend0a_Ok() ->
    L = lists:append([], []),
    case L of
        [] -> ok;
        _ -> warn(1)
    end.

test_listappend0a_Bad() ->
    L = lists:append([], []),
    case L of
        [] -> warn(1);
        _ -> ok
    end.

test_listappend1_Ok() ->
    L = lists:append([1], [2, 3]),
    case L of
        [1, 2, 3] -> ok;
        _ -> warn(1)
    end.

test_listappend1_Bad() ->
    L = lists:append([1], [2, 3]),
    case L of
        [1, 2, 3] -> warn(1);
        _ -> ok
    end.

test_listappend2_Ok() ->
    L = lists:append([1, 2], [3]),
    case L of
        [1, 2, 3] -> ok;
        _ -> warn(1)
    end.

test_listappend2_Bad() ->
    L = lists:append([1, 2], [3]),
    case L of
        [1, 2, 3] -> warn(1);
        _ -> ok
    end.

test_listappend2a_Ok() ->
    L = lists:append([1, 2], []),
    case L of
        [1, 2] -> ok;
        _ -> warn(1)
    end.

test_listappend2a_Bad() ->
    L = lists:append([1, 2], []),
    case L of
        [1, 2] -> warn(1);
        _ -> ok
    end.

test_listappend3_Ok() ->
    L = lists:append([1, 2, 3], [4, 5]),
    case L of
        [1, 2, 3, 4, 5] -> ok;
        _ -> warn(1)
    end.

test_listappend3_Bad() ->
    L = lists:append([1, 2, 3], [4, 5]),
    case L of
        [1, 2, 3, 4, 5] -> warn(1);
        _ -> ok
    end.

test_listappend4_Ok() ->
    L = lists:append([1, 2, 3, 4], [5]),
    case L of
        [1, 2, 3, 4, 5] -> ok;
        _ -> warn(1)
    end.

% Known false negative (list too long)
fn_test_listappend4_Bad() ->
    L = lists:append([1, 2, 3, 4], [5]),
    case L of
        [1, 2, 3, 4, 5] -> warn(1);
        _ -> ok
    end.
