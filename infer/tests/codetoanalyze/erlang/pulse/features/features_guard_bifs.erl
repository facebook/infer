% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_guard_bifs).

-export([
    test_is_list3_nomodule_Bad/0,
    test_is_list2_nomodule_Ok/0,
    test_is_list1_nomodule_Ok/0,
    test_is_map1_Ok/0,
    test_is_map2_Ok/0,
    test_is_map3_Ok/0,
    test_is_map4_Bad/0,
    test_is_map5_Bad/0,
    test_is_map_nomodule_Ok/0,
    test_is_map_nomodule_Bad/0,
    test_is_list1_Ok/0,
    test_is_list2_Ok/0,
    test_is_list3_Ok/0,
    test_is_list4_Ok/0,
    test_is_list5_Bad/0,
    test_is_list6_Bad/0,
    test_accepts_atom1_Ok/0,
    test_accepts_atom2_Ok/0,
    test_accepts_atom3_Ok/0,
    test_accepts_atom4_Bad/0,
    test_accepts_atom5_Bad/0,
    test_accepts_atom_nomodule_Ok/0,
    test_accepts_atom_nomodule_Bad/0,
    test_accepts_bool1_Ok/0,
    test_accepts_bool2_Ok/0,
    test_accepts_bool3_Bad/0,
    test_accepts_bool4_Bad/0,
    test_accepts_bool_nomodule1_Ok/0,
    test_accepts_bool_nomodule2_Ok/0,
    test_accepts_bool_nomodule3_Bad/0,
    test_accepts_bool_nomodule4_Bad/0
]).

accepts_map(M) when erlang:is_map(M) -> ok.

accepts_non_map(M) when not erlang:is_map(M) -> ok.

test_is_map1_Ok() -> accepts_map(#{}).

test_is_map2_Ok() -> accepts_non_map(an_atom).

test_is_map3_Ok() -> accepts_non_map(41).

test_is_map4_Bad() -> accepts_map([]).

test_is_map5_Bad() -> accepts_map(41).

accepts_map_nomodule(M) when is_map(M) -> ok.

test_is_map_nomodule_Ok() -> accepts_map_nomodule(#{}).

test_is_map_nomodule_Bad() -> accepts_map_nomodule([]).

accepts_list(L) when erlang:is_list(L) -> ok.

accepts_non_list(L) when not erlang:is_list(L) -> ok.

test_is_list1_Ok() -> accepts_list([]).

test_is_list2_Ok() -> accepts_list([1, 2]).

test_is_list3_Ok() -> accepts_non_list(#{}).

test_is_list4_Ok() -> accepts_non_list(182).

test_is_list5_Bad() -> accepts_list(#{}).

test_is_list6_Bad() -> accepts_list(182).

accepts_list_nomodule(L) when is_list(L) -> ok.

test_is_list1_nomodule_Ok() -> accepts_list_nomodule([]).

test_is_list2_nomodule_Ok() -> accepts_list_nomodule([1, 2]).

test_is_list3_nomodule_Bad() -> accepts_list_nomodule(#{}).

accepts_atom(A) when erlang:is_atom(A) -> ok.

accepts_non_atom(A) when not erlang:is_atom(A) -> ok.

test_accepts_atom1_Ok() -> accepts_atom(some_atom).

test_accepts_atom2_Ok() -> accepts_non_atom([1, 2, 3]).

test_accepts_atom3_Ok() -> accepts_non_atom(75).

test_accepts_atom4_Bad() -> accepts_atom([not_an, atom]).

test_accepts_atom5_Bad() -> accepts_atom(75).

accepts_atom_nomodule(A) when is_atom(A) -> ok.

test_accepts_atom_nomodule_Ok() -> accepts_atom_nomodule(some_atom).

test_accepts_atom_nomodule_Bad() -> accepts_atom_nomodule([not_an, atom]).

accepts_bool(B) when erlang:is_boolean(B) -> ok.

test_accepts_bool1_Ok() -> accepts_bool(true).

test_accepts_bool2_Ok() -> accepts_bool(false).

test_accepts_bool3_Bad() -> accepts_bool(not_a_bool).

test_accepts_bool4_Bad() -> accepts_bool([not_an, atom]).

accepts_bool_nomodule(B) when is_boolean(B) -> ok.

test_accepts_bool_nomodule1_Ok() -> accepts_bool_nomodule(true).

test_accepts_bool_nomodule2_Ok() -> accepts_bool_nomodule(false).

test_accepts_bool_nomodule3_Bad() -> accepts_bool_nomodule(not_a_bool).

test_accepts_bool_nomodule4_Bad() -> accepts_bool_nomodule([not_an, atom]).
