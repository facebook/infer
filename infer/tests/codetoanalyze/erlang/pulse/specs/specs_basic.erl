% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(specs_basic).

-export([
    test_record6_Bad/1,
    test_record5_Ok/1,
    test_record4_Latent/1,
    test_record3_Latent/1,
    test_record2_Latent/1,
    test_record1_Ok/1,
    test_tuple_with_types5_Latent/1,
    test_tuple_with_types4_Bad/1,
    test_tuple_with_types3_Ok/1,
    test_tuple_with_types2_Latent/1,
    test_tuple_with_types1_Ok/1,
    test_tuple_of_2_Latent/1,
    test_tuple_of_2_Ok/1,
    test_tuple_of_1_Latent/1,
    test_tuple_of_1_Ok/1,
    test_tuple_of_0_Latent/1,
    test_tuple_of_0_Ok/1,
    test_list7_Latent/1,
    test_list6_Latent/1,
    test_list5_Latent/1,
    test_list4_Ok/1,
    test_list3_Ok/1,
    test_list2_Ok/1,
    test_list1_Ok/1,
    test_nil3_Latent/1,
    test_nil2_Ok/1,
    test_nil1_Ok/1,
    test_union4_Latent/1,
    test_union3_Latent/1,
    test_union2_Latent/1,
    test_union1_Ok/1,
    test_map_Latent/1,
    test_map_Ok/1,
    test_bool6_Ok/1,
    test_bool5_Latent/1,
    test_bool4_Latent/1,
    fpl_test_bool3_Ok/1,
    test_bool2_Latent/1,
    test_bool1_Ok/1,
    test_atom_literal4_Latent/1,
    test_atom_literal3_Ok/1,
    test_atom_literal2_Latent/1,
    test_atom_literal1_Ok/1,
    test_atom3_Latent/1,
    test_atom2_Latent/1,
    test_atom1_Ok/1,
    test_any_Latent/1,
    test_any_Ok/1,
    test_no_args_Ok/0,
    test_no_args_Bad/0,
    test_integer1_Ok/1,
    test_integer2_Latent/1
]).

-spec test_any_Ok(any()) -> any().
test_any_Ok(_) -> ok.

-spec test_any_Latent(any()) -> any().
test_any_Latent([]) -> ok.

-spec test_atom1_Ok(atom()) -> any().
test_atom1_Ok(X) when is_atom(X) -> ok.

test_atom2_Latent(X) when is_atom(X) -> ok.

-spec test_atom3_Latent(atom()) -> any().
test_atom3_Latent(true) -> ok;
test_atom3_Latent(false) -> ok.

-spec test_atom_literal1_Ok('ok') -> any().
test_atom_literal1_Ok(X) when is_atom(X) -> ok.

test_atom_literal2_Latent(X) when is_atom(X) -> ok.

-spec test_atom_literal3_Ok('ok') -> any().
test_atom_literal3_Ok(ok) -> ok.

test_atom_literal4_Latent(ok) -> ok.

-spec test_bool1_Ok(boolean()) -> any().
test_bool1_Ok(X) when is_atom(X) -> ok.

test_bool2_Latent(X) when is_atom(X) -> ok.

% T115354480
-spec fpl_test_bool3_Ok(boolean()) -> any().
fpl_test_bool3_Ok(true) -> ok;
fpl_test_bool3_Ok(false) -> ok.

-spec test_bool4_Latent(boolean()) -> any().
test_bool4_Latent(true) -> ok.

-spec test_bool5_Latent(boolean()) -> any().
test_bool5_Latent(false) -> ok.

-spec test_bool6_Ok(boolean()) -> any().
test_bool6_Ok(X) when is_boolean(X) -> ok.

-spec test_map_Ok(map()) -> any().
test_map_Ok(X) when is_map(X) -> ok.

test_map_Latent(X) when is_map(X) -> ok.

-spec test_union1_Ok(atom() | map()) -> any().
test_union1_Ok(X) when is_atom(X) -> atom;
test_union1_Ok(X) when is_map(X) -> map.

test_union2_Latent(X) when is_atom(X) -> atom;
test_union2_Latent(X) when is_map(X) -> map.

-spec test_union3_Latent(atom() | map()) -> any().
test_union3_Latent(X) when is_atom(X) -> atom.

-spec test_union4_Latent(atom() | map()) -> any().
test_union4_Latent(X) when is_map(X) -> map.

-spec test_nil1_Ok([]) -> any().
test_nil1_Ok([]) -> ok.

-spec test_nil2_Ok(nil()) -> any().
test_nil2_Ok([]) -> ok.

test_nil3_Latent([]) -> ok.

-spec test_list1_Ok(list()) -> any().
test_list1_Ok([_ | _]) -> cons;
test_list1_Ok([]) -> nil.

-spec test_list2_Ok(list()) -> any().
test_list2_Ok(X) when is_list(X) -> ok.

-spec test_list3_Ok(list(any())) -> any().
test_list3_Ok([_ | _]) -> cons;
test_list3_Ok([]) -> nil.

-spec test_list4_Ok([any()]) -> any().
test_list4_Ok([_ | _]) -> cons;
test_list4_Ok([]) -> nil.

test_list5_Latent([_ | _]) -> cons;
test_list5_Latent([]) -> nil.

-spec test_list6_Latent(list()) -> any().
test_list6_Latent([_ | _]) -> cons.

-spec test_list7_Latent(list()) -> any().
test_list7_Latent([]) -> nil.

-spec test_tuple_of_0_Ok({}) -> any().
test_tuple_of_0_Ok({}) -> ok.

test_tuple_of_0_Latent({}) -> ok.

-spec test_tuple_of_1_Ok({any()}) -> any().
test_tuple_of_1_Ok({_}) -> ok.

test_tuple_of_1_Latent({_}) -> ok.

-spec test_tuple_of_2_Ok({any(), any()}) -> any().
test_tuple_of_2_Ok({_, _}) -> ok.

test_tuple_of_2_Latent({_, _}) -> ok.

-spec test_tuple_with_types1_Ok({atom()}) -> any().
test_tuple_with_types1_Ok({X}) when is_atom(X) -> ok.

test_tuple_with_types2_Latent({X}) when is_atom(X) -> ok.

-spec test_tuple_with_types3_Ok({atom(), map()}) -> any().
test_tuple_with_types3_Ok({X, Y}) when is_atom(X), is_map(Y) -> ok.

% not actually sure if this should be Bad or Latent
-spec test_tuple_with_types4_Bad({atom(), map()}) -> any().
test_tuple_with_types4_Bad({X, Y}) when is_map(X), is_atom(Y) -> ok.

-spec test_tuple_with_types5_Latent({any()}) -> any().
test_tuple_with_types5_Latent({X}) when is_atom(X) -> ok.

-record(car, {plate, owner}).

-spec test_record1_Ok(#car{}) -> any().
test_record1_Ok({_, _, _}) -> ok.

test_record2_Latent({_, _, _}) -> ok.

-spec test_record3_Latent(#car{}) -> any().
test_record3_Latent({_, _}) -> ok.

-spec test_record4_Latent(#car{}) -> any().
test_record4_Latent({_, _, _, _}) -> ok.

-spec test_record5_Ok(#car{}) -> any().
test_record5_Ok({car, _, _}) -> ok.

-spec test_record6_Bad(#car{}) -> any().
test_record6_Bad({not_a_car, _, _}) -> ok.

% Just to check that we don't crash
-spec test_no_args_Ok() -> any().
test_no_args_Ok() -> 1 = 1.

-spec test_no_args_Bad() -> any().
test_no_args_Bad() -> 1 = 2.

-spec test_integer1_Ok(integer()) -> any().
test_integer1_Ok(X) when is_integer(X) -> ok.

test_integer2_Latent(X) when is_integer(X) -> ok.
