% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(specs_basic).

-export([
    record6_Latent/1,
    fp_record5_Ok/1,
    record4_Latent/1,
    record3_Latent/1,
    record2_Latent/1,
    record1_Ok/1,
    tuple_with_types5_Latent/1,
    tuple_with_types4_Latent/1,
    tuple_with_types3_Ok/1,
    tuple_with_types2_Latent/1,
    tuple_with_types1_Ok/1,
    tuple_of_2_Latent/1,
    tuple_of_2_Ok/1,
    tuple_of_1_Latent/1,
    tuple_of_1_Ok/1,
    tuple_of_0_Latent/1,
    tuple_of_0_Ok/1,
    list7_Latent/1,
    list6_Latent/1,
    list5_Latent/1,
    list4_Ok/1,
    list3_Ok/1,
    list2_Ok/1,
    list1_Ok/1,
    nil3_Latent/1,
    nil2_Ok/1,
    nil1_Ok/1,
    union4_Latent/1,
    union3_Latent/1,
    union2_Latent/1,
    union1_Ok/1,
    map_Latent/1,
    map_Ok/1,
    fp_bool6_Ok/1,
    bool5_Latent/1,
    bool4_Latent/1,
    fp_bool3_Ok/1,
    bool2_Latent/1,
    bool1_Ok/1,
    atom_literal4_Latent/1,
    fp_atom_literal3_Ok/1,
    atom_literal2_Latent/1,
    atom_literal1_Ok/1,
    atom3_Latent/1,
    atom2_Latent/1,
    atom1_Ok/1,
    any_Latent/1,
    any_Ok/1,
    test_no_args_Ok/0,
    test_no_args_Bad/0,
    integer1_Ok/1,
    integer2_Latent/1
]).

-spec any_Ok(any()) -> any().
any_Ok(_) -> ok.

-spec any_Latent(any()) -> any().
any_Latent([]) -> ok.

-spec atom1_Ok(atom()) -> any().
atom1_Ok(X) when is_atom(X) -> ok.

atom2_Latent(X) when is_atom(X) -> ok.

-spec atom3_Latent(atom()) -> any().
atom3_Latent(true) -> ok;
atom3_Latent(false) -> ok.

-spec atom_literal1_Ok('ok') -> any().
atom_literal1_Ok(X) when is_atom(X) -> ok.

atom_literal2_Latent(X) when is_atom(X) -> ok.

% T115354480
-spec fp_atom_literal3_Ok('ok') -> any().
fp_atom_literal3_Ok(ok) -> ok.

atom_literal4_Latent(ok) -> ok.

-spec bool1_Ok(boolean()) -> any().
bool1_Ok(X) when is_atom(X) -> ok.

bool2_Latent(X) when is_atom(X) -> ok.

% T115354480
-spec fp_bool3_Ok(boolean()) -> any().
fp_bool3_Ok(true) -> ok;
fp_bool3_Ok(false) -> ok.

-spec bool4_Latent(boolean()) -> any().
bool4_Latent(true) -> ok.

-spec bool5_Latent(boolean()) -> any().
bool5_Latent(false) -> ok.

% T115354480
-spec fp_bool6_Ok(boolean()) -> any().
fp_bool6_Ok(X) when is_boolean(X) -> ok.

-spec map_Ok(map()) -> any().
map_Ok(X) when is_map(X) -> ok.

map_Latent(X) when is_map(X) -> ok.

-spec union1_Ok(atom() | map()) -> any().
union1_Ok(X) when is_atom(X) -> atom;
union1_Ok(X) when is_map(X) -> map.

union2_Latent(X) when is_atom(X) -> atom;
union2_Latent(X) when is_map(X) -> map.

-spec union3_Latent(atom() | map()) -> any().
union3_Latent(X) when is_atom(X) -> atom.

-spec union4_Latent(atom() | map()) -> any().
union4_Latent(X) when is_map(X) -> map.

-spec nil1_Ok([]) -> any().
nil1_Ok([]) -> ok.

-spec nil2_Ok(nil()) -> any().
nil2_Ok([]) -> ok.

nil3_Latent([]) -> ok.

-spec list1_Ok(list()) -> any().
list1_Ok([_ | _]) -> cons;
list1_Ok([]) -> nil.

-spec list2_Ok(list()) -> any().
list2_Ok(X) when is_list(X) -> ok.

-spec list3_Ok(list(any())) -> any().
list3_Ok([_ | _]) -> cons;
list3_Ok([]) -> nil.

-spec list4_Ok([any()]) -> any().
list4_Ok([_ | _]) -> cons;
list4_Ok([]) -> nil.

list5_Latent([_ | _]) -> cons;
list5_Latent([]) -> nil.

-spec list6_Latent(list()) -> any().
list6_Latent([_ | _]) -> cons.

-spec list7_Latent(list()) -> any().
list7_Latent([]) -> nil.

-spec tuple_of_0_Ok({}) -> any().
tuple_of_0_Ok({}) -> ok.

tuple_of_0_Latent({}) -> ok.

-spec tuple_of_1_Ok({any()}) -> any().
tuple_of_1_Ok({_}) -> ok.

tuple_of_1_Latent({_}) -> ok.

-spec tuple_of_2_Ok({any(), any()}) -> any().
tuple_of_2_Ok({_, _}) -> ok.

tuple_of_2_Latent({_, _}) -> ok.

-spec tuple_with_types1_Ok({atom()}) -> any().
tuple_with_types1_Ok({X}) when is_atom(X) -> ok.

tuple_with_types2_Latent({X}) when is_atom(X) -> ok.

-spec tuple_with_types3_Ok({atom(), map()}) -> any().
tuple_with_types3_Ok({X, Y}) when is_atom(X), is_map(Y) -> ok.

-spec tuple_with_types4_Latent({atom(), map()}) -> any().
tuple_with_types4_Latent({X, Y}) when is_map(X), is_atom(Y) -> ok.

-spec tuple_with_types5_Latent({any()}) -> any().
tuple_with_types5_Latent({X}) when is_atom(X) -> ok.

-record(car, {plate, owner}).

-spec record1_Ok(#car{}) -> any().
record1_Ok({_, _, _}) -> ok.

record2_Latent({_, _, _}) -> ok.

-spec record3_Latent(#car{}) -> any().
record3_Latent({_, _}) -> ok.

-spec record4_Latent(#car{}) -> any().
record4_Latent({_, _, _, _}) -> ok.

% T115354480
-spec fp_record5_Ok(#car{}) -> any().
fp_record5_Ok({car, _, _}) -> ok.

-spec record6_Latent(#car{}) -> any().
record6_Latent({not_a_car, _, _}) -> ok.

% Just to check that we don't crash
-spec test_no_args_Ok() -> any().
test_no_args_Ok() -> 1 = 1.

-spec test_no_args_Bad() -> any().
test_no_args_Bad() -> 1 = 2.

-spec integer1_Ok(integer()) -> any().
integer1_Ok(X) when is_integer(X) -> ok.

integer2_Latent(X) when is_integer(X) -> ok.
