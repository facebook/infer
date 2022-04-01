% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(specs_basic).

-export([
    test_no_args_Ok/0,
    test_no_args_Bad/0
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
list1_Ok([_|_]) -> cons;
list1_Ok([]) -> nil.

-spec list2_Ok(list()) -> any().
list2_Ok(X) when is_list(X) -> ok.

-spec list3_Ok(list(any())) -> any().
list3_Ok([_|_]) -> cons;
list3_Ok([]) -> nil.

-spec list4_Ok([any()]) -> any().
list4_Ok([_|_]) -> cons;
list4_Ok([]) -> nil.

list5_Latent([_|_]) -> cons;
list5_Latent([]) -> nil.

-spec list6_Latent(list()) -> any().
list6_Latent([_|_]) -> cons.

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
