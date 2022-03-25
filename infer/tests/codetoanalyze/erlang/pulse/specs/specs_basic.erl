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

-spec atom_Ok(atom()) -> any().
atom_Ok(X) when is_atom(X) -> ok.

atom_Latent(X) when is_atom(X) -> ok.

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

% Known limitation in Pulse
-spec fp_list2_Ok(list()) -> any().
fp_list2_Ok(X) when is_list(X) -> ok.

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

% Just to check that we don't crash
-spec test_no_args_Ok() -> any().
test_no_args_Ok() -> 1 = 1.

-spec test_no_args_Bad() -> any().
test_no_args_Bad() -> 1 = 2.
