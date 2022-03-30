% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(specs_userdefined).

-export_type([my_atom_type/0]).

-type my_atom_type() :: atom().

-type my_union_type() :: my_atom_type() | map().

-spec basic1_Ok(my_atom_type()) -> any().
basic1_Ok(X) when is_atom(X) -> X.

basic2_Latent(X) when is_atom(X) -> X.

-spec composite1_Ok(my_union_type()) -> any().
composite1_Ok(X) when is_atom(X) -> atom;
composite1_Ok(X) when is_map(X) -> map.

composite2_Latent(X) when is_atom(X) -> atom;
composite2_Latent(X) when is_map(X) -> map.

-spec composite3_Latent(my_union_type()) -> any().
composite3_Latent(X) when is_atom(X) -> atom.

-spec composite4_Latent(my_union_type()) -> any().
composite4_Latent(X) when is_map(X) -> map.
