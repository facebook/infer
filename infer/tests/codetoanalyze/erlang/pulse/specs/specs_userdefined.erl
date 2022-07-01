% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(specs_userdefined).

-export([
    test_composite4_Latent/1,
    test_composite3_Latent/1,
    test_composite2_Latent/1,
    test_composite1_Ok/1,
    test_basic2_Latent/1,
    test_basic1_Ok/1
]).

-export_type([my_atom_type/0]).

-type my_atom_type() :: atom().

-type my_union_type() :: my_atom_type() | map().

-spec test_basic1_Ok(my_atom_type()) -> any().
test_basic1_Ok(X) when is_atom(X) -> X.

test_basic2_Latent(X) when is_atom(X) -> X.

-spec test_composite1_Ok(my_union_type()) -> any().
test_composite1_Ok(X) when is_atom(X) -> atom;
test_composite1_Ok(X) when is_map(X) -> map.

test_composite2_Latent(X) when is_atom(X) -> atom;
test_composite2_Latent(X) when is_map(X) -> map.

-spec test_composite3_Latent(my_union_type()) -> any().
test_composite3_Latent(X) when is_atom(X) -> atom.

-spec test_composite4_Latent(my_union_type()) -> any().
test_composite4_Latent(X) when is_map(X) -> map.
