% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(specs_typevars).

-export([
    test_underscore_Ok/1,
    test_union4_Latent/1,
    test_union3_Latent/1,
    test_union2_Latent/1,
    test_union1_Ok/1,
    test_multi3_Latent/1,
    test_multi2_Latent/1,
    test_multi1_Ok/1,
    test_basic3_Latent/1,
    test_basic2_Latent/1,
    test_basic1_Ok/1
]).

-spec test_basic1_Ok(T) -> any() when T :: atom().
test_basic1_Ok(X) when is_atom(X) -> ok.

test_basic2_Latent(X) when is_atom(X) -> ok.

-spec test_basic3_Latent(T) -> any() when T :: any().
test_basic3_Latent(X) when is_atom(X) -> ok.

-spec test_multi1_Ok(T) -> any() when T :: S, S :: atom().
test_multi1_Ok(X) when is_atom(X) -> ok.

test_multi2_Latent(X) when is_atom(X) -> ok.

-spec test_multi3_Latent(T) -> any() when T :: S, S :: any().
test_multi3_Latent(X) when is_atom(X) -> ok.

-spec test_union1_Ok(T | S) -> any() when T :: atom(), S :: map().
test_union1_Ok(X) when is_atom(X) -> atom;
test_union1_Ok(X) when is_map(X) -> map.

test_union2_Latent(X) when is_atom(X) -> atom;
test_union2_Latent(X) when is_map(X) -> map.

-spec test_union3_Latent(T | S) -> any() when T :: atom(), S :: map().
test_union3_Latent(X) when is_atom(X) -> atom.

-spec test_union4_Latent(T | S) -> any() when T :: atom(), S :: map().
test_union4_Latent(X) when is_map(X) -> map.

-spec test_underscore_Ok(_) -> any().
test_underscore_Ok(X) -> X.
