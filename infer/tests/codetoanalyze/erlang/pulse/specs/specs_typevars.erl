% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(specs_typevars).

-export([
    underscore_Ok/1,
    union4_Latent/1,
    union3_Latent/1,
    union2_Latent/1,
    union1_Ok/1,
    multi3_Latent/1,
    multi2_Latent/1,
    multi1_Ok/1,
    basic3_Latent/1,
    basic2_Latent/1,
    basic1_Ok/1
]).

-spec basic1_Ok(T) -> any() when T :: atom().
basic1_Ok(X) when is_atom(X) -> ok.

basic2_Latent(X) when is_atom(X) -> ok.

-spec basic3_Latent(T) -> any() when T :: any().
basic3_Latent(X) when is_atom(X) -> ok.

-spec multi1_Ok(T) -> any() when T :: S, S :: atom().
multi1_Ok(X) when is_atom(X) -> ok.

multi2_Latent(X) when is_atom(X) -> ok.

-spec multi3_Latent(T) -> any() when T :: S, S :: any().
multi3_Latent(X) when is_atom(X) -> ok.

-spec union1_Ok(T | S) -> any() when T :: atom(), S :: map().
union1_Ok(X) when is_atom(X) -> atom;
union1_Ok(X) when is_map(X) -> map.

union2_Latent(X) when is_atom(X) -> atom;
union2_Latent(X) when is_map(X) -> map.

-spec union3_Latent(T | S) -> any() when T :: atom(), S :: map().
union3_Latent(X) when is_atom(X) -> atom.

-spec union4_Latent(T | S) -> any() when T :: atom(), S :: map().
union4_Latent(X) when is_map(X) -> map.

-spec underscore_Ok(_) -> any().
underscore_Ok(X) -> X.
