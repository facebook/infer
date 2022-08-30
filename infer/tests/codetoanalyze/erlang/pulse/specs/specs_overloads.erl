% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(specs_overloads).

-export([
    test_combinations2_Latent/2,
    test_combinations1_Ok/2,
    test_overload4_Latent/2,
    test_overload3_Latent/2,
    test_overload2_Latent/2,
    test_overload1_Ok/2
]).

-spec test_overload1_Ok
    (atom(), nil()) -> any();
    (map(), nil()) -> any().
test_overload1_Ok(X, []) when is_atom(X) -> ok;
test_overload1_Ok(X, []) when is_map(X) -> ok.

test_overload2_Latent(X, []) when is_atom(X) -> ok;
test_overload2_Latent(X, []) when is_map(X) -> ok.

-spec test_overload3_Latent
    (atom(), nil()) -> any();
    (map(), nil()) -> any().
test_overload3_Latent(X, []) when is_atom(X) -> ok.

-spec test_overload4_Latent
    (atom(), nil()) -> any();
    (map(), nil()) -> any().
test_overload4_Latent(X, []) when is_map(X) -> ok.

-spec test_combinations1_Ok
    (atom(), atom()) -> any();
    (nil(), nil()) -> any().
test_combinations1_Ok([], []) -> ok;
test_combinations1_Ok(X, Y) when is_atom(X), is_atom(Y) -> ok.

% This is to make sure we have a disjunction (overloads) of conjunctions (arguments)
% and not the other way around
-spec test_combinations2_Latent
    (atom(), atom()) -> any();
    (nil(), nil()) -> any().
test_combinations2_Latent([], Y) when is_atom(Y) -> ok;
test_combinations2_Latent(X, []) when is_atom(X) -> ok.
