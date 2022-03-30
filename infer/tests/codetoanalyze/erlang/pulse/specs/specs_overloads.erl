% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(specs_overloads).

-spec overload1_Ok
    (atom(), nil()) -> any();
    (map(), nil()) -> any().
overload1_Ok(X, []) when is_atom(X) -> ok;
overload1_Ok(X, []) when is_map(X) -> ok.

overload2_Latent(X, []) when is_atom(X) -> ok;
overload2_Latent(X, []) when is_map(X) -> ok.

-spec overload3_Latent
    (atom(), nil()) -> any();
    (map(), nil()) -> any().
overload3_Latent(X, []) when is_atom(X) -> ok.

-spec overload4_Latent
    (atom(), nil()) -> any();
    (map(), nil()) -> any().
overload4_Latent(X, []) when is_map(X) -> ok.

-spec combinations1_Ok
    (atom(), atom()) -> any();
    (nil(), nil()) -> any().
combinations1_Ok([], []) -> ok;
combinations1_Ok(X, Y) when is_atom(X), is_atom(Y) -> ok.

% This is to make sure we have a disjunction (overloads) of conjunctions (arguments)
% and not the other way around
-spec combinations2_Latent
    (atom(), atom()) -> any();
    (nil(), nil()) -> any().
combinations2_Latent([], Y) when is_atom(Y) -> ok;
combinations2_Latent(X, []) when is_atom(X) -> ok.
