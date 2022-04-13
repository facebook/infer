% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(specs_remote).

-export([
    remote2_Latent/1,
    remote1_Ok/1
]).

-spec remote1_Ok(specs_userdefined:my_atom_type()) -> any().
remote1_Ok(X) when is_atom(X) -> X.

remote2_Latent(X) when is_atom(X) -> X.
