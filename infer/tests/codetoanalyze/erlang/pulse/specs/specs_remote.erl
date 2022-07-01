% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(specs_remote).

-export([
    test_remote2_Latent/1,
    test_remote1_Ok/1
]).

-spec test_remote1_Ok(specs_userdefined:my_atom_type()) -> any().
test_remote1_Ok(X) when is_atom(X) -> X.

test_remote2_Latent(X) when is_atom(X) -> X.
