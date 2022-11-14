% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(topl_atom_literal).
-export([
    test_1_Ok/0,
    test_2_Ok/0,
    test_3_Bad/0
]).

source() -> secret.

% This should be something that crashes runtime (for our compiler tests),
% but is not reported by Pulse (so that we get TOPL error in TOPL tests).
sink(not_okay, secret) -> erlang:error(taint_error);
sink(_, _) -> ok.

test_1_Ok() ->
    sink(okay, source()).

test_2_Ok() ->
    sink(not_okay, not_secret).

test_3_Bad() ->
    sink(not_okay, source()).
