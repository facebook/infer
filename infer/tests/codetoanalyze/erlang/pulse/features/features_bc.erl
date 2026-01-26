% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_bc).

-export([
    test_strict_Ok/0,
    fn_test_strict_Bad/0
]).

test_strict_Ok() ->
    [X || <<X>> <:= <<1>>].

% We don't support bitstring generators
fn_test_strict_Bad() ->
    [X || <<X>> <:= nope].
