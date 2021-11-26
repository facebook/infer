% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_atoms).

-export([
    test_match1_Ok/0,
    test_match2_Ok/0,
    test_match3_Bad/0
]).

matches_ok(ok) -> ok.

test_match1_Ok() -> matches_ok(ok).

test_match2_Ok() -> matches_ok('ok').

test_match3_Bad() -> matches_ok(not_ok).
