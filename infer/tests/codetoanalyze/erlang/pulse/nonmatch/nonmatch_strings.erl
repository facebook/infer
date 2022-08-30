% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_strings).

-export([
    fp_test_match1_Ok/0,
    test_match2_Bad/0,
    test_match3_Bad/0,
    fp_test_match4_Ok/0,
    test_match5_Bad/0,
    test_match6_Bad/0,
    test_match_argument_Latent/1
]).

% TODO: model strings properly T93361792
matches_rabbit("rabbit") -> ok.

fp_test_match1_Ok() -> matches_rabbit("rabbit").

test_match2_Bad() -> matches_rabbit("not a rabbit").

test_match3_Bad() -> matches_rabbit(rabbit).

matches_rabbit_concat("rab" ++ "bit") -> ok.

fp_test_match4_Ok() -> matches_rabbit_concat("rabbit").

test_match5_Bad() -> matches_rabbit_concat("not a rabbit").

test_match6_Bad() -> matches_rabbit_concat(rabbit).

test_match_argument_Latent(X) ->
    case X of "rabbit" -> ok end.
