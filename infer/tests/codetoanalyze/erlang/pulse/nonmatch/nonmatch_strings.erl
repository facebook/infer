% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_strings).

-export([
    test_match1_Ok/0,
    test_match2_Bad/0,
    test_match3_Bad/0
]).

% TODO: model strings properly T93361792
fp_matches_rabbit("rabbit") -> ok.

test_match1_Ok() -> fp_matches_rabbit("rabbit").

test_match2_Bad() -> fp_matches_rabbit("not a rabbit").

test_match3_Bad() -> fp_matches_rabbit(rabbit).
