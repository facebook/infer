% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(otp_maps).

-export([
    test_intersect1_Ok/0,
    test_intersect2_Bad/0
]).

test_intersect1_Ok() ->
    M = maps:intersect(#{}, #{}),
    case M of
        #{} -> ok
    end.

test_intersect2_Bad() ->
    M = maps:intersect(#{}, #{}),
    case M of
        rabbit -> ok
    end.
