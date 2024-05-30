% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_maybe).
-include("../../common.hrl").
-feature(maybe_expr, enable).

-export([
    test_maybe_simple_Ok/0,
    test_maybe_simple_Bad/0
]).

test_maybe_simple_Ok() ->
    M = maybe
        X = 3,
        Y = X,
        Z = Y,
        Z
    end,
    ?ASSERT_EQUAL(3, M).

test_maybe_simple_Bad() ->
    M = maybe
        X = 3,
        Y = X,
        Z = Y,
        Z
    end,
    ?CRASH_IF_EQUAL(3, M).
