% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_variables).
-export([
    test1_Ok/0,
    test2_Bad/0,
    test3_Bad/0,
    test4_Ok/0,
    test5_Bad/0
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test1_Ok() ->
    X = 2,
    Y = X,
    Z = 3,
    case Y of
        2 -> ok
    end,
    case Z of
        3 -> ok
    end.

test2_Bad() ->
    X = 2,
    Y = X,
    Z = 3,
    case Y of
        2 -> warn(1)
    end,
    case Z of
        3 -> ok
    end.

test3_Bad() ->
    X = 2,
    Y = X,
    Z = 3,
    case Y of
        2 -> ok
    end,
    case Z of
        3 -> warn(1)
    end.

test4_Ok() ->
    X = 2,
    2 = X,
    ok.

test5_Bad() ->
    X = 2,
    3 = X,
    ok.
