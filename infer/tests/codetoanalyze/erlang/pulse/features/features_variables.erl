% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_variables).
-export([
    test_vars1_Ok/0,
    test_vars2_Bad/0,
    test_vars3_Bad/0,
    test_vars4_Ok/0,
    test_vars5_Bad/0
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test_vars1_Ok() ->
    X = 2,
    Y = X,
    Z = 3,
    case Y of
        2 -> ok
    end,
    case Z of
        3 -> ok
    end.

test_vars2_Bad() ->
    X = 2,
    Y = X,
    Z = 3,
    case Y of
        2 -> warn(1)
    end,
    case Z of
        3 -> ok
    end.

test_vars3_Bad() ->
    X = 2,
    Y = X,
    Z = 3,
    case Y of
        2 -> ok
    end,
    case Z of
        3 -> warn(1)
    end.

test_vars4_Ok() ->
    X = 2,
    2 = X,
    ok.

test_vars5_Bad() ->
    X = 2,
    3 = X,
    ok.
