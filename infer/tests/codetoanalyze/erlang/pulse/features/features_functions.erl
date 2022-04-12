% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_functions).
-export([
    test_override_Bad/0,
    test_override_Ok/0,
    test_call1_Ok/0,
    test_call1_Bad/0,
    test_call2_Ok/0,
    test_call2_Bad/0,
    test_call3_Ok/0,
    test_call3_Bad/0,
    test_call4_Ok/0,
    test_call4_Bad/0
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

f(1) -> 1;
f(_) -> 0.

first(X, _) -> X.

second(_, Y) -> Y.

test_call1_Ok() ->
    X = 5,
    Y = f(X),
    case Y of
        0 -> ok
    end.

test_call1_Bad() ->
    X = 5,
    Y = f(X),
    case Y of
        0 -> warn(1)
    end.

test_call2_Ok() ->
    X = 1,
    Y = f(X),
    case Y of
        1 -> ok
    end.

test_call2_Bad() ->
    X = 1,
    Y = f(X),
    case Y of
        1 -> warn(1)
    end.

test_call3_Ok() ->
    X = first(1, 2),
    case X of
        1 -> ok
    end.

test_call3_Bad() ->
    X = first(1, 2),
    case X of
        1 -> warn(1)
    end.

test_call4_Ok() ->
    X = second(1, 2),
    case X of
        2 -> ok
    end.

test_call4_Bad() ->
    X = second(1, 2),
    case X of
        2 -> warn(1)
    end.

% We check that this overrides erlang:is_map
is_map(_) -> true.

test_override_Ok() ->
    case is_map(1) of
        true -> ok;
        _ -> warn(1)
    end.

test_override_Bad() ->
    case is_map(1) of
        true -> warn(1);
        _ -> ok
    end.
