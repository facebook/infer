% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(lambdas).

% Workaround until we support true/false atoms
-define(True, (1 == 1)).

-export([
    test_lambda_within_function_Ok/0,
    test_lambda_within_function_Bad/0,
    test_lambda_within_function_nested_Ok/0,
    test_lambda_within_function_nested_Bad/0,
    fp_test_apply_fun_Ok/0,
    test_apply_fun_Bad/0,
    fp_test_lambda_capture_Ok/0,
    test_lambda_capture_Bad/0,
    test_scopes_Ok/1
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test_lambda_within_function_Ok() ->
    F = fun(X) -> X + 1 end,
    Y = F(1),
    case Y of
        2 -> ok;
        _ -> warn(1)
    end.

test_lambda_within_function_Bad() ->
    F = fun(X) -> X + 1 end,
    Y = F(1),
    case Y of
        2 -> warn(1);
        _ -> ok
    end.

test_lambda_within_function_nested_Ok() ->
    F = fun(X) ->
        G = fun(Y) -> Y + 1 end,
        G(X)
    end,
    Y = F(1),
    case Y of
        2 -> ok;
        _ -> warn(1)
    end.

test_lambda_within_function_nested_Bad() ->
    F = fun(X) ->
        G = fun(Y) -> Y + 1 end,
        G(X)
    end,
    Y = F(1),
    case Y of
        2 -> warn(1);
        _ -> ok
    end.

apply_fun(F, X) ->
    F(X).

% TODO: T104352372
fp_test_apply_fun_Ok() ->
    Y = apply_fun(fun(X) -> X + 1 end, 1),
    case Y of
        2 -> ok;
        _ -> warn(1)
    end.

test_apply_fun_Bad() ->
    Y = apply_fun(fun(X) -> X + 1 end, 1),
    case Y of
        2 -> warn(1);
        _ -> ok
    end.

% TODO: T104353993
fp_test_lambda_capture_Ok() ->
    N = 5,
    F = fun() -> N + 1 end,
    Y = F(),
    case Y of
        6 -> ok;
        _ -> warn(1)
    end.

test_lambda_capture_Bad() ->
    N = 5,
    F = fun() -> N + 1 end,
    Y = F(),
    case Y of
        6 -> warn(1);
        _ -> ok
    end.

test_scopes_Ok(1) ->
    X = 1;
test_scopes_Ok(_) ->
    % The X in the lambda is local to the lambda, despite
    % having seen an X in the previous function clause
    F = fun() -> X = 2 end,
    F(),
    X = 3.
