% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_lambdas).

-export([
    test_nested_capture_Bad/0,
    test_nested_capture_Ok/0,
    nondet_lambda3_Latent/1,
    nondet_lambda2_Latent/1,
    nondet_lambda1_Ok/1,
    test_lambda_within_function_Ok/0,
    test_lambda_within_function_Bad/0,
    test_lambda_within_function_nested_Ok/0,
    test_lambda_within_function_nested_Bad/0,
    fp_test_apply_fun_Ok/0,
    test_apply_fun_Bad/0,
    test_lambda_capture_Ok/0,
    test_lambda_capture_Bad/0,
    test_scopes1_Ok/1,
    test_scopes2_Ok/0,
    test_scopes3_Bad/0,
    test_scopes4_Ok/0,
    test_nested_capture_Ok/0,
    test_nested_capture_Bad/0,
    test_no_nullptr_Ok/1
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

test_lambda_capture_Ok() ->
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

test_scopes1_Ok(1) ->
    X = 1;
test_scopes1_Ok(_) ->
    % The X in the lambda is local to the lambda, despite
    % having seen an X in the previous function clause
    F = fun() -> X = 2 end,
    F(),
    X = 3.

test_scopes2_Ok() ->
    X = 1,
    % X is captured, but matches
    F = fun () -> 1 = X end,
    F().

test_scopes3_Bad() ->
    X = 1,
    % X is captured and doesn't match
    F = fun () -> 2 = X end,
    F().

test_scopes4_Ok() ->
    % Two different X variables
    F = fun () -> X = 2 end,
    X = 1,
    F().

nondet_lambda1_Ok(X) ->
    C = 1,
    F =
        case X of
            1 -> fun() -> C end;
            _ -> fun() -> 2 end
        end,
    % We don't exactly know what F is, but it must be 1 or 2
    case F() of
        1 -> ok;
        2 -> ok
    end.

nondet_lambda2_Latent(X) ->
    C = 1,
    F =
        case X of
            1 -> fun() -> C end;
            _ -> fun() -> 2 end
        end,
    % F might return 2
    case F() of
        1 -> ok
    end.

nondet_lambda3_Latent(X) ->
    C = 1,
    F =
        case X of
            1 -> fun() -> C end;
            _ -> fun() -> 2 end
        end,
    % F might return 1 (C)
    case F() of
        2 -> ok
    end.

test_nested_capture_Ok() ->
    C = 1,
    F = fun() ->
        G = fun() -> C end,
        G()
    end,
    case F() of
        1 -> ok;
        _ -> warn(1)
    end.

test_nested_capture_Bad() ->
    C = 1,
    F = fun() ->
        G = fun() -> C end,
        G()
    end,
    case F() of
        1 -> warn(1);
        _ -> ok
    end.

test_no_nullptr_Ok(X) ->
    F = fun(_) -> X end,
    % We used to report a null pointer issue here (which is wrong)
    F(0).
