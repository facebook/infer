% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_unsupported).
-include("../../common.hrl").

-export([
    test_function1_Latent/1,
    test_function2_Latent/1,
    test_case1_Latent/1,
    test_case2_Latent/1,
    test_case3_Ok/0,
    fp_test_case4_Ok/0,
    test_case5_Bad/0,
    test_match1_Latent/1
]).

% Test that the body of a clause still gets translated even if there are unsupported
% constructs. Currently, we use binary patterns as they are not supported. But when they do
% get supported, these tests might have to be replaced. And when everything is supported
% these tests can be removed.

test_function1_Latent(<<X>>) -> ok.

test_function2_Latent(<<X>>) -> ?EXPECTED_CRASH.

test_case1_Latent(X) -> case X of <<1>> -> ?EXPECTED_CRASH end.

test_case2_Latent(X) -> case X of <<Y>> -> 1 = Y end.

test_case3_Ok() ->
    case <<>> of
        <<1>> -> ok;
        _ -> ok
    end.

% For this we need binary support
fp_test_case4_Ok() ->
    case <<>> of
        <<1>> -> ?UNEXPECTED_CRASH;
        _ -> ok
    end.

test_case5_Bad() ->
    case <<>> of
        <<1>> -> ok;
        _ -> ?EXPECTED_CRASH
    end.

test_match1_Latent(X) ->
    <<1>> = X.
