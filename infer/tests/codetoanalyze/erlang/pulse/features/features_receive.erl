% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_receive).

-export([
    test_receive1_Ok/0,
    test_receive2_Bad/0,
    test_receive3_Ok/0,
    test_receive4_Bad/0,
    test_receive5_Bad/0,
    test_receive6_Bad/0,
    test_receive7_Ok/0,
    test_receive8_Bad/0,
    test_receive_bad_timeout_Bad/0
]).

test_receive1_Ok() ->
    % Avoid infinite waiting when actually run
    self() ! hello,
    X =
        receive
            _ -> 123
        end,
    123 = X.

test_receive2_Bad() ->
    % Avoid infinite waiting when actually run
    self() ! hello,
    X =
        receive
            _ -> 123
        end,
    456 = X.

test_receive3_Ok() ->
    X =
        receive
            _ -> 123
        after 0 -> 123
        end,
    123 = X.

test_receive4_Bad() ->
    X =
        receive
            _ -> 123
        after 0 -> 123
        end,
    456 = X.

test_receive5_Bad() ->
    X =
        receive
            _ -> 456
        after 1 -> 123
        end,
    456 = X.

test_receive6_Bad() ->
    % Make it crash when actually run
    self() ! hello,
    receive
        _ -> 1 = 2
    after 1 -> ok
    end.

test_receive7_Ok() ->
    % Do not warn that message might not match
    receive
        1 -> ok
    after 1 -> ok
    end.

test_receive8_Bad() ->
    % Make it crash when actually run
    self() ! hello,
    receive
        X ->
            case X of
                1 -> ok
            end
    after 1 -> ok
    end.

test_receive_bad_timeout_Bad() ->
    receive
        _ -> 123
    after 1 = 2 -> 123
    end.
