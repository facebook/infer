% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(block).

-export([
    test_block_Ok/0,
    test_block_Bad/0,
    test_block_in_function_Ok/0,
    test_block_in_function_Bad/0
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test_block_Ok() ->
    case
        begin
            X = 3,
            Y = X,
            Z = Y,
            Z
        end
    of
        3 -> ok;
        _ -> warn(1)
    end.

test_block_Bad() ->
    case
        begin
            X = 3,
            Y = X,
            Z = Y,
            Z
        end
    of
        3 -> warn(1);
        _ -> ok
    end.

block_in_function() ->
    begin
        1,
        2,
        3
    end.

test_block_in_function_Ok() ->
    X = block_in_function(),
    case X of
        3 -> ok;
        _ -> warn(1)
    end.

test_block_in_function_Bad() ->
    X = block_in_function(),
    case X of
        3 -> warn(1);
        _ -> ok
    end.
