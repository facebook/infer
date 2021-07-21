% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(short_circuit).

% Workaround until we support true/false atoms
-define(T, (1 == 1)).
-define(F, (1 == 0)).

-export([
    test_and_Ok/0,
    test_and_Bad/0,
    test_andalso_Ok/0,
    test_andalso_Bad/0,
    test_or_Ok/0,
    test_or_Bad/0,
    test_orelse_Ok/0,
    test_orelse_Bad/0
]).

accepts_one(1) ->
    true.

test_and_Ok() ->
    % All fine here
    ?T and accepts_one(1).

test_and_Bad() ->
    % Fails because no short circuit
    ?F and accepts_one(0).

test_andalso_Ok() ->
    % Ok because short circuit
    ?F andalso accepts_one(0).

test_andalso_Bad() ->
    % Fails because LHS comes first
    accepts_one(0) andalso ?F.

test_or_Ok() ->
    % All fine here
    ?F or accepts_one(1).

test_or_Bad() ->
    % Fails because no short circuit
    ?T or accepts_one(0).

test_orelse_Ok() ->
    % Ok because short circuit
    ?T orelse accepts_one(0).

test_orelse_Bad() ->
    % Fails because LHS comes first
    accepts_one(0) orelse ?T.
