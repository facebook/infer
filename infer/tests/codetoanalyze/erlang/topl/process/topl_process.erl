% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(topl_process).

-export([
    test_1_Ok/0,
    test_2_Bad/0,
    fpl_test_3_Ok/0
]).

f() -> ok.

spawn() -> erlang:spawn(fun f/0).
message(Pid) -> Pid ! hey.
kill(Pid) -> erlang:exit(Pid, die).

test_1_Ok() ->
    P = spawn(),
    message(P),
    kill(P).

test_2_Bad() ->
    P = spawn(),
    kill(P),
    message(P),
    % This is to make it crash during runtime
    erlang:error(expected_crash).

% Test below is fp (reported as LATENT error)
fpl_test_3_Ok() ->
    P1 = spawn(),
    P2 = spawn(),
    message(P1),
    kill(P1),
    message(P2),
    kill(P2).
