% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(reach).
-export([test_a_Ok/0, test_b_Ok/0, test_c_Bad/0, fp_test_d_Ok/0]).

test_a_Ok() ->
    _ = source(),
    sink([]).

test_b_Ok() ->
    X = source(),
    sink([X]).

test_c_Bad() ->
    X = source(),
    Y = source(),
    sink([X, Y]).

fp_test_d_Ok() ->
    X = source(),
    _ = source(),
    sink([X, X]).

%%
source() -> dirty.
% This should be something that crashes runtime (for our compiler tests),
% but is not reported by Pulse (so that we get TOPL error in TOPL tests).
sink(dirty) -> erlang:error(taint_error);
sink(_) -> ok.
