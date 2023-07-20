% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(reach).
-export([
    test_a_Ok/0,
    test_b_Ok/0,
    test_c_Bad/0,
    fpl_test_d_Ok/0,
    test_e_Ok/0,
    fnl_test_e_Bad/0,
    fnl_test_f_Bad/0,
    test_g_Ok/0,
    test_g_Bad/0,
    test_h_Bad/0
]).

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

% Test below is fpl due to being reported as latent
% Result from second call to source() is ignored so
% there should be no error reported.
fpl_test_d_Ok() ->
    X = source(),
    _ = source(),
    sink([X, X]).

test_e_Ok() ->
    X = source(),
    sink(get_map(X, X)).

% False negative (latent) is expected given that TOPL currently
% partially supports maps by keeping track of a single kv pair.
fnl_test_e_Bad() ->
    X = source(),
    Y = source(),
    sink(get_map(X, Y)).

fnl_test_f_Bad() ->
    X = source(),
    Y = source(),
    sink(#{"X" => X, "Y" => Y}).

test_g_Ok() ->
    X = source(),
    sink({X, X}).

test_g_Bad() ->
    X = source(),
    Y = source(),
    sink({X, Y}).

test_h_Bad() ->
    X = source(),
    Y = source(),
    sink(get_tuple(X, Y)).

%%
source() -> dirty.
% This should be something that crashes runtime (for our compiler tests),
% but is not reported by Pulse (so that we get TOPL error in TOPL tests).
sink(dirty) -> erlang:error(taint_error);
sink(_) -> ok.

get_map(X, Y) ->
    #{"X" => X, "Y" => Y}.

get_tuple(X, Y) ->
    {X, Y}.
