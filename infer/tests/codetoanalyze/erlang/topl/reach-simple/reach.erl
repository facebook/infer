% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(reach).
-export([
    test_a_Bad/0,
    test_b_Bad/0,
    test_c_Bad/0,
    test_d_Bad/0,
    test_e_Bad/0,
    test_f_Bad/0,
    test_g_Bad/0,
    fnl_test_h_Bad/0,
    fnl_test_i_Bad/0,
    test_j_Bad/0,
    test_k_Bad/0
]).

test_a_Bad() ->
    sink(source()).

test_b_Bad() ->
    sink(id(source())).

test_c_Bad() ->
    sink({source()}).

test_d_Bad() ->
    sink(id({source()})).

test_e_Bad() ->
    sink({id({source()})}).

test_f_Bad() ->
    indirect_sink(source()).

test_g_Bad() ->
    indirect_sink({source()}).

% T142413251
fnl_test_h_Bad() ->
    indirect_wrapping_sink(source()).

% T142413251
fnl_test_i_Bad() ->
    indirect_wrapping_sink({source()}).

test_j_Bad() ->
    {X} = getD(),
    {Y} = id(X),
    Z = [Y, Y],
    go(Z).

test_k_Bad() ->
    X = source(),
    sink(#{"X" => X}).

go(Z) ->
    gogo({[], Z}).

gogo(X) ->
    {_, Y} = X,
    sink(Y).

indirect_wrapping_sink(X) -> sink({X}).
indirect_sink(X) -> sink(X).
id(X) -> X.
getD() -> {{source()}}.

%%
source() -> dirty.
% This should be something that crashes runtime (for our compiler tests),
% but is not reported by Pulse (so that we get TOPL error in TOPL tests).
sink(dirty) -> erlang:error(taint_error);
sink(_) -> ok.
