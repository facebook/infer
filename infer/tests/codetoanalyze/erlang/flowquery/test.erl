% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(test).
-export([
    test_aa_Bad/0,
    test_a_Bad/0,
    test_b_Ok/0,
    test_c_Ok/0,
    test_d_Bad/0,
    test_e_Ok/0,
    test_f_Bad/0,
    test_g_Bad/0,
    test_h_Ok/0,
    test_i_Bad/0,
    fn_test_j_Bad/0,
    fn_test_k_Bad/0,
    test_l_Bad/0,
    test_m_Bad/0,
    test_n_Bad/0,
    test_o_Bad/0,
    fp_test_p_Ok/0
]).

test_aa_Bad() ->
    log(get_name(bla, bla, bad), bla, bla, bla).

test_a_Bad() ->
    log({get_name(bla, bla, bad)}, bla, bla, bla).

test_b_Ok() ->
    log(get_name(bad, bad, good), bla, bla, bla).

test_c_Ok() ->
    log(bla, get_name(bla, bla, bad), bla, bla).

test_d_Bad() ->
    log(bla, bla, bla, {source()}).

test_e_Ok() ->
    log(source(), bla, bla, bla).

test_f_Bad() ->
    X = 1,
    tag(X, "private"),
    log(bla, bla, bla, X).

test_g_Bad() ->
    X = private,
    tagXlogY(X, X).

test_h_Ok() ->
    tagXlogY(private, public).

test_i_Bad() ->
    X = {1, 2},
    tagXlogY(X, X).

% TOPL won't catch this because {1,2} and {1,2} do not have the same address.
fn_test_j_Bad() ->
    tagXlogY({1, 2}, [{1, 2}]).

% TODO: we want everythin inside the tainted value to be tainted
fn_test_k_Bad() ->
    X = {1, 2},
    tag(X, "private"),
    % default propagation rule applies (to the implementation of fst)
    Y = fst(X),
    log(bla, bla, bla, Y).

test_l_Bad() ->
    X = private,
    log(bla, bla, bla, X),
    tag(X, "private").

test_m_Bad() ->
    _ = source(),
    X = source(),
    log(bla, bla, bla, X).

test_n_Bad() ->
    X = source(),
    _ = source(),
    log(bla, bla, bla, X).

test_o_Bad() ->
    X = foo,
    log(bla, bla, bla, X),
    tag(X, "private").

% T152883347
fp_test_p_Ok() ->
    X = foo,
    log(bla, bla, bla, X),
    tag(X, "public").

% helpers
get_name(_IgnoreA, _IgnoreB, bad) -> private;
get_name(_IgnoreA, _IgnoreB, _IgnoreC) -> public.
log(_IgnoreA, _IgnoreB, _IgnoreC, _IgnoreD) -> ok.
source() -> private.
tag(_Value, _Tag) -> ok.
tagXlogY(X, Y) ->
    tag(X, "private"),
    log(bla, bla, bla, Y).
fst({X, _Y}) -> X.
