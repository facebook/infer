% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(pulse_taint_simple).
-export([
    test_taint1_Bad/0,
    test_taint2_Bad/0,
    test_taint3_Bad/0,
    test_taint4_Bad/0,
    test_taint5_Bad/0,
    test_taint6_Ok/0,
    test_taint7_Ok/0,
    test_taint8_Ok/0,
    test_taint9_Bad/0,
    test_taint10_Ok/0,
    test_taint11_Ok/0,
    test_taint12_Bad/0,
    test_taint13_Ok/0,
    test_taint14_Bad/0
]).

source() -> dirty.

conditional_source(X) when X < 0 -> ok;
conditional_source(_) -> source().

sink(_) -> oops.

indirect_sink(X) -> sink(X).

% This propagates the input but is marked explicitly as a sanitizer
sanitizer(X) -> {sanitized, X}.

id(X) -> X.

make_array(X) -> [1, X, 2].

make_tuple(X) -> {ok, X}.

does_not_propagete(_) -> hey.

taint_if_first_arg_true(true, X) -> X;
taint_if_first_arg_true(false, _) -> hey.

test_taint1_Bad() ->
    X = source(),
    sink(X).

test_taint2_Bad() ->
    X = source(),
    Y = id(X),
    sink(Y).

test_taint3_Bad() ->
    X = source(),
    Y = make_array(X),
    sink(Y).

test_taint4_Bad() ->
    X = source(),
    Y = make_tuple(X),
    sink(Y).

test_taint5_Bad() ->
    X = source(),
    Y = make_array(make_tuple(X)),
    sink(Y).

test_taint6_Ok() ->
    X = source(),
    Y = does_not_propagete(X),
    sink(Y).

test_taint7_Ok() ->
    X = source(),
    Y = sanitizer(X),
    sink(Y).

test_taint8_Ok() ->
    X = source(),
    Y = taint_if_first_arg_true(false, X),
    sink(Y).

test_taint9_Bad() ->
    X = source(),
    Y = taint_if_first_arg_true(true, X),
    sink(Y).

test_taint10_Ok() ->
    _X = source(),
    sink(not_tainted).

test_taint11_Ok() ->
    X = source(),
    X = not_tainted,
    sink(X).

test_taint12_Bad() ->
    indirect_sink(source()).

test_taint13_Ok() ->
    sink(conditional_source(-1)).

test_taint14_Bad() ->
    sink(conditional_source(1)).
