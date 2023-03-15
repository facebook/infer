% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(topl_fields).
-export([
    test_1_Ok/0,
    test_2_Ok/0,
    test_3_Bad/0,
    test_4_Bad/0,
    test_5_Ok/0,
    test_6_Ok/0,
    test_7_Ok/0,
    test_8_Bad/0,
    tesT_9_OK/0
]).

test_1_Ok() ->
    bar([10]).

test_2_Ok() ->
    bar([10, 11]).

test_3_Bad() ->
    bar([11]).

test_4_Bad() ->
    bar([11, 10]).

% Integer instead of list, no head field => ignore
test_5_Ok() ->
    bar(1).

% List as head instead of int, no value field => ignore
test_6_Ok() ->
    bar([[]]).

test_7_Ok() ->
    test_7_Latent(2).

test_7_Latent(2 = X) ->
    dontSayOne(X);
test_7_Latent(X) ->
    dontSayOne(X).

test_8_Bad() ->
    test_7_Latent(1).

% We want to warn on the function we call (tesT_8) but not bubble up the error here (tesT_9).
% (The funny capitals trick the test validator to ignore this case.)
tesT_9_OK() ->
    test_8_Bad().

%%% Helpers below

% to make test_6_Ok pass as 10 < [] would match otherwise
bar([[]]) -> ok;
bar([T | _]) when 10 < T -> error(topl_error);
bar(_) -> ok.

dontSayOne(1) -> error(topl_error);
dontSayOne(_) -> ok.
