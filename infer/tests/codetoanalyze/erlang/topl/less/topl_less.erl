% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(topl_less).
-export([test_a_Ok/0, test_b_Bad/0, test_c_Bad/0, test_d_Ok/0, test_e_Bad/0]).

test_a_Ok() ->
    bar(10).

test_b_Bad() ->
    bar(11).

test_c_Bad() ->
    bar(14).

test_d_Ok() ->
    bar(15).

test_e_Bad() ->
    bar(10 + one()).

%%% Helpers below

bar(T) when 10 < T, T < 15 -> error(topl_error);
bar(_) -> ok.

one() -> 1.
