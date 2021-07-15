% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(logic).

% Workaround until we support true/false atoms
-define(T, (1 == 1)).
-define(F, (1 == 0)).

-export([
    test_and00_Bad/0,
    test_and01_Bad/0,
    test_and10_Bad/0,
    test_and11_Ok/0,
    test_or00_Bad/0,
    test_or01_Ok/0,
    test_or10_Ok/0,
    test_or11_Ok/0,
    test_unot_Ok/0,
    test_unot_Bad/0
]).

test_and00_Bad() ->
    if
        ?F and ?F -> ok
    end.

test_and01_Bad() ->
    if
        ?F and ?T -> ok
    end.

test_and10_Bad() ->
    if
        ?T and ?F -> ok
    end.

test_and11_Ok() ->
    if
        ?T and ?T -> ok
    end.

test_or00_Bad() ->
    if
        ?F or ?F -> ok
    end.

test_or01_Ok() ->
    if
        ?F or ?T -> ok
    end.

test_or10_Ok() ->
    if
        ?T or ?F -> ok
    end.

test_or11_Ok() ->
    if
        ?T or ?T -> ok
    end.

test_unot_Ok() ->
    if
        not ?F -> ok
    end.

test_unot_Bad() ->
    if
        not ?T -> ok
    end.
