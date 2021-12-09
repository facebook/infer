% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_case_guards).

-export([
    test_accepts_positive_Bad/0,
    test_accepts_positive_Ok/0,
    test_accepts_positive2_Bad/0,
    test_accepts_positive2_Ok/0,
    test_accepts_all_Ok/0
]).

accepts_positive(X) ->
    case X of
        X when X > 0 -> ok
    end.

accepts_positive2(X) ->
    case X of
        X when 1 =:= 1, 1 =:= 0; X > 0 -> ok
    end.

accepts_all(X) ->
    case X of
        X when X > 0 -> ok;
        X when not (X > 0) -> ok
    end.

test_accepts_positive_Bad() ->
    accepts_positive(0).

test_accepts_positive_Ok() ->
    accepts_positive(1).

test_accepts_positive2_Bad() ->
    accepts_positive2(0).

test_accepts_positive2_Ok() ->
    accepts_positive2(1).

test_accepts_all_Ok() ->
    accepts_all(0),
    accepts_all(1).
