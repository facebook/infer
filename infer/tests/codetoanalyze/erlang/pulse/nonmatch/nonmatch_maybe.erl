% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_maybe).
% Won't be needed from OTP 27
-feature(maybe_expr, enable).

-export([
    test_maybe1_Ok/0,
    test_maybe2_Ok/0,
    test_maybe3_Ok/0,
    test_maybe4_Bad/0
]).

test_maybe1_Ok() ->
    maybe
        X ?= 1,
        X
    else
        2 -> ok
    end.

test_maybe2_Ok() ->
    maybe
        1 ?= 2
    else
        2 -> ok
    end.

test_maybe3_Ok() ->
    maybe
        1 ?= 2
    else
        1 -> ok;
        2 -> ok
    end.

test_maybe4_Bad() ->
    maybe
        1 ?= 2
    else
        1 -> ok
    end.
