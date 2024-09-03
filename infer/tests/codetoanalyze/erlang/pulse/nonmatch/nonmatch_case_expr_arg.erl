% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_case_expr_arg).

-export([
    test_Ok/0,
    fn_test_Bad/0
]).

crash_if_different(A, B) ->
    % The matching of A against the bound B should be compiled to an equality check.
    case A of
        B -> ok
    end.

test_Ok() ->
    crash_if_different(0, 0).
fn_test_Bad() ->
    crash_if_different(0, 1).
