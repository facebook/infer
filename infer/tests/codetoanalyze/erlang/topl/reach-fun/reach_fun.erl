% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(reach_fun).
-export([
    test_a_Ok/0, test_b_Ok/0, test_c_Bad/0, test_d_Bad/0
]).

test_a_Ok() ->
    sink(fun () -> [source()] end).

test_b_Ok() -> sink(fun source/0).

test_c_Bad() -> sink(mk_closure(source())).

test_d_Bad() ->
    Secret = {secret, source()},
    sink(fun () -> Secret end).

mk_closure(X) -> (fun() -> X end).

source() -> dirty.

sink(F) -> F().
