% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(guard_bifs).

-export([
    test_is_map_Ok/0,
    test_is_map_Bad/0,
    test_is_map_nomodule_Ok/0,
    test_is_map_nomodule_Bad/0
]).

accepts_map(M) when erlang:is_map(M) -> ok.

test_is_map_Ok() -> accepts_map(#{}).

test_is_map_Bad() -> accepts_map([]).

accepts_map_nomodule(M) when is_map(M) -> ok.

test_is_map_nomodule_Ok() -> accepts_map_nomodule(#{}).

test_is_map_nomodule_Bad() -> accepts_map_nomodule([]).
