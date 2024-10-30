% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(reach).

-export([
    test_source1_Ok/0,
    test_source2_Bad/0,
    test_source3_Ok/0,
    test_source4_Bad/0, test_source5_Ok/0
]).

sink() -> ok.

not_si_nk() -> ok.

not_sink_because_arity(X) -> X.

sanitizer() -> sink().

not_sani_tizer() -> sink().

test_source1_Ok() -> not_si_nk().

test_source2_Bad() -> sink().

test_source3_Ok() -> sanitizer().

test_source4_Bad() -> not_sani_tizer().

test_source5_Ok() -> not_sink_because_arity(1).
