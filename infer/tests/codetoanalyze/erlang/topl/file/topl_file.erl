% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(topl_file).

-export([
    good/1,
    bad/1
]).

good(F) -> nop(F), file:write(F, "hi").
bad(F)  -> op(F), file:write(F, "hi").

nop(_)  -> ok.
op(F)   -> file:close(F).
