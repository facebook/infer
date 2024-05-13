% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(stack_overflow).
-export([f_Ok/1]).

f_Ok(X) when ((X > 7) or (X < 2)) ->
    case X of
        2 -> ok
    end.
