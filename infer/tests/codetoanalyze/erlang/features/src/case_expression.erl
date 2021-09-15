% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(case_expression).

-export([test_case_Ok/0, test_case_Bad/0]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test_case_Ok() ->
    Y = 3,
    X =
        case Y of
            3 -> 1;
            _ -> 0
        end,
    case X of
        1 -> ok;
        _ -> warn(1)
    end.

test_case_Bad() ->
    Y = 3,
    X =
        case Y of
            3 -> 1;
            _ -> 0
        end,
    case X of
        1 -> warn(1);
        _ -> ok
    end.
