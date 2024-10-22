% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_binary_attr).
-some_attribute(<<"which has binary inside">>).
-export([
    test_ok_Ok/0,
    test_bad_Bad/0
]).
-include("../../common.hrl").

test_ok_Ok() -> ok.

test_bad_Bad() -> ?EXPECTED_CRASH().
