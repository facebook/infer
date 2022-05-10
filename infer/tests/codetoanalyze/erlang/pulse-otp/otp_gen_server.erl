% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(otp_gen_server).

-export([
    fp_test_cast_Ok/0,
    test_cast_Bad/0
]).

% T119301532 -- should become non-FP with an OTP update
fp_test_cast_Ok() ->
    ok = gen_server:cast(x, y).

test_cast_Bad() ->
    i_will_not_match = gen_server:cast(x, y).
