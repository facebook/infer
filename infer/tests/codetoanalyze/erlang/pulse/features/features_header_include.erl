% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.
-module(features_header_include).
-export([test_trivial_fail_Bad/0]).

% Test that issue gets reported for the correct file
-include_lib("features_header.hrl").
