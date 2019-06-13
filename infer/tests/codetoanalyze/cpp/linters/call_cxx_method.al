// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.
DEFINE-CHECKER CALL_CXX_METHOD = {
  SET report_when =
    WHEN call_cxx_method("fooBar") OR call_cxx_method("bar")
    HOLDS-IN-NODE CXXMemberCallExpr;

  SET message = "Do not call fooBar or bar.";
  SET suggestion = "Call something else.";
  SET severity = "WARNING";
  SET mode = "OFF";

};
