// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.
DEFINE-CHECKER CALL_FUNCTION = {
  SET report_when =
    WHEN call_qualified_function("anamespace::the_function") OR call_function("the_other_function")
    HOLDS-IN-NODE CallExpr;

  SET message = "the_function and the_other_function are not to be called";
  SET suggestion = "Call something else.";
  SET severity = "WARNING";
  SET mode = "ON";

};
