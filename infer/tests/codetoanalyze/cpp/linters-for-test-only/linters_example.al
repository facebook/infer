// Copyright (c) 2018-present, Facebook, Inc.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

DEFINE-CHECKER FIND_CXX_COPY_CONSTRUCTOR = {

LET is_copy_constructor =
  is_node("CXXConstructExpr") AND is_cxx_copy_constructor();

SET report_when  =
  IN-NODE VarDecl WITH-TRANSITION InitExpr
    (is_copy_constructor HOLDS-EVENTUALLY )
  HOLDS-EVENTUALLY;

SET message = "Found Copy Constructor";

};

DEFINE-CHECKER FIND_STATIC_LOCAL_VAR = {

SET report_when =
  WHEN is_static_local_var
  HOLDS-IN-NODE VarDecl;

SET message = "Found static local var";

};

DEFINE-CHECKER FIND_CXX_METHOD_OVERRIDES = {
    SET report_when = is_cxx_method_overriding;
    SET message = "%decl_name% overrides";
};

DEFINE-CHECKER FIND_CXX_METHOD_OVERRIDES_MATCHING = {
    SET report_when = WHEN is_cxx_method_overriding(REGEXP("oo::.*::SvIf::future"))
        HOLDS-IN-NODE CXXMethodDecl;
    SET message = "%decl_name% overriding matching method";
};
