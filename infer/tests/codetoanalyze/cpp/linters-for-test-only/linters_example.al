// Copyright (c) Facebook, Inc. and its affiliates.
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

DEFINE-CHECKER FIND_STATIC_GLOBAL_VAR = {
SET report_when =
  WHEN is_static_var AND is_global_var
  HOLDS-IN-NODE VarDecl;
SET message = "Found a static global var";
};

DEFINE-CHECKER FIND_EXTERN_VAR = {
SET report_when =
  WHEN is_extern_var
  HOLDS-IN-NODE VarDecl;
SET message = "Found extern var";
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

DEFINE-CHECKER FIND_NODES_WITH_CXX_FULL_NAME = {
    SET report_when = has_cxx_fully_qualified_name(REGEXP("::Foo::.*"));
    SET message = "%cxx_fully_qualified_name% matches";
};

DEFINE-CHECKER FIND_CXX_METHODS_FROM_HEADER_FILE = {
    SET report_when = WHEN is_in_source_file(REGEXP("/test_included.h$"))
        HOLDS-IN-NODE CXXMethodDecl;
    SET message = "found C++ method %name% in %source_file%";
};

DEFINE-CHECKER FIND_LISTED_SYMBOLS = {
    SET report_when = NOT is_decl() AND has_cxx_fully_qualified_name_in_custom_symbols("test");
    SET message = "found usage of %cxx_fully_qualified_name%";
};

DEFINE-CHECKER FIND_REF_FROM_SRC_FILE = {
    SET report_when = is_referencing_decl_from_source_file(REGEXP("/test_included.h$"));
    SET message = "found ref from %ref_source_file%";
};
