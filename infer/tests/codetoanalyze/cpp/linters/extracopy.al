// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.
DEFINE-CHECKER EXTRA_COPY = {
  LET name_not_contains_copy = NOT declaration_has_name(REGEXP("[cC]opy"));

  LET is_local_var = NOT is_global_var AND NOT is_static_local_var;

  LET is_copy_contructor =
    HOLDS-NEXT WITH-TRANSITION InitExpr
    (is_node("CXXConstructExpr") AND is_cxx_copy_constructor);

  SET report_when  =
    WHEN name_not_contains_copy AND is_local_var AND is_copy_contructor
    HOLDS-IN-NODE VarDecl;

  SET message = "Potentially unnecessary to copy var %name%";
  SET severity = "WARNING";
  SET mode = "ON";
  //SET whitelist_path = {
  //  REGEXP("admarket/.*"),
  //  REGEXP("multifeed/.*")
  //};

};

DEFINE-CHECKER NAMESPACE_STRING = {

  SET report_when =
    WHEN has_type("REGEXP('string')")
    HOLDS-IN-NODE VarDecl;

  SET message = "Found type strings with namespace";
  SET mode = "ON";

};

DEFINE-CHECKER ITERATOR = {

  SET report_when =
    WHEN has_type("REGEXP('iterator')")
    HOLDS-IN-NODE VarDecl;

  SET message = "Found type iterator";
  SET mode = "ON";

};


DEFINE-CHECKER CONSTANT_EXPR = {

  LET eventually_const_sub_expr =
        HOLDS-NEXT WITH-TRANSITION InitExpr
            (has_init_list_const_expr HOLDS-EVENTUALLY);

  LET not_static_and_not_global = NOT (is_global_var OR is_static_local_var);

  SET report_when =
    WHEN
        not_static_and_not_global AND  (is_init_expr_cxx11_constant() OR eventually_const_sub_expr)
    HOLDS-IN-NODE VarDecl;

  SET message = "Found constant expression";
  SET mode = "ON";

};
