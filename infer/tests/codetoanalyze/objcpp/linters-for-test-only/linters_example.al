DEFINE-CHECKER TEST_REFERENCE = {
  SET report_when =
          WHEN method_return_type("instancetype")
          AND HOLDS-NEXT WITH-TRANSITION Parameters
          (has_type("MyClass") OR has_type("MyClass &"))
          AND declaration_has_name(REGEXP("^new.*:$"))
          HOLDS-IN-NODE ObjCMethodDecl;
  SET message = "Found reference in parameter of method new";
};

DEFINE-CHECKER TEST_PARAMETER_LABEL = {
  LET method_has_parameter_type =
        WHEN
          HOLDS-NEXT WITH-TRANSITION ParameterName "number"
            (has_type("int"))
          HOLDS-IN-NODE ObjCMessageExpr;
  SET report_when =
      WHEN
         method_has_parameter_type
         AND call_method(REGEXP("^new.*:$"))
      HOLDS-IN-NODE ObjCMessageExpr;
  SET message = "Found method with parameter labeled with `number` and with type `int`";
};

DEFINE-CHECKER TEST_PARAMETER_LABEL_REGEXP = {
  LET method_has_parameter_type =
        WHEN
          HOLDS-NEXT WITH-TRANSITION ParameterName REGEXP("num.*")
            (has_type("int"))
          HOLDS-IN-NODE ObjCMessageExpr;
  SET report_when =
      WHEN
         method_has_parameter_type
         AND call_method(REGEXP("^new.*:$"))
      HOLDS-IN-NODE ObjCMessageExpr;
  SET message = "Found method with parameter labeled with `number` and with type `int`";
};
