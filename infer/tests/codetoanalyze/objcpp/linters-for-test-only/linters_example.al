DEFINE-CHECKER TEST_REFERENCE = {
  SET report_when =
          WHEN method_return_type("instancetype")
          AND HOLDS-NEXT WITH-TRANSITION Parameters
          (has_type("MyClass") OR has_type("MyClass &"))
          AND declaration_has_name(REGEXP("^new.*:$"))
          HOLDS-IN-NODE ObjCMethodDecl;
  SET message = "Found reference in parameter of method new";
};
