
// Check that class A is not subclassed.
DEFINE-CHECKER SUBCLASSING_TEST_EXAMPLE = {

   SET report_when =
      is_class(A) HOLDS-IN-SOME-SUPERCLASS-OF ObjCInterfaceDecl;

    SET message = "This is subclassing A. Class A should not be subclassed.";

};
