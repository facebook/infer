#IMPORT <library.al>

GLOBAL-MACROS {

  LET global_is_subclass_of(x) =
        is_class(x) HOLDS-IN-SOME-SUPERCLASS-OF  ObjCInterfaceDecl;

};


//Check that class A is not subclassed.
DEFINE-CHECKER SUBCLASSING_TEST_EXAMPLE = {

   SET report_when =
      is_class("A") HOLDS-IN-SOME-SUPERCLASS-OF ObjCInterfaceDecl;

    SET message = "This is subclassing A. Class A should not be subclassed.";

};

DEFINE-CHECKER MACRO_TEST1 = {

  LET call_two_methods(x,y) = call_method(x) OR call_method(y);

  SET report_when = call_two_methods("foo:", "bar");

  SET message = "Error message here";

};

// Test reverse parameter of macro
DEFINE-CHECKER MACRO_TEST2 = {

  LET my_macro_to_call_method_of_class(x,y) = call_instance_method(y,x);

  SET report_when = my_macro_to_call_method_of_class("foo:", "A");

  SET message = "Error message here";

};

// Test macro call macro
DEFINE-CHECKER MACRO_TEST3 = {

  LET my_macro_to_call_method_of_class(x,y) = call_instance_method(y,x);

  LET call_my_macro(t,v) = my_macro_to_call_method_of_class(t,v);

  SET report_when = call_my_macro("foo:", "A");

  SET message = "Error message here";

};


DEFINE-CHECKER LOCAL_MACRO_SUBCLASS = {
  LET is_subclass_of(x) =
          is_class(x) HOLDS-IN-SOME-SUPERCLASS-OF ObjCInterfaceDecl;

   SET report_when = is_subclass_of("A");

   SET message = "This is subclassing A. Class A should not be subclassed.";

};

DEFINE-CHECKER GLOBAL_MACRO_SUBCLASS = {

   SET report_when = global_is_subclass_of("A");

   SET message = "This is subclassing A. Class A should not be subclassed.";

};

 DEFINE-CHECKER IMPORTED_MACRO_SUBCLASS = {

    SET report_when = imported_is_subclass_of("A");

    SET message = "This is subclassing A. Class A should not be subclassed.";

 };

DEFINE-CHECKER TEST_ALL_METHODS = {

  SET report_when = call_class_method(REGEXP(".*"), REGEXP(".*"));

  SET message = "Method call...";

};

DEFINE-CHECKER TEST_RETURN_METHOD = {

  SET report_when =
        WHEN
            method_return_type("int")
        HOLDS-IN-NODE ObjCMethodDecl;

  SET message = "Method return int";

};
