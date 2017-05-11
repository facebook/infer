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

DEFINE-CHECKER TEST_BUILTIN_TYPE = {

  SET report_when =
        WHEN
          method_return_type("void")
          OR method_return_type("bool")
          OR method_return_type("char")
          OR method_return_type("unsigned char")
          OR method_return_type("wchar_t")
          OR method_return_type("unsigned short")
          OR method_return_type("unsigned int")
          OR method_return_type("unsigned long")
          OR method_return_type("unsigned long long")
          OR method_return_type("__int128")
          OR method_return_type("unsigned __int128")
          OR method_return_type("signed char")
          OR method_return_type("short")
          OR method_return_type("int")
          OR method_return_type("long")
          OR method_return_type("long long")
          OR method_return_type("float")
          OR method_return_type("double")
          OR method_return_type("long double")
          OR method_return_type("id")
          OR method_return_type("Class")
          OR method_return_type("SEL")
          OR method_return_type("float *")
          OR method_return_type("unsigned int **")
        HOLDS-IN-NODE ObjCMethodDecl;

  SET message = "Method return.....";
};

DEFINE-CHECKER TEST_IMPLICIT_CAST_CHECK = {

  LET has_type_long_expr = has_type("long") HOLDS-EVENTUALLY;

  SET report_when =
        WHEN
            has_type("int") AND has_type_long_expr
	    HOLDS-IN-NODE ImplicitCastExpr;

  SET message = "An implicit case from long to int can cause a crash";
};

DEFINE-CHECKER TEST_VAR_TYPE_CHECK = {

  SET report_when =
        WHEN has_type("int") OR has_type("long")
	    HOLDS-IN-NODE VarDecl;

  SET message = "Var has type int or long";
};
