#IMPORT <library.al>

GLOBAL-MACROS {

  LET global_is_subclass_of(x) =
        is_class(x) HOLDS-IN-SOME-SUPERCLASS-OF  ObjCInterfaceDecl;

  LET parameter_of_type(x) =
        WHEN
          has_type(x)
        HOLDS-IN-NODE ParmVarDecl;

  LET method_has_a_parameter_with_type(x) =
        WHEN
          HOLDS-NEXT WITH-TRANSITION Parameters
                (has_type(x))
        HOLDS-IN-NODE ObjCMethodDecl;

  LET method_has_at_least_a_parameter =
      WHEN
         HOLDS-NEXT WITH-TRANSITION Parameters
               (TRUE)
       HOLDS-IN-NODE ObjCMethodDecl;

   LET method_has_all_parameter_with_type(x) =
        WHEN
          HOLDS-EVERYWHERE-NEXT WITH-TRANSITION Parameters
           (has_type(x))
        HOLDS-IN-NODE ObjCMethodDecl;
 };


 GLOBAL-PATHS {
 	LET filtered_files = {REGEXP("codetoanalyze/objc/linters-for-test-only/filter_by_path/.*") };
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
          OR method_return_type("A*")
          OR method_return_type("REGEXP('This.+')*" )
        HOLDS-IN-NODE ObjCMethodDecl;

  SET message = "Method return.....";
};

DEFINE-CHECKER TEST_IMPLICIT_CAST_CHECK = {

  LET has_type_long_expr = has_type("long") HOLDS-EVENTUALLY;

  SET report_when =
        WHEN
            has_type("int") AND has_type_long_expr
	    HOLDS-IN-NODE ImplicitCastExpr;

  SET message = "An implicit cast from long to int can cause a crash";
};

DEFINE-CHECKER TEST_VAR_TYPE_CHECK = {

  SET report_when =
        WHEN has_type("int") OR has_type("long")
	    HOLDS-IN-NODE VarDecl;

  SET message = "Var has type int or long";
};

DEFINE-CHECKER TEST_TYPEDEF_CHECK = {

  SET report_when =
        WHEN
        has_type("my_ulong") OR
        has_type("my_pS") OR
        has_type("my_listNode")
	    HOLDS-IN-NODE VarDecl;

  SET message = "Var has type....";
};

DEFINE-CHECKER TEST_PARAM_TYPE_CHECK = {

  SET report_when =
           method_has_a_parameter_with_type("REGEXP('This.+')*" );

  SET message = "Found a method with a parameter of type....";

};

DEFINE-CHECKER TEST_PARAM_TYPE_CHECK2 = {

  SET report_when = method_has_at_least_a_parameter AND
                    method_has_all_parameter_with_type("int");

  SET message = "Found a method with a parameter of type....";

};

DEFINE-CHECKER TEST_NTH_PARAM_TYPE_CHECK = {

  SET report_when =
    WHEN
      objc_method_has_nth_parameter_of_type("2", "REGEXP('This.+')*")
    HOLDS-IN-NODE ObjCMethodDecl;

  SET message = "Found a method with nth parameter of type....";
  SET severity = "LIKE";
};

DEFINE-CHECKER TEST_PROTOCOL_DEF_INHERITANCE = {

  LET is_subprotocol_of(x) =
          declaration_has_name(x) HOLDS-EVENTUALLY WITH-TRANSITION Protocol;

  SET report_when =
    WHEN
      is_subprotocol_of("A")
    HOLDS-IN-NODE ObjCProtocolDecl;

  SET message = "Protocol inherit from A";
};

DEFINE-CHECKER TEST_PROTOCOL_TYPE_INHERITANCE = {

  LET method_has_parameter_subprotocol_of(x) =
            WHEN
             HOLDS-NEXT WITH-TRANSITION Parameters
                (has_type_subprotocol_of(x))
             HOLDS-IN-NODE ObjCMethodDecl;

  SET report_when =
      WHEN
        declaration_has_name(REGEXP("^newWith.*:$")) AND
        method_has_parameter_subprotocol_of("A")
      HOLDS-IN-NODE ObjCMethodDecl;

  SET message = "Method declared with parameter whose type inherit from protocol A";
};

DEFINE-CHECKER TEST_GENERICS_TYPE = {

  LET method_has_parameter_type(x) =
        WHEN
          HOLDS-NEXT WITH-TRANSITION Parameters
            (has_type(x))
          HOLDS-IN-NODE ObjCMethodDecl;

  SET report_when =
      WHEN
         method_has_parameter_type ("NSArray<id<C>>*")
      HOLDS-IN-NODE ObjCMethodDecl;

  SET message = "Method declared with parameter whose type NSArray<id<C>>*";
};


DEFINE-CHECKER TEST_INSTANCE_TYPE = {

  SET report_when =
      WHEN
        has_type("instancetype")
      HOLDS-IN-NODE ObjCMethodDecl;

  SET message = "Method declared has return type instancetype";
};

DEFINE-CHECKER TEST_DEFINE_NAMESPACE = {

  SET report_when =
    WHEN
      declaration_has_name(REGEXP("FirstNam*"))
    HOLDS-IN-NODE NamespaceDecl;

  SET message = "Found a namespace with name....";

};

DEFINE-CHECKER TEST_USING_NAMESPACE = {

  SET report_when =
      using_namespace(REGEXP("FirstNam*"));

  SET message = "Found using namespace with name....";

};

DEFINE-CHECKER FILTER_BY_PATH_EXAMPLE = {
  SET report_when =
     WHEN declaration_has_name("main")
     HOLDS-IN-NODE FunctionDecl;
  SET message = "Found main method";
  SET whitelist_path = { filtered_files, "A.m" };
};

DEFINE-CHECKER ALL_PATH_NO_FILTER_EXAMPLE = {
  SET report_when =
     WHEN declaration_has_name("main")
     HOLDS-IN-NODE FunctionDecl;
  SET message = "Found main method";
};

DEFINE-CHECKER FILTER_BY_ALL_PATH_EXAMPLE = {
  SET report_when =
     WHEN declaration_has_name("main")
     HOLDS-IN-NODE FunctionDecl;
  SET message = "Found main method";
  SET whitelist_path = { REGEXP(".*") };
};

DEFINE-CHECKER BLACKLIST_PATH_EXAMPLE = {
  SET report_when =
     WHEN declaration_has_name("main")
     HOLDS-IN-NODE FunctionDecl;
  SET message = "Found main method";
  SET blacklist_path = { REGEXP("codetoanalyze/objc/linters-for-test-only/filter_by_path/.*") };
};

DEFINE-CHECKER WHITE_BLACKLIST_PATH_EXAMPLE = {
  SET report_when =
     WHEN declaration_has_name("main")
     HOLDS-IN-NODE FunctionDecl;
  SET message = "Found main method";
  SET whitelist_path = { all_files };
  SET blacklist_path = { filtered_files };
  SET doc_url = "www.example.com";
};

DEFINE-CHECKER ENUM_CONSTANTS = {
  SET report_when = is_enum_constant(REGEXP("MyName.*")) OR is_enum_constant("IMMEDIATE");
  SET message = "Do not use the enum MyName or strategy";
};

 DEFINE-CHECKER TEST_SELECTOR = {
   SET report_when = is_at_selector_with_name("actionButtonTapped:");
   SET message = "Found @selector(actionButtonTapped:)";
 };

 DEFINE-CHECKER TEST_DEFAULT_VISIBILITY = {
   SET report_when =
       WHEN has_visibility_attribute("Default")
       HOLDS-IN-NODE CallExpr;
   SET message = "%name% has default visibility";
 };

 DEFINE-CHECKER TEST_HIDDEN_VISIBILITY = {
   SET report_when =
       WHEN has_visibility_attribute("Hidden")
       HOLDS-IN-NODE CallExpr;
   SET message = "%name% has hidden visibility";
 };

 DEFINE-CHECKER TEST_USED_ATTRIBUTE = {
   SET report_when =
       WHEN has_used_attribute()
       HOLDS-IN-NODE CallExpr;
   SET message = "%name% has used attribute";
 };

 DEFINE-CHECKER TEST_DEFAULT_VISIBILITY_WITH_USED_ATTRIBUTE = {
   SET report_when =
       WHEN has_visibility_attribute("Default") AND has_used_attribute
       HOLDS-IN-NODE CallExpr;
   SET message = "%name% has default visibility and used attribute";
 };
