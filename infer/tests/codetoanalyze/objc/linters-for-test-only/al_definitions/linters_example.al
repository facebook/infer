// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

#IMPORT "library.al"

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

  LET my_macro_to_call_method_of_class(x,y) = call_instance_method(x) AND is_receiver_class_named(y);

  SET report_when = my_macro_to_call_method_of_class("foo:", "A");

  SET message = "Error message here";

};

// Test macro call macro
DEFINE-CHECKER MACRO_TEST3 = {

  LET my_macro_to_call_method_of_class(x,y) = call_instance_method(x) AND is_receiver_class_named(y);

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

  SET report_when = call_class_method(REGEXP(".*"));

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
          is_in_objc_class_named("TestType") AND
          (method_return_type("void")
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
          OR method_return_type("REGEXP('This.+')*" ))
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
        HOLDS-NEXT WITH-TRANSITION ParameterPos "2"
          (has_type("REGEXP('This.+')*"))
        HOLDS-IN-NODE ObjCMethodDecl;
  SET message = "Found a method with nth parameter of type....";
  SET severity = "LIKE";
};

DEFINE-CHECKER TEST_NTH_PARAM_TYPE_CHECK_FUNCTION = {
  SET report_when =
      WHEN
        HOLDS-NEXT WITH-TRANSITION ParameterPos "1"
          (has_value("2") HOLDS-EVENTUALLY)
        HOLDS-IN-NODE CallExpr;
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

DEFINE-CHECKER ENUM_CONSTANTS_OF_ENUM = {
  SET report_when = is_enum_constant_of_enum("MyName");
  SET message = "Do not use the enum MyName";
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

 DEFINE-CHECKER TEST_IN_METHOD_CONTEXT = {
   SET report_when =
       WHEN declaration_has_name("x") AND is_in_objc_method("method")
       HOLDS-IN-NODE VarDecl;
   SET message = "var %name% declared in ObjC method";
 };

 DEFINE-CHECKER TEST_IN_BLOCK_CONTEXT = {
   SET report_when =
       WHEN declaration_has_name("x") AND is_in_block()
       HOLDS-IN-NODE VarDecl;
   SET message = "var %name% declared in block";
 };

 DEFINE-CHECKER TEST_IN_FUNCTION_CONTEXT = {
   SET report_when =
       WHEN declaration_has_name("x") AND is_in_function(REGEXP("funct.*"))
       HOLDS-IN-NODE VarDecl;
   SET message = "var %name% declared in function";
 };

DEFINE-CHECKER TEST_PARAMETER_SEL_TYPE = {

 SET report_when =
     WHEN
       method_return_type("instancetype") AND
       method_has_a_parameter_with_type("SEL")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method has parameter of type SEL";

 };

 DEFINE-CHECKER IN_SUBCLASS_TEST = {
   SET report_when =
   		       WHEN is_in_objc_subclass_of("SubClassTestClass")
             HOLDS-IN-NODE ObjCMessageExpr;
   SET message = "Found a message call in a class that is not subclass of A.";
 };

 DEFINE-CHECKER TEST_IF_VIEW_METHOD_IS_NOT_CALLED_WITH_SUPER = {

  SET report_when =
      WHEN
        call_method("testView") AND
        NOT is_method_called_by_superclass()
      HOLDS-IN-NODE ObjCMessageExpr;

  SET message = "Method %name% is not called with super.";

};

DEFINE-CHECKER TEST_IF_METHOD_IS_IN_INTERFACE_NAMED = {

 SET report_when =
     WHEN
       is_in_objc_interface_named("MyBaseClass")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method %name% is in the interface named MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_METHOD_IS_IN_IMPLEMENTATION_NAMED = {

 SET report_when =
     WHEN
       is_in_objc_implementation_named("MyBaseClass")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method %name% is in the implementation named MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_METHOD_IS_IN_CLASS_NAMED = {

 SET report_when =
     WHEN
       is_in_objc_class_named("MyBaseClass")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method %name% is in the class named MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_METHOD_IS_IN_CATEGORY_INTERFACE_NAMED = {

 SET report_when =
     WHEN
       is_in_objc_category_interface_named("MyBaseClassCategory")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method %name% is in the category interface named MyBaseClassCategory.";

};

DEFINE-CHECKER TEST_IF_METHOD_IS_IN_CATEGORY_IMPLEMENTATION_NAMED = {

 SET report_when =
     WHEN
       is_in_objc_category_implementation_named("MyBaseClassCategory")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method %name% is in the category implementation named MyBaseClassCategory.";

};

DEFINE-CHECKER TEST_IF_METHOD_IS_IN_CATEGORY_NAMED = {

 SET report_when =
     WHEN
       is_in_objc_category_named("MyBaseClassCategory")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method %name% is in the category named MyBaseClassCategory.";

};

DEFINE-CHECKER TEST_IF_METHOD_IS_IN_CATEGORY_INTERFACE_ON_CLASS_NAMED = {

 SET report_when =
     WHEN
       is_in_objc_category_interface_on_class_named("MyBaseClass")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method %name% is in the category interface on a class named MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_METHOD_IS_IN_CATEGORY_IMPLEMENATION_ON_CLASS_NAMED = {

 SET report_when =
     WHEN
       is_in_objc_category_implementation_on_class_named("MyBaseClass")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method %name% is in the category implementation on a class named MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_METHOD_IS_IN_CATEGORY_ON_CLASS_NAMED = {

 SET report_when =
     WHEN
       is_in_objc_category_on_class_named("MyBaseClass")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method %name% is in the category on a class named MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_METHOD_IS_IN_CATEGORY_INTERFACE_ON_SUBCLASS_OF = {

 SET report_when =
     WHEN
       is_in_objc_category_interface_on_subclass_of("MyBaseClass")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method %name% is in the category interface on a subclass of MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_METHOD_IS_IN_CATEGORY_IMPLEMENTATION_ON_SUBCLASS_OF = {

 SET report_when =
     WHEN
       is_in_objc_category_implementation_on_subclass_of("MyBaseClass")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method %name% is in the category implementation on a subclass of MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_METHOD_IS_IN_CATEGORY_ON_SUBCLASS_OF = {

 SET report_when =
     WHEN
       is_in_objc_category_on_subclass_of("MyBaseClass")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method %name% is in the category on a subclass of MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_METHOD_IS_IN_PROTOCOL_NAMED = {

 SET report_when =
     WHEN
       is_in_objc_protocol_named("MyBaseClassProtocol")
     HOLDS-IN-NODE ObjCMethodDecl;

 SET message = "Method %name% is in the protocol named MyBaseClassProtocol.";

};

DEFINE-CHECKER TEST_IF_IS_CATEGORY_INTERFACE_NAMED = {

 SET report_when =
    is_objc_category_interface_named("MyBaseClassCategory");

 SET message = "Node is a category interface named MyBaseClassCategory.";

};

DEFINE-CHECKER TEST_IF_IS_CATEGORY_IMPLEMENTATION_NAMED = {

 SET report_when =
    is_objc_category_implementation_named("MyBaseClassCategory");

 SET message = "Node is a category implementation named MyBaseClassCategory.";

};

DEFINE-CHECKER TEST_IF_IS_CATEGORY_NAMED = {

 SET report_when =
    is_objc_category_named("MyBaseClassCategory");

 SET message = "Node is a category named MyBaseClassCategory.";

};


DEFINE-CHECKER TEST_IF_IS_CATEGORY_INTERFACE_ON_CLASS_NAMED = {

 SET report_when =
    is_objc_category_interface_on_class_named("MyBaseClass");

 SET message = "Node is a category interface on a class named MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_IS_CATEGORY_IMPLEMENTATION_ON_CLASS_NAMED = {

 SET report_when =
    is_objc_category_implementation_on_class_named("MyBaseClass");

 SET message = "Node is a category implementation on a class named MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_IS_CATEGORY_ON_CLASS_NAMED = {

 SET report_when =
    is_objc_category_on_class_named("MyBaseClass");

 SET message = "Node is a category on a class named MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_IS_CATEGORY_INTERFACE_ON_SUBCLASS_OF = {

 SET report_when =
    is_objc_category_interface_on_subclass_of("MyBaseClass");

 SET message = "Node is a category interface on a subclass of MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_IS_CATEGORY_IMPLEMENTATION_ON_SUBCLASS_OF = {

 SET report_when =
    is_objc_category_implementation_on_subclass_of("MyBaseClass");

 SET message = "Node is a category implementation on a subclass of MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_IS_CATEGORY_ON_SUBCLASS_OF = {

 SET report_when =
    is_objc_category_on_subclass_of("MyBaseClass");

 SET message = "Node is a category on a subclass of MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_IS_IMPLEMENTATION_NAMED = {

 SET report_when =
    is_objc_implementation_named("MyBaseClass");

 SET message = "Node is an implementation named MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_IS_CLASS_NAMED = {

 SET report_when =
    is_objc_class_named("MyBaseClass");

 SET message = "Node is a class named MyBaseClass.";

};

DEFINE-CHECKER TEST_IF_IS_PROTOCOL_NAMED = {

 SET report_when =
    is_objc_protocol_named("MyBaseClassProtocol");

 SET message = "Node is a protocol named MyBaseClassProtocol.";

};

DEFINE-CHECKER TEST_IF_IS_CLASS_METHOD_NAMED = {

 SET report_when =
    is_objc_class_method_named("myBaseClassClassMethod");

 SET message = "Node is a class method named myBaseClassClassMethod.";

};

DEFINE-CHECKER TEST_IF_IS_INSTANCE_METHOD_NAMED = {

 SET report_when =
    is_objc_instance_method_named("mySubclassMethod");

 SET message = "Node is an instance method named mySubclassMethod.";

};

DEFINE-CHECKER TEST_IF_IS_METHOD_NAMED = {

 SET report_when =
    is_objc_method_named("mySubclassMethod") OR
    is_objc_method_named("myBaseClassClassMethod");

 SET message = "Node is a method named mySubclassMethod or myBaseClassClassMethod.";

};

DEFINE-CHECKER TEST_IS_OVERRIDING_METHOD = {

 SET report_when =
    is_objc_method_overriding;

 SET message = "Method %name% is overriding a method in a superclass.";

};

DEFINE-CHECKER TEST_IS_METHOD_EXPOSED = {

 SET report_when =
    is_objc_method_exposed;

 SET message = "Method %name% is exposed in an interface.";

};

DEFINE-CHECKER TEST_IN_CLASS_METHOD = {

  SET report_when =
     is_in_objc_class_method("myBaseClassClassMethod");

  SET message = "This node is in a class method named myBaseClassClassMethod.";

};

DEFINE-CHECKER TEST_IN_INSTANCE_METHOD = {

  SET report_when =
     is_in_objc_instance_method("myBaseClassMethod");

  SET message = "This node is in an instance method named myBaseClassClassMethod.";

};

DEFINE-CHECKER TEST_IS_RECEIVER_CLASS_TYPE = {

  SET report_when =
     is_receiver_objc_class_type;

  SET message = "This node is a method call to 'Class'.";

};

DEFINE-CHECKER TEST_IS_RECEIVER_ID_TYPE = {

  SET report_when =
     is_receiver_objc_id_type;

  SET message = "This node is a method call to 'id'.";

};

DEFINE-CHECKER TEST_IS_RECEIVER_SUBCLASS_OF = {

  SET report_when =
     is_receiver_subclass_of("MyBaseClass");

  SET message = "This node is a method call to an object which inherits from 'MyBaseClass'.";

};

DEFINE-CHECKER TEST_IS_RECEIVER_CLASS_NAMED = {

  SET report_when =
     is_receiver_class_named("MyBaseClass");

  SET message = "This node is a method call to an object of class 'MyBaseClass'.";

};

DEFINE-CHECKER TEST_IS_RECEIVER_SUPER = {

  SET report_when =
     is_receiver_super();

  SET message = "This node is a method call to 'super'.";

};

DEFINE-CHECKER CONST_NAMING = {
  SET report_when = WHEN
        is_qual_type_const()
    HOLDS-IN-NODE VarDecl;

  SET message = "That's a const";
};

DEFINE-CHECKER UNNECESSARY_OBJC_INSTANCE_METHOD = {

  LET is_in_implementation =
    is_in_objc_implementation_named(REGEXP(".*")) OR
    is_in_objc_category_implementation_named(REGEXP(".*"));


 LET dereference_self =
      declaration_ref_name(REGEXP("self")) HOLDS-EVENTUALLY;

LET  dereference_self_in_opaque_value_expr =
          IN-NODE OpaqueValueExpr WITH-TRANSITION SourceExpr
           (dereference_self)
          HOLDS-EVENTUALLY;

LET dereference_self_in_method_decl =
           IN-NODE ObjCMethodDecl WITH-TRANSITION Body
           (dereference_self  OR dereference_self_in_opaque_value_expr)
           HOLDS-EVENTUALLY;

  LET is_private_method =
    declaration_has_name(REGEXP("^_")) AND
    NOT is_objc_method_exposed AND
    (NOT dereference_self_in_method_decl)
    AND is_in_implementation;



  SET report_when =
    WHEN
    is_private_method
    HOLDS-IN-NODE ObjCMethodDecl;


  SET name = "Unnecessary Objective-C Method";
  SET message = "This is an unnecessary Objective-C Method";
  SET severity = "ERROR";
  SET mode = "ON";
};

DEFINE-CHECKER TEST_UNAVAILABLE_ATTR = {

  SET report_when =
     has_unavailable_attribute;

  SET message = "This node has unavailable attribute";

};

DEFINE-CHECKER TEST_IS_OPTIONAL_METHOD = {

  SET report_when =
  WHEN
     is_optional_objc_method
  HOLDS-IN-NODE ObjCMethodDecl;

  SET message = "This is an optional method";

};

DEFINE-CHECKER IVAR_CAPTURED_IN_OBJC_BLOCK = {
		SET report_when =
 			WHEN
				objc_block_is_capturing_var_of_type("Ivars*")
 			HOLDS-IN-NODE BlockDecl;

	  SET message = "Found ivar of a given type captured in block";
    SET severity = "ERROR";
		SET mode = "ON";
	};
