// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// DIRECT_ATOMIC_PROPERTY_ACCESS:
// a property declared atomic should not be accessed directly via its ivar
DEFINE-CHECKER DIRECT_ATOMIC_PROPERTY_ACCESS = {

    SET report_when =
      WHEN
            is_ivar_atomic()
            AND NOT context_in_synchronized_block()
            AND NOT is_ivar_readonly()
            AND NOT is_method_property_accessor_of_ivar()
            AND NOT is_objc_constructor()
            AND NOT is_objc_dealloc()
        HOLDS-IN-NODE ObjCIvarRefExpr;

      SET message = "Direct access to ivar %ivar_name% of an atomic property";
      SET suggestion = "Accessing an ivar of an atomic property makes the property nonatomic.";
      SET severity = "WARNING";
};

// ASSIGN_POINTER_WARNING:
// a property with a pointer type should not be declared `assign`
DEFINE-CHECKER ASSIGN_POINTER_WARNING = {

     SET report_when =
        WHEN
              is_assign_property() AND is_property_pointer_type()
      HOLDS-IN-NODE ObjCPropertyDecl;

     SET message = "Property %decl_name% is a pointer type marked with the `assign` attribute";
     SET suggestion = "Use a different attribute like `strong` or `weak`.";
     SET severity = "WARNING";
};

// BAD_POINTER_COMPARISON:
// Fires whenever a NSNumber is dangerously coerced to a boolean in a comparison
DEFINE-CHECKER BAD_POINTER_COMPARISON = {

    LET bool_op =
        is_binop_with_kind("LAnd") OR is_binop_with_kind("LOr")
        OR is_unop_with_kind("LNot") OR is_unop_with_kind("LNot");

  LET comparison_with_integral =
      ( is_binop_with_kind("EQ") OR is_binop_with_kind("NE")
            OR is_binop_with_kind("GT") OR is_binop_with_kind("GE")
            OR is_binop_with_kind("LT") OR is_binop_with_kind("LE"))
        AND
        ( (is_node("ImplicitCastExpr") AND has_type("NSNumber *")
            AND has_cast_kind("IntegralToPointer")
        ) HOLDS-NEXT);

    LET root_is_stmt_expecting_bool =
        is_node("IfStmt") OR is_node("ForStmt") OR is_node("WhileStmt");

    LET use_num_as_bool =
        (bool_op OR root_is_stmt_expecting_bool) AND (has_type("NSNumber *") HOLDS-NEXT);

    LET bad_conditional =
      is_node("ConditionalOperator") AND (has_type("NSNumber *") HOLDS-NEXT WITH-TRANSITION Cond);

    SET report_when =
        use_num_as_bool OR comparison_with_integral OR bad_conditional;

  SET message = "Implicitly checking whether NSNumber pointer is nil or comparing to integral value";

    SET suggestion =
        "Did you mean to use/compare against the unboxed value instead? Please either explicitly compare the NSNumber instance to nil, or use one of the NSNumber accessors before the comparison.";
};

DEFINE-CHECKER STRONG_DELEGATE_WARNING = {

  LET name_contains_delegate = declaration_has_name(REGEXP("[dD]elegate"));
  LET name_does_not_contain_delegates =
                    NOT declaration_has_name(REGEXP("[dD]elegates"));
  LET name_does_not_contains_queue =
                    NOT declaration_has_name(REGEXP("[qQ]ueue"));

  SET report_when =
        WHEN
                name_contains_delegate AND name_does_not_contain_delegates AND name_does_not_contains_queue AND is_strong_property()
            HOLDS-IN-NODE ObjCPropertyDecl;

  SET message = "Property or ivar %decl_name% declared strong";
  SET suggestion = "In general delegates should be declared weak or assign.";

};

DEFINE-CHECKER GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL = {

  LET is_global_but_not_const_variable =
     is_objc_extension() AND is_global_var() AND (NOT is_const_expr_var()) AND (NOT is_init_integral_constant_expr());

    LET makes_an_expensive_call =
     (is_node("CallExpr") AND NOT call_function("CGPointMake"))
        OR is_node("CXXTemporaryObjectExpr")
    OR is_node("CXXMemberCallExpr")
    OR is_node("CXXOperatorCallExpr")
    OR is_node("ObjCMessageExpr");

  LET is_initialized_with_expensive_call  =
    IN-NODE VarDecl WITH-TRANSITION InitExpr
          (makes_an_expensive_call HOLDS-EVENTUALLY)
        HOLDS-EVENTUALLY;

  SET report_when =
       WHEN
             (is_global_but_not_const_variable AND is_initialized_with_expensive_call)
         HOLDS-IN-NODE VarDecl;

  SET message =
       "Global variable %decl_name% is initialized using a function or method call";
  SET suggestion =
        "If the function/method call is expensive, it can affect the starting time of the app.";
};


DEFINE-CHECKER CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK = {

        // The current node is block definition capturing a C++ reference
        LET capture_reference =
             WHEN
                ((is_node("BlockDecl") AND captures_cxx_references()) HOLDS-NEXT)
             HOLDS-IN-NODE BlockExpr;

         // At some point we encounter a block definition capturing a C++ reference
         LET block_definition_capture_reference =
            capture_reference HOLDS-EVENTUALLY;

        // A variable definition initialized with a block capturing a C++ reference
         LET variable_initialized_with_block =
        IN-NODE VarDecl WITH-TRANSITION InitExpr
            (block_definition_capture_reference)
         HOLDS-EVENTUALLY;

         // Reference to a variable initialized with a capturing block
         LET variable_block_definition =
            IN-NODE DeclRefExpr WITH-TRANSITION PointerToDecl
                (variable_initialized_with_block)
            HOLDS-EVENTUALLY;

        // Report when a function that does not have NoEscapeAttribute call a block or a variable definied with
        // a block capturing a C++ ref
        SET report_when =
            WHEN
                (NOT has_no_escape_attribute) AND (block_definition_capture_reference OR variable_block_definition)
            HOLDS-IN-NODE CallExpr;

      SET message =
            "C++ Reference variable(s) %cxx_ref_captured_in_block% captured by Objective-C block";

      SET suggestion = "This will very likely cause a crash because C++ References are unmanaged and may be invalid by the time the block executes.";

    SET severity = "ERROR";
        SET mode = "ON";
};


DEFINE-CHECKER POINTER_TO_INTEGRAL_IMPLICIT_CAST = {
  SET report_when =
      WHEN has_cast_kind("PointerToIntegral")
      HOLDS-IN-NODE ImplicitCastExpr;
  SET message = "Implicit conversion from %child_type% to %type% in usage of %name%";
    SET doc_url = "https://clang.llvm.org/docs/DiagnosticsReference.html#wint-conversion";
};

DEFINE-CHECKER POINTER_TO_CONST_OBJC_CLASS = {
  SET name = "Pointer To const Objective-C Class";
  SET report_when = is_decl() AND has_type_const_ptr_to_objc_class();
  SET message = "`const %class_name%*` may not mean what you want:
                   it represents a mutable pointer pointing to an Objective-C
                   class where the ivars cannot be changed.";
  SET suggestion = "Consider using `%class_name% *const` instead, meaning
                      the destination of the pointer cannot be changed.";
  SET severity = "WARNING";
  SET mode = "ON";
};


DEFINE-CHECKER DISCOURAGED_WEAK_PROPERTY_CUSTOM_SETTER = {
  LET has_body = HOLDS-NEXT WITH-TRANSITION Body (TRUE);
  LET is_weak_property_setter =
    HOLDS-NEXT WITH-TRANSITION AccessorForProperty "setter" (
      is_weak_property()
    );
  SET report_when =
    WHEN
      has_body AND
      is_weak_property_setter
    HOLDS-IN-NODE ObjCMethodDecl;
  SET message = "Custom setters are not called when ARC sets weak properties to nil.";
  SET severity = "WARNING";
  SET mode = "OFF";
};

DEFINE-CHECKER WRONG_SCOPE_FOR_DISPATCH_ONCE_T = {

  SET report_when =
        WHEN
              NOT (is_global_var() OR is_static_local_var()) AND
                has_type("dispatch_once_t")
            HOLDS-IN-NODE VarDecl;

        SET message = "Variables of type dispatch_once_t must have global or static scope. The result of using this type with automatic or dynamic allocation is undefined.";
        SET severity = "WARNING";
        SET mode = "ON";
};


DEFINE-CHECKER UNSAFE_CALL_TO_OPTIONAL_METHOD = {

  SET report_when =
   WHEN
     is_call_to_optional_objc_method
     AND
     (NOT objc_method_call_within_responds_to_selector_block)
  HOLDS-IN-NODE ObjCMessageExpr;

  SET message = "This is a call to an `@optional` protocol method. Calling it without checking if its implemented can lead to crashes at run time.";
    SET suggestion = "Please make sure to test the method is implemented by first calling `if ([object respondsToSelector:@selector(%decl_name%)]) ...` ";
    SET severity = "ERROR";
    SET mode = "ON";

};
