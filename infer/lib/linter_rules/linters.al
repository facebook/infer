// Copyright (c) 2016 - present Facebook, Inc.
// All rights reserved.
//
// This source code is licensed under the BSD style license found in the
// LICENSE file in the root directory of this source tree. An additional grant
// of patent rights can be found in the PATENTS file in the same directory.

// DIRECT_ATOMIC_PROPERTY_ACCESS:
// a property declared atomic should not be accessed directly via its ivar
DEFINE-CHECKER DIRECT_ATOMIC_PROPERTY_ACCESS = {

	SET report_when =
	  WHEN
			((NOT context_in_synchronized_block()) AND is_ivar_atomic())
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

	LET is_binop = is_node("BinaryOperator");
	LET is_binop_eq = is_binop_with_kind("EQ");
	LET is_binop_ne = is_binop_with_kind("NE");
	LET is_binop_neq = is_binop_eq OR is_binop_ne;
	LET is_unop_lnot = is_unop_with_kind("LNot");
	LET is_implicit_cast_expr = is_node("ImplicitCastExpr");
	LET is_expr_with_cleanups = is_node("ExprWithCleanups");
	LET is_nsnumber = isa("NSNumber");

  LET eu =(
	 					(NOT is_binop_neq)
						AND (is_expr_with_cleanups
	 								OR is_implicit_cast_expr
	 								OR is_binop
	 								OR is_unop_lnot)
					)
						HOLDS-UNTIL
					(
						is_nsnumber
					);

  LET etx =
		IN-EXCLUSIVE-NODE IfStmt, ForStmt, WhileStmt, ConditionalOperator WITH-TRANSITION Cond
				(eu)
		HOLDS-EVENTUALLY;

 	SET report_when =
					WHEN
         		etx
					HOLDS-IN-NODE IfStmt, ForStmt, WhileStmt, ConditionalOperator;

  SET message = "Implicitly checking whether NSNumber pointer is nil";

	SET suggestion =
		"Did you mean to compare against the unboxed value instead? Please either explicitly compare the NSNumber instance to nil, or use one of the NSNumber accessors before the comparison.";
};


DEFINE-CHECKER REGISTERED_OBSERVER_BEING_DEALLOCATED = {

	LET exists_method_calling_addObserver =
		 call_method("addObserver:selector:name:object:") HOLDS-EVENTUALLY;

	LET exists_method_calling_addObserverForName =
	   call_method("addObserverForName:object:queue:usingBlock:") HOLDS-EVENTUALLY;

	LET add_observer =
	   exists_method_calling_addObserver OR exists_method_calling_addObserverForName;

  LET eventually_addObserver =
	    IN-NODE ObjCMethodDecl WITH-TRANSITION Body
			 		(add_observer)
			 HOLDS-EVENTUALLY;

	LET exists_method_calling_removeObserver =
	     call_method("removeObserver:") HOLDS-EVENTUALLY;

	LET exists_method_calling_removeObserverName =
		  call_method("removeObserver:name:object:") HOLDS-EVENTUALLY;

	LET remove_observer =
				exists_method_calling_removeObserver OR exists_method_calling_removeObserverName;

	LET remove_observer_in_block =
				IN-NODE BlockDecl WITH-TRANSITION Body
						(remove_observer)
				HOLDS-EVENTUALLY;

 	LET remove_observer1 =
 				remove_observer OR remove_observer_in_block;

  LET remove_observer_in_method =
  	IN-NODE ObjCMethodDecl WITH-TRANSITION Body
	  		(remove_observer1)
		HOLDS-EVENTUALLY;

 	LET eventually_removeObserver =
		IN-NODE ObjCImplementationDecl, ObjCProtocolDecl WITH-TRANSITION _
		 		(remove_observer_in_method  OR
					remove_observer_in_method HOLDS-IN-SOME-SUPERCLASS-OF ObjCImplementationDecl)
  	HOLDS-EVENTUALLY;

  SET report_when =
			WHEN
	    	NOT (eventually_addObserver IMPLIES eventually_removeObserver)
			HOLDS-IN-NODE ObjCImplementationDecl, ObjCProtocolDecl;

	SET message =
		"Object self is registered in a notification center but not being removed before deallocation";

	SET suggestion =
		"Consider removing the object from the notification center before its deallocation.";

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

  LET is_global_variable =
     is_objc_extension() AND is_global_var() AND (NOT is_const_var());

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
     		(is_global_variable AND is_initialized_with_expensive_call)
		 HOLDS-IN-NODE VarDecl;

  SET message =
       "Global variable %decl_name% is initialized using a function or method call";
  SET suggestion =
        "If the function/method call is expensive, it can affect the starting time of the app.";
};


DEFINE-CHECKER CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK = {
	  SET report_when =
		     WHEN
				   ((is_node("BlockDecl") AND captures_cxx_references())
					 HOLDS-NEXT)
         HOLDS-IN-NODE BlockExpr;

// * Alternative ways of writing this check:
//			 SET report_when =
//				 		 WHEN
//				 			 captures_cxx_references()
//				 		 HOLDS-IN-NODE BlockDecl;
//
//		SET report_when =
//		is_node(BlockDecl) AND captures_cxx_references();

	  SET message =
	        "C++ Reference variable(s) %cxx_ref_captured_in_block% captured by Objective-C block";

	  SET suggestion = "C++ References are unmanaged and may be invalid by the time the block executes.";

	};

	// If the declaration has availability attributes, check that it's compatible with
	// the iphoneos_target_sdk_version
	DEFINE-CHECKER UNAVAILABLE_API_IN_SUPPORTED_IOS_SDK = {

		SET report_when =
		     WHEN HOLDS-NEXT WITH-TRANSITION PointerToDecl
	 				 (decl_unavailable_in_supported_ios_sdk() AND
	 				 NOT within_responds_to_selector_block())
				 HOLDS-IN-NODE DeclRefExpr, ObjCMessageExpr;

		  SET message =
		        "%decl_ref_or_selector_name% is not available in the required iOS SDK version
		         %iphoneos_target_sdk_version% (only available from version %available_ios_sdk%)";
      SET name = "Unavailable API In Supported iOS SDK";
		  SET suggestion = "This could cause a crash.";
			SET severity = "ERROR";
		};

	DEFINE-CHECKER UNAVAILABLE_CLASS_IN_SUPPORTED_IOS_SDK = {
		SET report_when =
				 WHEN (class_unavailable_in_supported_ios_sdk())
				 HOLDS-IN-NODE ObjCMessageExpr;

			SET message =
						"The receiver %receiver_method_call% of %name% is not available in the required iOS SDK version
						 %iphoneos_target_sdk_version% (only available from version %class_available_ios_sdk%)";
			SET name = "Unavailable API In Supported iOS SDK";
			SET severity = "ERROR";
			SET mode = "OFF";
		};


DEFINE-CHECKER POINTER_TO_INTEGRAL_IMPLICIT_CAST = {
  SET report_when =
      WHEN has_cast_kind("PointerToIntegral")
      HOLDS-IN-NODE ImplicitCastExpr;
  SET message = "Implicit conversion from %child_type% to %type% in usage of %name%";
	SET doc_url = "https://clang.llvm.org/docs/DiagnosticsReference.html#wint-conversion";
};
