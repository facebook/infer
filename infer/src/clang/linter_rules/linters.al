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
		(NOT context_in_synchronized_block())
		AND NOT is_method_property_accessor_of_ivar()
		AND NOT is_objc_constructor()
		AND NOT is_objc_dealloc();

  	SET message = "Direct access to ivar %s of an atomic property";
  	SET suggestion = "Accessing an ivar of an atomic property makes the property nonatomic";
	  SET severity = "WARNING";
};

// ASSIGN_POINTER_WARNING:
// a property with a pointer type should not be declared `assign`
DEFINE-CHECKER ASSIGN_POINTER_WARNING = {

	 SET report_when =
	  	is_assign_property() AND is_property_pointer_type();

	 SET message = "Property `%s` is a pointer type marked with the `assign` attribute";
	 SET suggestion = "Use a different attribute like `strong` or `weak`.";
	 SET severity = "WARNING";
};

// BAD_POINTER_COMPARISON:
// Fires whenever a NSNumber is dangerously coerced to a boolean in a comparison
DEFINE-CHECKER BAD_POINTER_COMPARISON = {

	LET is_binop = is_stmt(BinaryOperator);
	LET is_binop_eq = is_binop_with_kind(EQ);
	LET is_binop_ne = is_binop_with_kind(NE);
	LET is_binop_neq = Or (is_binop_eq, is_binop_ne);
	LET is_unop_lnot = is_unop_with_kind(LNot);
	LET is_implicit_cast_expr = is_stmt(ImplicitCastExpr);
	LET is_expr_with_cleanups = is_stmt(ExprWithCleanups);
	LET is_nsnumber = isa(NSNumber);

 	SET report_when =
         	(
					 (NOT is_binop_neq) AND (
					 is_expr_with_cleanups
					 OR is_implicit_cast_expr
					 OR is_binop
					 OR is_unop_lnot)
				 	)
				 	HOLDS-UNTIL
				 	(
				 	 	is_nsnumber
				 	);
};

DEFINE-CHECKER REGISTERED_OBSERVER_BEING_DEALLOCATED = {

	LET exists_method_calling_addObserver =
		 call_method(addObserver:selector:name:object:) HOLDS-EVENTUALLY;

	LET exists_method_calling_addObserverForName =
	   call_method(addObserverForName:object:queue:usingBlock:) HOLDS-EVENTUALLY;

	LET add_observer =
	   exists_method_calling_addObserver OR exists_method_calling_addObserverForName;

  LET eventually_addObserver =
	    IN-NODE ObjCMethodDecl WITH-TRANSITION Body
			 		(add_observer)
			 HOLDS-EVENTUALLY;

	LET exists_method_calling_removeObserver =
	     call_method(removeObserver:) HOLDS-EVENTUALLY;

	LET exists_method_calling_removeObserverName =
		  call_method(removeObserver:name:object:) HOLDS-EVENTUALLY;

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
					remove_observer_in_method INSTANCEOF ObjCImplementationDecl, ObjCProtocolDecl)
  	HOLDS-EVENTUALLY;

  SET report_when = NOT (eventually_addObserver IMPLIES eventually_removeObserver);

};
