// Copyright (c) 2016 - present Facebook, Inc.
// All rights reserved.
//
// This source code is licensed under the BSD style license found in the
// LICENSE file in the root directory of this source tree. An additional grant
// of patent rights can be found in the PATENTS file in the same directory.

// DIRECT_ATOMIC_PROPERTY_ACCESS:
// a property declared atomic should not be accessed directly via its ivar
define-checker DIRECT_ATOMIC_PROPERTY_ACCESS = {

	set formula =
		(NOT context_in_synchronized_block())
		AND NOT is_method_property_accessor_of_ivar()
		AND NOT is_objc_constructor()
		AND NOT is_objc_dealloc();

  	set message = "Direct access to ivar %s of an atomic property";
  	set suggestion = "Accessing an ivar of an atomic property makes the property nonatomic";
	  set severity = "WARNING";
};

// ASSIGN_POINTER_WARNING:
// a property with a pointer type should not be declared `assign`
define-checker ASSIGN_POINTER_WARNING = {

	 set formula =
	  	is_assign_property() AND is_property_pointer_type();

	 set message = "Property `%s` is a pointer type marked with the `assign` attribute";
	 set suggestion = "Use a different attribute like `strong` or `weak`.";
	 set severity = "WARNING";
};

// BAD_POINTER_COMPARISON:
// Fires whenever a NSNumber is dangerously coerced to a boolean in a comparison
define-checker BAD_POINTER_COMPARISON = {

	let is_binop = is_stmt(BinaryOperator);
	let is_binop_eq = is_binop_with_kind(EQ);
	let is_binop_ne = is_binop_with_kind(NE);
	let is_binop_neq = Or (is_binop_eq, is_binop_ne);
	let is_unop_lnot = is_unop_with_kind(LNot);
	let is_implicit_cast_expr = is_stmt(ImplicitCastExpr);
	let is_expr_with_cleanups = is_stmt(ExprWithCleanups);
	let is_nsnumber = isa(NSNumber);

 	set formula =
         	(
					 (NOT is_binop_neq) AND (
					 is_expr_with_cleanups
					 OR is_implicit_cast_expr
					 OR is_binop
					 OR is_unop_lnot)
				 	)
				 	holds-until
				 	(
				 	 	is_nsnumber
				 	);
};
