(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = ALVar.formula_id * ALVar.alexp list [@@deriving compare]

(** (name, [param1,...,paramK]) *)

val captured_variables_cxx_ref : Ctl_parser_types.ast_node -> Clang_ast_t.named_decl_info list
(** list of cxx references captured by an ObjC Block *)

val objc_block_is_capturing_values : Ctl_parser_types.ast_node -> bool
(** true if the ObjC Block captures any variables *)

val call_method : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** 'call_method an m an' is true iff node an is a call to an ObjC method with name containing string m *)

val call_class_method : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** 'call_class_method an mname' is true iff node an is a call to an ObjC
    class method with name containing mname *)

val call_instance_method : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** 'call_instance_method an mname' is true iff node an is a call to an ObjC
    instance method with name containing mname *)

val declaration_name : Clang_ast_t.decl -> string option
(** 'declaration_name d' returns the name of declaration d *)

val is_enum_constant : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** 'is_enum_constant an name' is true iff an is an EnumConstant with name containing 'name' *)

val is_enum_constant_of_enum : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val is_global_var : Ctl_parser_types.ast_node -> bool
(** 'is_global_var an' is true iff an is a global variable (but not a static local) *)

val is_static_local_var : Ctl_parser_types.ast_node -> bool
(** 'is_static_local_var an' is true iff an is a static local variable *)

val is_const_expr_var : Ctl_parser_types.ast_node -> bool
(** 'is_const_expr_var an' is true iff an is a 'const' variable declaration *)

val is_qual_type_const : Ctl_parser_types.ast_node -> bool
(** 'is_qual_type_const an' is true iff an is a qual_type 'const' expression *)

val has_init_list_const_expr : Ctl_parser_types.ast_node -> bool
(** 'has_init_list_const_expr' is true iff for an InitListExpr where all subexpressions are const expression *)

val call_function : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** 'call_function an name' is true iff an is a call to a function whose name contains 'name' *)

val is_strong_property : Ctl_parser_types.ast_node -> bool
(** 'is_strong_property an' is true iff an denotes a objc property declaration with 'strong' attribute *)

val is_strong_ivar : Ctl_parser_types.ast_node -> bool
(** 'is_strong_ivar an' is true iff an denotes a objc ivar with 'strong' attribute *)

val is_weak_property : Ctl_parser_types.ast_node -> bool
(** 'is_weak_property an' is true iff an denotes a objc property declaration with 'weak' attribute *)

val is_assign_property : Ctl_parser_types.ast_node -> bool
(** 'is_assign_property an' is true iff an denotes a objc property declaration with 'assign' attribute *)

val is_property_pointer_type : Ctl_parser_types.ast_node -> bool
(** 'is_property_pointer_type an' is true iff an denotes a objc property declaration with type pointer *)

val context_in_synchronized_block : CLintersContext.context -> bool
(** true if the current node is in the context of a synchronized objc block *)

val is_ivar_atomic : Ctl_parser_types.ast_node -> bool
(** 'is_ivar_atomic an' is true iff an denotes an atomi objc ivar *)

val is_method_property_accessor_of_ivar :
  Ctl_parser_types.ast_node -> CLintersContext.context -> bool

val is_in_block : CLintersContext.context -> bool
(** true if the current node is in the context of an objc block *)

val is_in_cxx_constructor : CLintersContext.context -> ALVar.alexp -> bool
(** 'is_in_cxx_constructor context name' is true if the curent node is within a CXX constructor whose name contains 'name' *)

val is_in_cxx_destructor : CLintersContext.context -> ALVar.alexp -> bool
(** 'is_in_destructor_constructor context name' is true if the curent node is within a CXX destructor whose name contains 'name' *)

val is_in_cxx_method : CLintersContext.context -> ALVar.alexp -> bool
(** 'is_in_cxx_method context name' is true if the curent node is within a CXX method whose name contains 'name' *)

val is_in_function : CLintersContext.context -> ALVar.alexp -> bool
(** 'is_in_function context name' is true if the curent node is within a function whose name contains 'name' *)

val is_objc_extension : CLintersContext.context -> bool
(**
 *  Checks if the current file has an ObjC file extension (I.E. '.m' or '.mm')
 *)

val is_objc_class_named : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCInterfaceDecl or ObjCImplementationDecl
 *    node whose name matches the provided REGEXP
 *
 *  Matches on MyClass in:
 *    @interface MyClass
 *    @implementation MyClass
 *)

val is_objc_interface_named : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCInterfaceDecl node
 *    whose name matches the provided REGEXP
 *
 *  Matches on MyClass in @interface MyClass
 *)

val is_objc_implementation_named : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCImplementationDecl node
 *    whose name matches the provided REGEXP
 *
 *  Matches on MyClass in @implementation MyClass
 *)

val is_objc_category_named : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCCategoryDecl or ObjCCategoryImplDecl
 *    node whose name matches the provided REGEXP
 *
 *  Matches on MyCategory in:
 *    @interface MyClass (MyCategory)
 *    @implementation MyClass (MyCategory)
 *)

val is_objc_category_interface_named : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCCategoryDecl node
 *    whose name matches the provided REGEXP
 *
 *  Matches on MyCategory in @interface MyClass (MyCategory)
 *)

val is_objc_category_implementation_named : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCCategoryImplDecl node
 *    whose name matches the provided REGEXP
 *
 *  Matches on MyCategory in @implementation MyClass (MyCategory)
 *)

val is_objc_category_on_class_named : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCCategoryDecl or ObjCCategoryImplDecl
 *    node whose class's name matches the provided REGEXP
 *
 *  Matches on MyClass in:
 *    @interface MyClass (MyCategory)
 *    @implementation MyClass (MyCategory)
 *)

val is_objc_category_interface_on_class_named : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCCategoryDecl node
 *    whose class's name matches the provided REGEXP
 *
 *  Matches on MyClass in @interface MyClass (MyCategory)
 *)

val is_objc_category_implementation_on_class_named :
  Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCCategoryImplDecl node
 *    whose class's name matches the provided REGEXP
 *
 *  Matches on MyClass in @implementation MyClass (MyCategory)
 *)

val is_objc_category_on_subclass_of : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCCategoryDecl or ObjCCategoryImplDecl
 *  node whose class inherits from a class whose name matches the provided REGEXP
 *)

val is_objc_category_interface_on_subclass_of : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCCategoryDecl node whose class
 *    inherits from a class whose name matches the provided REGEXP
 *)

val is_objc_category_implementation_on_subclass_of :
  Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCCategoryImplDecl node whose class
 *    inherits from a class whose name matches the provided REGEXP
 *)

val adhere_to_protocol : Ctl_parser_types.ast_node -> bool
(** true if an objC  class adhere to a protocol *)

val is_objc_protocol_named : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCProtocolDecl node
 *    whose name matches the provided REGEXP
 *)

val is_objc_class_method_named : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCMethodDecl node whose name
 *    matches the provided REGEXP and is a class method
 *)

val is_objc_instance_method_named : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCMethodDecl node whose name
 *    matches the provided REGEXP and is an instance method
 *)

val is_objc_method_named : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCMethodDecl node
 *    whose name matches the provided REGEXP
 *)

val is_objc_constructor : CLintersContext.context -> bool
(** 'is_in_objc_constructor context' is true if the curent node is within an ObjC constructor *)

val objc_class_has_only_one_constructor_method_named :
  Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** true if an ObjC class has only one class method and is a constructor
whose name matches the provided REGEXP *)

val is_objc_dealloc : CLintersContext.context -> bool
(** 'is_in_objc_dealloc context' is true if the curent node is within an ObjC dealloc method *)

val is_in_objc_subclass_of : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCInterfaceDecl or
 *    ObjCImplementationDecl node which inherits from a class whose
 *    name matches the provided REGEXP
 *)

val is_in_objc_interface_named : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCInterfaceDecl
 *    node whose name matches the provided REGEXP
 *)

val is_in_objc_implementation_named : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCImplementationDecl
 *    node whose name matches the provided REGEXP
 *)

val is_in_objc_class_named : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCInterfaceDecl or
 *    ObjCImplementationDecl node whose name matches the provided REGEXP
 *)

val is_in_objc_category_interface_on_class_named : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCCategoryDecl node
 *    whose class's name matches the provided REGEXP
 *)

val is_in_objc_category_implementation_on_class_named :
  CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCCategoryImplDecl node
 *    whose class's name matches the provided REGEXP
 *)

val is_in_objc_category_on_class_named : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCCategoryDecl or
 *    ObjCCategoryImplDecl node whose class's name matches the provided REGEXP
 *)

val is_in_objc_category_interface_on_subclass_of : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCCategoryDecl node
 *    whose class inherits from a class whose name matches the provided REGEXP
 *)

val is_in_objc_category_implementation_on_subclass_of :
  CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCCategoryImplDecl node
 *    whose class inherits from a class whose name matches the provided REGEXP
 *)

val is_in_objc_category_on_subclass_of : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCCategoryDecl or
 *    ObjCCategoryImplDecl node whose class inherits from a class whose
 *    name matches the provided REGEXP
 *)

val is_in_objc_category_interface_named : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCCategoryDecl node
 *    whose name matches the provided REGEXP
 *)

val is_in_objc_category_implementation_named : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCCategoryImplDecl node
 *    whose name matches the provided REGEXP
 *)

val is_in_objc_category_named : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCCategoryDecl or
 *    ObjCCategoryImplDecl node whose name matches the provided REGEXP
 *)

val is_in_objc_protocol_named : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node is a subnode of an ObjCProtocolDecl
 *    node whose name matches the provided REGEXP
 *)

val is_in_objc_class_method : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node, or a parent node, is an ObjCMethodDecl node
 *    whose name matches the provided REGEXP and is a class method
 *)

val is_in_objc_instance_method : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node, or a parent node, is an ObjCMethodDecl node
 *    whose name matches the provided REGEXP and is an instance method
 *)

val is_in_objc_method : CLintersContext.context -> ALVar.alexp -> bool
(**
 *  Checks if the current node, or a parent node, is an ObjCMethodDecl node
 *    whose name matches the provided REGEXP
 *)

val is_objc_method_overriding : Ctl_parser_types.ast_node -> bool
(**
 *  Checks if the current node is an ObjCMethodDecl node and is overriding a
 *    method in the superclass.
 *
 *  A method is said to override any method in the class's base classes,
 *    its protocols, or its categories' protocols, that has the same selector
 *    and is of the same kind (class or instance). A method in an implementation
 *    is not considered as overriding the same method in the interface or its categories.
 *)

val is_objc_method_exposed : CLintersContext.context -> Ctl_parser_types.ast_node -> bool
(**
 *  Checks if the current node is an ObjCMethodDecl node and is exposed in an interface.
 *
 *  A method is said to be exposed if it's overriding a method or it's declared
 *    in a matching interface. For example, a method defined in a class's
 *    implementation is exposed if it's declared in the class's interface or
 *    interface extension, but not if it's declared in a category on the class.
 *    If the current node is a subnode of an ObjCInterfaceDecl, ObjCCategoryDecl,
 *    or ObjCProtocolDecl, this predicate returns false.
 *)

val captures_cxx_references : Ctl_parser_types.ast_node -> bool
(** 'captures_cxx_references an' is true iff the node an captures some CXX references *)

val is_binop_with_kind : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** 'is_binop_with_kind an binop' is true iff an denotes a binary operator of kind binop *)

val is_unop_with_kind : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** 'is_unop_of_kind an unop' is true iff an denotes a unary operator of kind unop *)

val has_cast_kind : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** 'has_cast_kind an cast' is true iff an denotes a cast operation of kind cast *)

val isa : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**  node an is of class classname *)

val is_node : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** 'is_node an nodename' is true iff an is a node of kind nodename *)

val declaration_has_name : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val declaration_ref_name :
  ?kind:Clang_ast_t.decl_kind -> Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** 'declaration_ref_has_name an n' is true iff node an is a DeclRefExpr with name containing string n. The optional parameter kind
allow to distinguish between special kind of decl_ref_exprs like 'is_enum_constant'. *)

val is_class : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val pp_predicate : Format.formatter -> t -> unit

val decl_unavailable_in_supported_ios_sdk :
  CLintersContext.context -> Ctl_parser_types.ast_node -> bool

val class_unavailable_in_supported_ios_sdk :
  CLintersContext.context -> Ctl_parser_types.ast_node -> bool

val has_type : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val has_value : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val method_return_type : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val has_type_subprotocol_of : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val get_available_attr_ios_sdk : Ctl_parser_types.ast_node -> string option

val get_selector : Ctl_parser_types.ast_node -> string option

val within_responds_to_selector_block :
  CLintersContext.context -> Ctl_parser_types.ast_node -> bool

val using_namespace : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val receiver_class_method_call : Ctl_parser_types.ast_node -> Clang_ast_t.decl option

val receiver_method_call : Ctl_parser_types.ast_node -> Clang_ast_t.decl option

val is_receiver_objc_class_type : Ctl_parser_types.ast_node -> bool
(**
 *  Checks if the current node is an ObjCMessageExpr node and has a
 *    receiver equivalent to the 'Class' type.
 *)

val is_receiver_objc_id_type : Ctl_parser_types.ast_node -> bool
(**
 *  Checks if the current node is an ObjCMessageExpr node and has a
 *    receiver equivalent to the 'id' type.
 *)

val is_receiver_subclass_of :
  CLintersContext.context -> Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCMessageExpr node and has a receiver
 *    which inherits from a class whose name matches the provided REGEXP.
 *)

val is_receiver_class_named :
  CLintersContext.context -> Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(**
 *  Checks if the current node is an ObjCMessageExpr node and has a
 *    receiver whose class name matches the provided REGEXP.
 *)

val is_receiver_super : Ctl_parser_types.ast_node -> bool
(**
 *  Checks if the current node is an ObjCMessageExpr node and has a
 *    receiver which is equal to 'super'.
 *
 *  Matches on [super myMethod];
 *)

val is_receiver_self : Ctl_parser_types.ast_node -> bool
(**
 *  Checks if the current node is an ObjCMessageExpr node and has a
 *    receiver which is equal to 'self'.
 *)

val is_at_selector_with_name : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** an is an expression @selector with whose name in the language of re *)

val has_visibility_attribute : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val cxx_construct_expr_has_name : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** true if the node is a CXXConstruct with name matching the provided REGEXP *)

val has_used_attribute : Ctl_parser_types.ast_node -> bool

val iphoneos_target_sdk_version_by_path : CLintersContext.context -> string option

val iphoneos_target_sdk_version_greater_or_equal : CLintersContext.context -> string -> bool

val within_available_class_block : CLintersContext.context -> Ctl_parser_types.ast_node -> bool

val has_type_const_ptr_to_objc_class : Ctl_parser_types.ast_node -> bool

val is_decl : Ctl_parser_types.ast_node -> bool
(** is_decl an is true iff an is a node denoting a declaration *)

val get_ast_node_type_ptr : Ctl_parser_types.ast_node -> Clang_ast_t.type_ptr option

val is_method_called_by_superclass : Ctl_parser_types.ast_node -> bool

val is_cxx_copy_constructor : Ctl_parser_types.ast_node -> bool
(** true if the current node is a C++ copy constructor *)

val has_cxx_fully_qualified_name : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** true iff node has C++ fully qualified name (w/class and namespace)
 *  matching the given regexp
 *)

val is_cxx_method_overriding : Ctl_parser_types.ast_node -> ALVar.alexp option -> bool
(**
 *  True iff the current node is a CXXMethodDecl node and is overriding a
 *    method whose fully-qualified name (with class and namespace) matches
 *    the given regexp (if given, otherwise any overriding method satisfies).
 *)

val is_init_expr_cxx11_constant : Ctl_parser_types.ast_node -> bool
(** true if the current node is classified as C++11 constant expression by the AST. It works only for VarDecl init expr *)

val cxx_construct_expr_has_no_parameters : Ctl_parser_types.ast_node -> bool
(** true if a construct expr has no subexpressions *)
