(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Attributes of a procedure. *)

(** Visibility modifiers. *)
type access = Default | Public | Private | Protected [@@deriving compare]

val equal_access : access -> access -> bool

type objc_accessor_type = Objc_getter of Struct.field | Objc_setter of Struct.field

type var_data =
  { name: Mangled.t
  ; typ: Typ.t
  ; modify_in_block: bool
        (** __block attribute of Objective-C variables, means that it will be modified inside a
            block *)
  ; is_constexpr: bool
  ; is_declared_unused: bool  (** variable declared with attribute [unused] *)
  ; is_structured_binding: bool  (** variable declared by structured binding *)
  ; has_cleanup_attribute: bool
        (** variable declared with attribute [cleanup], only set in clang frontend *)
  ; tmp_id: Ident.t option
        (** the tmp id used to build the variable name in case of a temp variable, None otherwise. *)
  }

type specialized_with_aliasing_info =
  { orig_proc: Procname.t
  ; aliases: Pvar.t list list
        (** all the pvars in a same list are aliasing each other. e.g.
            [aliases = [[x; y; z]; [a; b]]] indicates that [x], [y] and [z] alias each other and [a]
            and [b] as well *) }
[@@deriving compare]

type 'captured_var passed_closure =
  | Closure of (Procname.t * 'captured_var list)
  | Fields of (Fieldname.t * 'captured_var passed_closure) list
[@@deriving compare, equal]

type specialized_with_closures_info =
  {orig_proc: Procname.t; formals_to_closures: CapturedVar.t passed_closure Pvar.Map.t}
[@@deriving compare]

type block_as_arg_attributes = {passed_to: Procname.t; passed_as_noescape_block: bool}
[@@deriving compare, equal]

type t =
  { access: access  (** visibility access *)
  ; captured: CapturedVar.t list
        (** name, type, and mode of variables captured in blocks and lambdas *)
  ; mutable changed: bool  (** true if proc has changed since last analysis *)
  ; exceptions: string list  (** exceptions thrown by the procedure *)
  ; formals: (Mangled.t * Typ.t * Annot.Item.t) list
        (** name, type, and annotation of formal parameters *)
  ; const_formals: int list  (** list of indices of formals that are const-qualified *)
  ; reference_formals: int list  (** list of indices of formals that are passed by reference *)
  ; is_abstract: bool  (** the procedure is abstract *)
  ; is_biabduction_model: bool  (** the procedure is a model for the biabduction analysis *)
  ; is_bridge_method: bool  (** the procedure is a bridge method *)
  ; is_cpp_const_member_fun: bool  (** true if the procedure is a const function *)
  ; is_cpp_copy_assignment: bool  (** true if the procedure is a copy assignment *)
  ; is_cpp_copy_ctor: bool  (** true if the procedure is a copy constructor *)
  ; is_cpp_move_ctor: bool  (** true if the procedure is a move constructor *)
  ; is_cpp_deleted: bool  (** true if the procedure is deleted *)
  ; is_cpp_implicit: bool
        (** returns false if the declaration exists in code and true if it was created implicitly by
            the compiler *)
  ; is_defined: bool  (** true if the procedure is defined, and not just declared *)
  ; is_java_synchronized_method: bool  (** the procedure is a Java synchronized method *)
  ; is_csharp_synchronized_method: bool  (** the procedure is a C# synchronized method *)
  ; is_hack_async: bool
  ; is_hack_wrapper: bool  (** a generated wrapper for LSB or default parameters *)
  ; block_as_arg_attributes: block_as_arg_attributes option
        (** Present if the procedure is an Objective-C block that has been passed to the given
            method in a position annotated with the NS_NOESCAPE attribute. *)
  ; is_no_return: bool  (** the procedure is known not to return *)
  ; is_objc_arc_on: bool  (** the ObjC procedure is compiled with ARC *)
  ; is_specialized: bool  (** the procedure is a clone specialized for dynamic dispatch handling *)
  ; is_synthetic_method: bool  (** the procedure is a synthetic method *)
  ; is_clang_variadic: bool  (** the procedure is variadic, only supported for Clang procedures *)
  ; hack_variadic_position: int option
        (** the procedure is variadic and [Some n] means the variadic vector is composed of the
            arguments n, n+1, ..., length formals -1 *)
  ; sentinel_attr: (int * int) option  (** __attribute__((sentinel(int, int))) *)
  ; specialized_with_aliasing_info: specialized_with_aliasing_info option
        (** the procedure is a clone specialized with captured variables and paramaters sharing
            memory, with link to the original procedure, and a list of variables aliasing each
            other. *)
  ; specialized_with_closures_info: specialized_with_closures_info option
        (** the procedure is a clone specialized with calls to concrete closures, with link to the
            original procedure, and a map that links the original formals to the elements of the
            closure used to specialize the procedure. *)
  ; clang_method_kind: ClangMethodKind.t  (** the kind of method the procedure is *)
  ; loc: Location.t  (** location of this procedure in the source code *)
  ; loc_instantiated: Location.t option  (** location of this procedure is possibly instantiated *)
  ; translation_unit: SourceFile.t  (** source file where the procedure was captured *)
  ; mutable locals: var_data list  (** name, type and attributes of local variables *)
  ; objc_accessor: objc_accessor_type option  (** type of ObjC accessor, if any *)
  ; proc_name: Procname.t  (** name of the procedure *)
  ; ret_type: Typ.t  (** return type *)
  ; ret_annots: Annot.Item.t  (** annotations of return type *)
  ; has_added_return_param: bool  (** whether or not a return param was added *)
  ; is_ret_type_pod: bool  (** whether or not the return type is POD *)
  ; is_ret_constexpr: bool  (** whether the (C++) function or method is declared as [constexpr] *)
  }

val default : SourceFile.t -> Procname.t -> t
(** Create a proc_attributes with default values. *)

val default_var_data : Pvar.t -> Typ.t -> var_data

val pp : Format.formatter -> t -> unit

val get_access : t -> access
(** Return the visibility attribute *)

val get_loc : t -> Location.t
(** Return loc information for the procedure *)

val get_loc_instantiated : t -> Location.t option
(** Return instantiated loc information for the procedure *)

val get_proc_name : t -> Procname.t

val get_pvar_formals : t -> (Pvar.t * Typ.t) list
(** Return pvar and type of formal parameters *)

val get_passed_by_value_formals : t -> (Pvar.t * Typ.t) list
(** Return pvar and type of formal parameters that are passed by value *)

val get_passed_by_ref_formals : t -> (Pvar.t * Typ.t) list
(** Return pvar and type of formal parameters that are passed by reference *)

val get_pointer_formals : t -> (Pvar.t * Typ.t) list
(** Return pvar and type of formal parameters that are passed as pointer, i.e. [T*] *)

val to_return_type : t -> Typ.t
(** the return type from method signature, taking into account if the procedure has added return
    parameter *)

val get_this : t -> Pvar.t option
(** if the procedures is an instance method then this is its [self] or [this] variable *)

val pp_block_as_arg_attributes : F.formatter -> block_as_arg_attributes -> unit

module SQLite : SqliteUtils.Data with type t = t
