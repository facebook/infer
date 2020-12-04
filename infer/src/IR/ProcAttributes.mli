(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Attributes of a procedure. *)

type objc_accessor_type = Objc_getter of Struct.field | Objc_setter of Struct.field

type var_data =
  { name: Mangled.t
  ; typ: Typ.t
  ; modify_in_block: bool
        (** __block attribute of Objective-C variables, means that it will be modified inside a
            block *)
  ; is_constexpr: bool
  ; is_declared_unused: bool  (** variable declared with attribute [unused] *) }

type specialized_with_blocks_info =
  { orig_proc: Procname.t
  ; formals_to_procs_and_new_formals: (Procname.t * (Mangled.t * Typ.t) list) Mangled.Map.t }
[@@deriving compare]

type t =
  { access: PredSymb.access  (** visibility access *)
  ; captured: CapturedVar.t list
        (** name, type, and mode of variables captured in blocks and lambdas *)
  ; exceptions: string list  (** exceptions thrown by the procedure *)
  ; formals: (Mangled.t * Typ.t) list  (** name and type of formal parameters *)
  ; const_formals: int list  (** list of indices of formals that are const-qualified *)
  ; is_abstract: bool  (** the procedure is abstract *)
  ; is_biabduction_model: bool  (** the procedure is a model for the biabduction analysis *)
  ; is_bridge_method: bool  (** the procedure is a bridge method *)
  ; is_defined: bool  (** true if the procedure is defined, and not just declared *)
  ; is_java_synchronized_method: bool  (** the procedure is a Java synchronized method *)
  ; passed_as_noescape_block_to: Procname.t option
        (** Present if the procedure is an Objective-C block that has been passed to the given
            method in a position annotated with the NS_NOESCAPE attribute. *)
  ; is_no_return: bool  (** the procedure is known not to return *)
  ; is_objc_arc_on: bool  (** the ObjC procedure is compiled with ARC *)
  ; is_specialized: bool  (** the procedure is a clone specialized for dynamic dispatch handling *)
  ; is_synthetic_method: bool  (** the procedure is a synthetic method *)
  ; is_variadic: bool  (** the procedure is variadic, only supported for Clang procedures *)
  ; sentinel_attr: (int * int) option  (** __attribute__((sentinel(int, int))) *)
  ; specialized_with_blocks_info: specialized_with_blocks_info option
        (** the procedure is a clone specialized with calls to concrete closures, with link to the
            original procedure, and a map that links the original formals to the elements of the
            closure used to specialize the procedure. *)
  ; clang_method_kind: ClangMethodKind.t  (** the kind of method the procedure is *)
  ; loc: Location.t  (** location of this procedure in the source code *)
  ; translation_unit: SourceFile.t  (** source file where the procedure was captured *)
  ; mutable locals: var_data list  (** name, type and attributes of local variables *)
  ; method_annotation: Annot.Method.t  (** annotations for all methods *)
  ; objc_accessor: objc_accessor_type option  (** type of ObjC accessor, if any *)
  ; proc_name: Procname.t  (** name of the procedure *)
  ; ret_type: Typ.t  (** return type *)
  ; has_added_return_param: bool  (** whether or not a return param was added *) }

val default : SourceFile.t -> Procname.t -> t
(** Create a proc_attributes with default values. *)

val pp : Format.formatter -> t -> unit

val get_access : t -> PredSymb.access
(** Return the visibility attribute *)

val get_formals : t -> (Mangled.t * Typ.t) list
(** Return name and type of formal parameters *)

val get_annotated_formals : t -> ((Mangled.t * Typ.t) * Annot.Item.t) list

val get_loc : t -> Location.t
(** Return loc information for the procedure *)

val get_proc_name : t -> Procname.t

val get_pvar_formals : t -> (Pvar.t * Typ.t) list
(** Return pvar and type of formal parameters *)

module SQLite : SqliteUtils.Data with type t = t
