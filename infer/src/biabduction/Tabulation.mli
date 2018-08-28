(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** kind of result of a procedure call *)
type call_result =
  | CR_success  (** successful call *)
  | CR_not_met  (** precondition not met *)
  | CR_not_found  (** the callee has no specs *)
  | CR_skip  (** the callee was skipped *)

val log_call_trace :
     caller_name:Typ.Procname.t
  -> callee_name:Typ.Procname.t
  -> ?callee_attributes:ProcAttributes.t
  -> ?reason:string
  -> ?dynamic_dispatch:EventLogger.dynamic_dispatch
  -> Location.t
  -> call_result
  -> unit

(** Interprocedural footprint analysis *)

val remove_constant_string_class : Tenv.t -> 'a Prop.t -> Prop.normal Prop.t
(** Remove constant string or class from a prop *)

val check_attr_dealloc_mismatch : PredSymb.t -> PredSymb.t -> unit
(** Check if the attribute change is a mismatch between a kind of allocation
    and a different kind of deallocation *)

val find_dereference_without_null_check_in_sexp : Sil.strexp -> (int * PredSymb.path_pos) option
(** Check whether a sexp contains a dereference without null check,
    and return the line number and path position *)

val create_cast_exception :
  Tenv.t -> Logging.ocaml_pos -> Typ.Procname.t option -> Exp.t -> Exp.t -> Exp.t -> exn
(** raise a cast exception *)

val prop_is_exn : Typ.Procname.t -> 'a Prop.t -> bool
(** check if a prop is an exception *)

val prop_get_exn_name : Typ.Procname.t -> 'a Prop.t -> Typ.Name.t option
(** when prop is an exception, return the exception name *)

val lookup_custom_errors : 'a Prop.t -> string option
(** search in prop contains an error state *)

val exe_function_call :
     ?dynamic_dispatch:EventLogger.dynamic_dispatch
  -> Exe_env.t
  -> Summary.t
  -> Tenv.t
  -> Ident.t
  -> Procdesc.t
  -> Typ.Procname.t
  -> Location.t
  -> (Exp.t * Typ.t) list
  -> Prop.normal Prop.t
  -> Paths.Path.t
  -> (Prop.normal Prop.t * Paths.Path.t) list
(** Execute the function call and return the list of results with return value *)

val get_specs_from_payload : Summary.t -> Prop.normal BiabductionSummary.spec list
(** Get the specs from the payload of the summary. *)
