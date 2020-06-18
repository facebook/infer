(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Interprocedural footprint analysis *)

val remove_constant_string_class : Tenv.t -> 'a Prop.t -> Prop.normal Prop.t
(** Remove constant string or class from a prop *)

val create_cast_exception :
  Tenv.t -> Logging.ocaml_pos -> Procname.t option -> Exp.t -> Exp.t -> Exp.t -> exn
(** raise a cast exception *)

val prop_is_exn : Procname.t -> 'a Prop.t -> bool
(** check if a prop is an exception *)

val prop_get_exn_name : Procname.t -> 'a Prop.t -> Typ.Name.t option
(** when prop is an exception, return the exception name *)

val lookup_custom_errors : 'a Prop.t -> string option
(** search in prop contains an error state *)

val exe_function_call :
     BiabductionSummary.t InterproceduralAnalysis.t
  -> callee_attributes:ProcAttributes.t
  -> callee_pname:Procname.t
  -> callee_summary:BiabductionSummary.t
  -> ret_id:Ident.t
  -> Location.t
  -> actuals:(Exp.t * Typ.t) list
  -> Prop.normal Prop.t
  -> Paths.Path.t
  -> (Prop.normal Prop.t * Paths.Path.t) list
(** Execute the function call and return the list of results with return value *)
