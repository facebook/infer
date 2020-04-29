(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for builtin functions with their symbolic execution handler *)

type args =
  { instr: Sil.instr
  ; prop_: Prop.normal Prop.t
  ; path: Paths.Path.t
  ; ret_id_typ: Ident.t * Typ.t
  ; args: (Exp.t * Typ.t) list
  ; proc_name: Procname.t
  ; loc: Location.t
  ; analysis_data: BiabductionSummary.t InterproceduralAnalysis.t }

type ret_typ = (Prop.normal Prop.t * Paths.Path.t) list

type t = args -> ret_typ

type registered

val register : Procname.t -> t -> registered
(** Register a builtin [Procname.t] and symbolic execution handler *)

val get : Procname.t -> t option
(** Get the symbolic execution handler associated to the builtin function name *)

val print_and_exit : unit -> 'a
(** Print the builtin functions and exit *)
