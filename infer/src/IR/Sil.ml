(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language *)

open! IStd
module F = Format
module L = Logging

(** {2 Programs and Types} *)

(** Kind of prune instruction *)
type if_kind =
  | Ik_bexp  (** boolean expressions, and exp ? exp : exp *)
  | Ik_dowhile
  | Ik_for
  | Ik_if
  | Ik_land_lor  (** obtained from translation of && or || *)
  | Ik_while
  | Ik_switch
[@@deriving compare, equal]

type instr_metadata =
  | Abstract of Location.t
      (** a good place to apply abstraction, mostly used in the biabduction analysis *)
  | ExitScope of Var.t list * Location.t  (** remove temporaries and dead program variables *)
  | Nullify of Pvar.t * Location.t  (** nullify stack variable *)
  | Skip  (** no-op *)
  | VariableLifetimeBegins of Pvar.t * Typ.t * Location.t  (** stack variable declared *)
[@@deriving compare]

(** An instruction. *)
type instr =
  (* Note for frontend writers:
     [x] must be used in a subsequent instruction, otherwise the entire
     `Load` instruction may be eliminated by copy-propagation. *)
  | Load of {id: Ident.t; e: Exp.t; root_typ: Typ.t; typ: Typ.t; loc: Location.t}
      (** Load a value from the heap into an identifier.

          [id = *exp:typ(root_typ)] where

          - [exp] is an expression denoting a heap address
          - [typ] is typ of [exp] and [id]
          - [root_typ] is the root type of [exp]

          The [root_typ] is deprecated: it is broken in C/C++. We are removing [root_typ] in the
          future, so please use [typ] instead. *)
  | Store of {e1: Exp.t; root_typ: Typ.t; typ: Typ.t; e2: Exp.t; loc: Location.t}
      (** Store the value of an expression into the heap.

          [*exp1:typ(root_typ) = exp2] where

          - [exp1] is an expression denoting a heap address
          - [typ] is typ of [*exp1] and [exp2]
          - [root_typ] is the root type of [exp1]
          - [exp2] is the expression whose value is stored.

          The [root_typ] is deprecated: it is broken in C/C++. We are removing [root_typ] in the
          future, so please use [typ] instead. *)
  | Prune of Exp.t * Location.t * bool * if_kind
      (** prune the state based on [exp=1], the boolean indicates whether true branch *)
  | Call of (Ident.t * Typ.t) * Exp.t * (Exp.t * Typ.t) list * Location.t * CallFlags.t
      (** [Call ((ret_id, ret_typ), e_fun, arg_ts, loc, call_flags)] represents an instruction
          [ret_id = e_fun(arg_ts);] *)
  | Metadata of instr_metadata
      (** hints about the program that are not strictly needed to understand its semantics, for
          instance information about its original syntactic structure *)
[@@deriving compare]

let equal_instr = [%compare.equal: instr]

let skip_instr = Metadata Skip

(** Check if an instruction is auxiliary, or if it comes from source instructions. *)
let instr_is_auxiliary = function
  | Load _ | Store _ | Prune _ | Call _ ->
      false
  | Metadata _ ->
      true


(** {2 Pretty Printing} *)

let color_wrapper ~f = if Config.print_using_diff then Pp.color_wrapper ~f else f

let pp_exp_typ pe f (e, t) = F.fprintf f "%a:%a" (Exp.pp_diff pe) e (Typ.pp pe) t

let location_of_instr_metadata = function
  | Abstract loc | ExitScope (_, loc) | Nullify (_, loc) | VariableLifetimeBegins (_, _, loc) ->
      loc
  | Skip ->
      Location.dummy


(** Get the location of the instruction *)
let location_of_instr = function
  | Load {loc} | Store {loc} | Prune (_, loc, _, _) | Call (_, _, _, loc, _) ->
      loc
  | Metadata metadata ->
      location_of_instr_metadata metadata


let exps_of_instr_metadata = function
  | Abstract _ ->
      []
  | ExitScope (vars, _) ->
      List.map ~f:Var.to_exp vars
  | Nullify (pvar, _) ->
      [Exp.Lvar pvar]
  | Skip ->
      []
  | VariableLifetimeBegins (pvar, _, _) ->
      [Exp.Lvar pvar]


(** get the expressions occurring in the instruction *)
let exps_of_instr = function
  | Load {id; e} ->
      [Exp.Var id; e]
  | Store {e1; e2} ->
      [e1; e2]
  | Prune (cond, _, _, _) ->
      [cond]
  | Call ((id, _), e, exps, _, _) ->
      let exps = List.map ~f:fst exps in
      e :: Exp.Var id :: exps
  | Metadata metadata ->
      exps_of_instr_metadata metadata


(** Convert an if_kind to string *)
let if_kind_to_string = function
  | Ik_bexp ->
      "boolean exp"
  | Ik_dowhile ->
      "do while"
  | Ik_for ->
      "for loop"
  | Ik_if ->
      "if"
  | Ik_land_lor ->
      "obtained from && or ||"
  | Ik_while ->
      "while"
  | Ik_switch ->
      "switch"


let pp_instr_metadata pe f = function
  | Abstract loc ->
      F.fprintf f "APPLY_ABSTRACTION; [%a]" Location.pp loc
  | ExitScope (vars, loc) ->
      F.fprintf f "EXIT_SCOPE(%a); [%a]" (Pp.seq ~sep:"," Var.pp) vars Location.pp loc
  | Nullify (pvar, loc) ->
      F.fprintf f "NULLIFY(%a); [%a]" (Pvar.pp pe) pvar Location.pp loc
  | Skip ->
      F.pp_print_string f "SKIP"
  | VariableLifetimeBegins (pvar, typ, loc) ->
      F.fprintf f "VARIABLE_DECLARED(%a:%a); [%a]" Pvar.pp_value pvar (Typ.pp_full pe) typ
        Location.pp loc


let pp_instr ~print_types pe0 f instr =
  let pp_typ = if print_types then Typ.pp_full else Typ.pp in
  let pp_root ~typ ~root_typ f =
    if not (Typ.equal typ root_typ) then F.fprintf f "(root %a)" (pp_typ pe0) root_typ
  in
  color_wrapper pe0 f instr ~f:(fun pe f instr ->
      match instr with
      | Load {id; e; root_typ; typ; loc} ->
          F.fprintf f "%a=*%a:%a%t [%a]" Ident.pp id (Exp.pp_diff ~print_types pe) e (pp_typ pe0)
            typ (pp_root ~typ ~root_typ) Location.pp loc
      | Store {e1; root_typ; typ; e2; loc} ->
          F.fprintf f "*%a:%a%t=%a [%a]" (Exp.pp_diff ~print_types pe) e1 (pp_typ pe0) root_typ
            (pp_root ~typ ~root_typ) (Exp.pp_diff ~print_types pe) e2 Location.pp loc
      | Prune (cond, loc, true_branch, _) ->
          F.fprintf f "PRUNE(%a, %b); [%a]" (Exp.pp_diff ~print_types pe) cond true_branch
            Location.pp loc
      | Call ((id, _), e, arg_ts, loc, cf) ->
          F.fprintf f "%a=" Ident.pp id ;
          F.fprintf f "%a(%a)%a [%a]" (Exp.pp_diff ~print_types pe) e
            (Pp.comma_seq (pp_exp_typ pe))
            arg_ts CallFlags.pp cf Location.pp loc
      | Metadata metadata ->
          pp_instr_metadata pe0 f metadata )


(** Dump an instruction. *)
let d_instr (i : instr) = L.d_pp_with_pe ~color:Pp.Green (pp_instr ~print_types:true) i
