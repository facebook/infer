(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

type if_kind =
  | Ik_bexp of {terminated: bool}
  | Ik_compexch
  | Ik_dowhile
  | Ik_for
  | Ik_if of {terminated: bool}
  | Ik_land_lor
  | Ik_while
  | Ik_switch
[@@deriving compare, equal]

let pp_if_kind fmt = function
  | Ik_bexp {terminated} ->
      F.pp_print_string fmt "boolean exp" ;
      if terminated then F.pp_print_string fmt " (terminated)"
  | Ik_compexch ->
      F.pp_print_string fmt "atomic compare exchange"
  | Ik_dowhile ->
      F.pp_print_string fmt "do while"
  | Ik_for ->
      F.pp_print_string fmt "for loop"
  | Ik_if {terminated} ->
      F.pp_print_string fmt "if" ;
      if terminated then F.pp_print_string fmt " (terminated)"
  | Ik_land_lor ->
      F.pp_print_string fmt "obtained from && or ||"
  | Ik_while ->
      F.pp_print_string fmt "while"
  | Ik_switch ->
      F.pp_print_string fmt "switch"


let is_terminated_if_kind = function
  | Ik_bexp {terminated} | Ik_if {terminated} ->
      terminated
  | Ik_compexch | Ik_dowhile | Ik_for | Ik_land_lor | Ik_while | Ik_switch ->
      false


type instr_metadata =
  | Abstract of Location.t
  | CatchEntry of {try_id: int; loc: Location.t}
  | EndBranches
  | ExitScope of Var.t list * Location.t
  | Nullify of Pvar.t * Location.t
  | Skip
  | TryEntry of {try_id: int; loc: Location.t}
  | TryExit of {try_id: int; loc: Location.t}
  | VariableLifetimeBegins of Pvar.t * Typ.t * Location.t
[@@deriving compare, equal]

type instr =
  | Load of {id: Ident.t; e: Exp.t; typ: Typ.t; loc: Location.t}
  | Store of {e1: Exp.t; typ: Typ.t; e2: Exp.t; loc: Location.t}
  | Prune of Exp.t * Location.t * bool * if_kind
  | Call of (Ident.t * Typ.t) * Exp.t * (Exp.t * Typ.t) list * Location.t * CallFlags.t
  | Metadata of instr_metadata
[@@deriving compare, equal]

let skip_instr = Metadata Skip

let instr_is_auxiliary = function
  | Load _ | Store _ | Prune _ | Call _ ->
      false
  | Metadata _ ->
      true


let color_wrapper ~f = if Config.print_using_diff then Pp.color_wrapper ~f else f

let pp_exp_typ pe f (e, t) = F.fprintf f "%a:%a" (Exp.pp_diff pe) e (Typ.pp pe) t

let location_of_instr_metadata = function
  | Abstract loc
  | CatchEntry {loc}
  | ExitScope (_, loc)
  | Nullify (_, loc)
  | TryEntry {loc}
  | TryExit {loc}
  | VariableLifetimeBegins (_, _, loc) ->
      loc
  | EndBranches | Skip ->
      Location.dummy


let location_of_instr = function
  | Load {loc} | Store {loc} | Prune (_, loc, _, _) | Call (_, _, _, loc, _) ->
      loc
  | Metadata metadata ->
      location_of_instr_metadata metadata


let exps_of_instr_metadata = function
  | Abstract _ | CatchEntry _ | EndBranches ->
      []
  | ExitScope (vars, _) ->
      List.map ~f:Var.to_exp vars
  | Nullify (pvar, _) ->
      [Exp.Lvar pvar]
  | Skip | TryEntry _ | TryExit _ ->
      []
  | VariableLifetimeBegins (pvar, _, _) ->
      [Exp.Lvar pvar]


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


let pp_instr_metadata pe f = function
  | Abstract loc ->
      F.fprintf f "APPLY_ABSTRACTION; [%a]" Location.pp loc
  | CatchEntry {loc} ->
      F.fprintf f "CATCH_ENTRY; [%a]" Location.pp loc
  | EndBranches ->
      F.fprintf f "END_BRANCHES"
  | ExitScope (vars, loc) ->
      F.fprintf f "EXIT_SCOPE(%a); [%a]" (Pp.seq ~sep:"," Var.pp) vars Location.pp loc
  | Nullify (pvar, loc) ->
      F.fprintf f "NULLIFY(%a); [%a]" (Pvar.pp pe) pvar Location.pp loc
  | Skip ->
      F.pp_print_string f "SKIP"
  | TryEntry {loc} ->
      F.fprintf f "TRY_ENTRY; [%a]" Location.pp loc
  | TryExit {loc} ->
      F.fprintf f "TRY_EXIT; [%a]" Location.pp loc
  | VariableLifetimeBegins (pvar, typ, loc) ->
      F.fprintf f "VARIABLE_DECLARED(%a:%a); [%a]" Pvar.pp_value pvar (Typ.pp_full pe) typ
        Location.pp loc


let pp_instr ~print_types pe0 f instr =
  let pp_typ = if print_types then Typ.pp_full else Typ.pp in
  color_wrapper pe0 f instr ~f:(fun pe f instr ->
      match instr with
      | Load {id; e; typ; loc} ->
          F.fprintf f "%a=*%a:%a [%a]" Ident.pp id (Exp.pp_diff ~print_types pe) e (pp_typ pe0) typ
            Location.pp loc
      | Store {e1; typ; e2; loc} ->
          F.fprintf f "*%a:%a=%a [%a]" (Exp.pp_diff ~print_types pe) e1 (pp_typ pe0) typ
            (Exp.pp_diff ~print_types pe) e2 Location.pp loc
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


let d_instr (i : instr) = L.d_pp_with_pe ~color:Pp.Green (pp_instr ~print_types:true) i
