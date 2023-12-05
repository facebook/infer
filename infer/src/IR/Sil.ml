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
[@@deriving compare, equal, hash, normalize]

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
  | VariableLifetimeBegins of
      {pvar: Pvar.t; typ: Typ.t; loc: Location.t; is_cpp_structured_binding: bool}
[@@deriving compare, equal, hash, normalize]

type ret_var = Ident.t * Typ.t [@@deriving compare, equal, hash, normalize]

type param = Exp.t * Typ.t [@@deriving compare, equal, hash, normalize]

type instr =
  | Load of {id: Ident.t; e: Exp.t; typ: Typ.t; loc: Location.t}
  | Store of {e1: Exp.t; typ: Typ.t; e2: Exp.t; loc: Location.t}
  | Prune of Exp.t * Location.t * bool * if_kind
  | Call of ret_var * Exp.t * param list * Location.t * CallFlags.t
  | Metadata of instr_metadata
[@@deriving compare, equal, hash, normalize]

let skip_instr = Metadata Skip

let instr_is_auxiliary = function
  | Load _ | Store _ | Prune _ | Call _ ->
      false
  | Metadata _ ->
      true


let color_wrapper ~f = if Config.print_using_diff then Pp.color_wrapper ~f else f

let pp_exp_typ ~print_types pe pp_typ f (e, t) =
  F.fprintf f "%a:%a" (Exp.pp_diff ~print_types pe) e (pp_typ pe) t


let location_of_instr_metadata = function
  | Abstract loc
  | CatchEntry {loc}
  | ExitScope (_, loc)
  | Nullify (_, loc)
  | TryEntry {loc}
  | TryExit {loc}
  | VariableLifetimeBegins {loc} ->
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
  | VariableLifetimeBegins {pvar} ->
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
  | VariableLifetimeBegins {pvar; typ; loc; is_cpp_structured_binding} ->
      F.fprintf f "VARIABLE_DECLARED(%a:%a%t); [%a]" Pvar.pp_value pvar (Typ.pp_full pe) typ
        (fun f -> if is_cpp_structured_binding then F.pp_print_string f ", cpp_structured_binding")
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
            (Pp.comma_seq (pp_exp_typ ~print_types pe pp_typ))
            arg_ts CallFlags.pp cf Location.pp loc
      | Metadata metadata ->
          pp_instr_metadata pe0 f metadata )


let d_instr (i : instr) = L.d_pp_with_pe ~color:Pp.Green (pp_instr ~print_types:true) i

(** Shorthands to carry expression maps through chained structural equalities below*)
let ( &&* ) (equality, exp_map) f = if equality then f exp_map else (false, exp_map)

let ( &&+ ) acc x = acc &&* Tuple2.create x

(** compare expressions from different procedures without considering loc's, ident's, and pvar's.
    the [exp_map] param gives a mapping of names used in the procedure of [e] to names used in the
    procedure of [e'] *)
let rec exp_equal_structural e e' exp_map =
  let equal_exps_with_map e e' exp_map =
    match Exp.Map.find_opt e exp_map with
    | Some mapped_e ->
        (Exp.equal mapped_e e', exp_map)
    | None ->
        (* assume e and e' equal, enforce by adding to [exp_map] *)
        (true, Exp.Map.add e e' exp_map)
  in
  match ((e : Exp.t), (e' : Exp.t)) with
  | Var _, Var _ ->
      equal_exps_with_map e e' exp_map
  | UnOp (o, e, typ), UnOp (o', e', typ') ->
      (Unop.equal o o', exp_map) &&* exp_equal_structural e e' &&+ [%equal: Typ.t option] typ typ'
  | BinOp (o, l, r), BinOp (o', l', r') ->
      (Binop.equal o o', exp_map) &&* exp_equal_structural l l' &&* exp_equal_structural r r'
  | Cast (t, e), Cast (t', e') ->
      exp_equal_structural e e' exp_map &&+ Typ.equal t t'
  | Lvar _, Lvar _ ->
      equal_exps_with_map e e' exp_map
  | Lfield (e, f, t), Lfield (e', f', t') ->
      exp_equal_structural e e' exp_map &&+ (Fieldname.equal f f' && Typ.equal t t')
  | Lindex (e, f), Lindex (e', f') ->
      exp_equal_structural e e' exp_map &&* exp_equal_structural f f'
  | _ ->
      (Exp.equal e e', exp_map)


let exp_typ_equal_structural (e, t) (e', t') exp_map =
  exp_equal_structural e e' exp_map &&+ Typ.equal t t'


(** Compare instructions from different procedures without considering [loc]s, [ident]s, [pvar]s, or
    [try_id]s. The [exp_map] parameter gives a mapping of names used in the first [instr] to those
    used in the second, and the returned [exp_map] includes any additional mappings inferred from
    this comparison. *)
let equal_structural_instr instr instr' exp_map =
  let id_typ_equal_structural (id, typ) (id', typ') exp_map =
    exp_equal_structural (Var id) (Var id') exp_map &&+ Typ.equal typ typ'
  in
  let var_list_equal_structural ids ids' exp_map =
    if Int.equal (List.length ids) (List.length ids') then
      List.fold2_exn
        ~f:(fun acc v v' -> acc &&* exp_equal_structural (Var.to_exp v) (Var.to_exp v'))
        ~init:(true, exp_map) ids ids'
    else (false, exp_map)
  in
  match (instr, instr') with
  | Load {id; e; typ; loc= _}, Load {id= id'; e= e'; typ= typ'; loc= _} ->
      exp_equal_structural (Var id) (Var id') exp_map
      &&* exp_equal_structural e e' &&+ Typ.equal typ typ'
  | Store {e1; typ; e2; loc= _}, Store {e1= e1'; typ= typ'; e2= e2'; loc= _} ->
      (Typ.equal typ typ', exp_map) &&* exp_equal_structural e1 e1' &&* exp_equal_structural e2 e2'
  | Prune (cond, _, true_branch, ik), Prune (cond', _, true_branch', ik') ->
      exp_equal_structural cond cond' exp_map
      &&+ (Bool.equal true_branch true_branch' && equal_if_kind ik ik')
  | Call (ret_id, e, arg_ts, _, cf), Call (ret_id', e', arg_ts', _, cf') ->
      let args_equal_structural args args' exp_map =
        if Int.equal (List.length args) (List.length args') then
          List.fold2_exn
            ~f:(fun acc arg arg' -> acc &&* exp_typ_equal_structural arg arg')
            ~init:(true, exp_map) args args'
        else (false, exp_map)
      in
      id_typ_equal_structural ret_id ret_id' exp_map
      &&* exp_equal_structural e e'
      &&* args_equal_structural arg_ts arg_ts'
      &&+ CallFlags.equal cf cf'
  | Metadata (Nullify (pvar, _)), Metadata (Nullify (pvar', _)) ->
      exp_equal_structural (Lvar pvar) (Lvar pvar') exp_map
  | Metadata (Abstract _), Metadata (Abstract _) ->
      (true, exp_map)
  | Metadata (ExitScope (temps, _)), Metadata (ExitScope (temps', _)) ->
      var_list_equal_structural temps temps' exp_map
  | ( Metadata (VariableLifetimeBegins {pvar= pv; typ= t})
    , Metadata (VariableLifetimeBegins {pvar= pv'; typ= t'}) ) ->
      exp_equal_structural (Lvar pv) (Lvar pv') exp_map &&+ Typ.equal t t'
  | Metadata (CatchEntry _), Metadata (CatchEntry _)
  | Metadata (TryEntry _), Metadata (TryEntry _)
  | Metadata (TryExit _), Metadata (TryExit _) ->
      (* ignore [try_id] of exception-handling metadata nodes, just checking for structural equality *)
      (true, exp_map)
  | _ ->
      (equal_instr instr instr', exp_map)
