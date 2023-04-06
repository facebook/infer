(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Returns a human-readable representation of the expression for debugging the front-end or the
    validator. *)
let rec exp_pp fmt (expr : Exp.t) =
  match expr with
  | Var id ->
      F.fprintf fmt "Var(%a)" Ident.pp id
  | UnOp (op, e1, _) ->
      F.fprintf fmt "UnOp(%s, %a, _)" (Unop.to_string op) exp_pp e1
  | BinOp (op, e1, e2) ->
      F.fprintf fmt "BinOp(%s, %a, %a)" (Binop.str Pp.text op) exp_pp e1 exp_pp e2
  | Exn e ->
      F.fprintf fmt "Exn(%a)" exp_pp e
  | Closure exp' ->
      F.fprintf fmt "Closure(%a)" Exp.pp_closure exp'
  | Const c ->
      F.fprintf fmt "Const(%a)" (Const.pp Pp.text) c
  | Cast (typ, e) ->
      F.fprintf fmt "Cast(%a, %a)" (Typ.pp Pp.text) typ exp_pp e
  | Lvar pvar ->
      F.fprintf fmt "Lvar(%a)" (Pvar.pp Pp.text) pvar
  | Lfield (base, fldname, _) ->
      F.fprintf fmt "Lfield(%a,%s,_)" exp_pp base (Fieldname.to_string fldname)
  | Lindex (base, idx) ->
      F.fprintf fmt "Lindex(%a, %a)" exp_pp base exp_pp idx
  | Sizeof _ ->
      F.fprintf fmt "%a" Exp.pp expr


(** Checks whether the given operator is a relation comparison operator. *)
let is_compare_exp (op : Binop.t) =
  match op with
  | Lt | Gt | Le | Ge | Eq | Ne ->
      true
  | PlusA _
  | PlusPI
  | MinusA _
  | MinusPI
  | MinusPP
  | Mult _
  | DivI
  | DivF
  | Mod
  | Shiftlt
  | Shiftrt
  | BAnd
  | BXor
  | BOr
  | LAnd
  | LOr ->
      false


let is_minimal_leaf_exp (exp : Exp.t) =
  match exp with Var _ -> true | Const _ -> true | _ -> false


(** Returns an error string detailing the offending instruction and/or offending expressions. *)
let instr_error_msg ~call_exp_conforms ~arg_exp_conforms (instr : Sil.instr) =
  match instr with
  | Load {id; e; typ= _} ->
      F.asprintf "Non-conforming Load(%s,%a,_)" (Ident.to_string id) exp_pp e
  | Store {e1; typ= _; e2; loc= _} ->
      F.asprintf "Non-conforming Store(%a, _, %a)" exp_pp e1 exp_pp e2
  | Prune (e, _, _, _) ->
      F.asprintf "Non-conforming Prune(%a,_, _, _) " exp_pp e
  | Metadata _ ->
      F.asprintf "Non-conforming Metadata(%a)" (Sil.pp_instr ~print_types:false Pp.text) instr
  | Call ((_, _), call_exp, args, _, _) when call_exp_conforms call_exp ->
      let bad_args =
        List.foldi args ~init:[] ~f:(fun idx accum arg ->
            let exp, _ = arg in
            if arg_exp_conforms exp then accum else (idx, exp) :: accum )
      in
      if List.is_empty bad_args then ""
      else
        let bad_args = List.rev bad_args in
        let msg =
          List.fold bad_args ~init:"Non-conforming argument(s): " ~f:(fun accum (idx, exp) ->
              let exp_str = F.asprintf "#%i=%a " idx exp_pp exp in
              accum ^ exp_str )
        in
        msg
  | Call ((_, _), e, _, _, _) ->
      F.asprintf "Non-conforming Call expression: %a" exp_pp e


(** A type for instruction validators. *)
type validator_callbacks =
  { instr_conforms: Sil.instr -> bool
        (** Checks whether an instruction conforms to the front-end's subset. *)
  ; call_exp_conforms: Exp.t -> bool
        (** Checks whether a call expression conforms to the front-end's subset. This is used to
            emit error messages. *)
  ; arg_exp_conforms: Exp.t -> bool
        (** Checks whether an argument expression of a call conforms to the front-end's subset. This
            is used to emit error messages. *) }

(** A validator for the Clang front-end (C/C++/ObjC). *)
module Clang : sig
  val callbacks : validator_callbacks
end = struct
  let rec heap_exp (exp : Exp.t) =
    match exp with
    | Lvar _ | Var _ | Const _ | Sizeof _ ->
        true
    | Cast (_, exp') ->
        heap_exp exp'
    | UnOp (Neg, exp', _) ->
        heap_exp exp'
    | UnOp (BNot, exp', _) ->
        heap_exp exp'
    | BinOp (_, e1, e2) ->
        heap_exp e1 && heap_exp e2
    | Lfield (exp', _, _) ->
        heap_exp exp'
    | Lindex (base, index) ->
        heap_exp base && heap_exp index
    | Closure _ ->
        true
    | _ ->
        false


  let rec prune_exp (exp : Exp.t) =
    match exp with
    | UnOp (LNot, e, _) ->
        prune_exp e
    | BinOp (_, e1, e2) ->
        prune_exp e1 && prune_exp e2
    | _ ->
        heap_exp exp


  let call_exp_conforms (exp : Exp.t) =
    match exp with Const _ | Closure _ | Var _ -> true | _ -> false


  let arg_exp_conforms exp = heap_exp exp

  let instr_conforms (instr : Sil.instr) =
    match instr with
    | Load {id= _; e; typ= _} when heap_exp e ->
        true
    | Store {e1; typ= _; e2} when heap_exp e2 ->
        heap_exp e1
    | Store {e1= Lvar _; typ= _; e2= Exn (Var _)} ->
        true
    | Call ((_, _), call_exp, args, _, _) ->
        call_exp_conforms call_exp && List.for_all args ~f:(fun (exp', _) -> arg_exp_conforms exp')
    | Prune (e, _, _, _) ->
        prune_exp e
    | Metadata _ ->
        true
    | _ ->
        false


  let callbacks = {instr_conforms; call_exp_conforms; arg_exp_conforms}
end

(** A validator the Java bytecode front-end. *)
module Java : sig
  val callbacks : validator_callbacks
end = struct
  let call_exp_conforms (exp : Exp.t) = match exp with Const _ | Var _ -> true | _ -> false

  let rec is_pure (exp : Exp.t) =
    match exp with
    | Lvar _ | Var _ | Const _ | Sizeof _ ->
        true
    | Cast (_, exp') | UnOp (Neg, exp', _) | UnOp (BNot, exp', _) ->
        is_pure exp'
    | BinOp (_, e1, e2) ->
        is_pure e1 && is_pure e2
    | _ ->
        false


  let prune_exp (exp : Exp.t) =
    match exp with
    | Const _ | Var _ ->
        true
    | UnOp (LNot, BinOp (op, e1, e2), _) ->
        is_compare_exp op && is_pure e1 && is_pure e2
    | UnOp (LNot, e, _) ->
        is_minimal_leaf_exp e
    | BinOp (op, e1, e2) ->
        is_compare_exp op && is_pure e1 && is_pure e2
    | _ ->
        false


  let instr_conforms (instr : Sil.instr) =
    match instr with
    | Load {id= _; e; typ= _} when is_pure e ->
        true
    | Load {id= _; e= Lfield (exp', _, _); typ= _} when is_pure exp' ->
        true
    | Load {id= _; e= Lindex (e1, e2); typ= _} when is_pure e1 && is_pure e2 ->
        true
    | Store {e1= Lvar _; typ= _; e2} when is_pure e2 ->
        true
    | Store {e1= Lvar _; typ= _; e2= Exn (Var _)} ->
        true
    | Store {e1= Lfield (Var _, _, _); typ= _; e2} ->
        is_pure e2
    | Store {e1= Lfield (Lvar _, _, _); typ= _; e2} ->
        is_pure e2
    | Store {e1= Lindex (Var _, index); typ= _; e2} ->
        is_pure index && is_pure e2
    | Call ((_, _), call_exp, args, _, _) ->
        call_exp_conforms call_exp && List.for_all args ~f:(fun (exp', _) -> is_pure exp')
    | Prune (e, _, _, _) ->
        prune_exp e
    | Metadata _ ->
        true
    | _ ->
        false


  let callbacks = {instr_conforms; call_exp_conforms; arg_exp_conforms= is_pure}
end

(* A validator for the Erlang front-end. *)
module Erlang : sig
  val callbacks : validator_callbacks
end = struct
  let rec prune_exp (exp : Exp.t) =
    match exp with
    | Const _ | Var _ ->
        true
    | UnOp (LNot, exp', _) ->
        prune_exp exp'
    | BinOp (LAnd, e1, e2) ->
        prune_exp e1 && prune_exp e2
    | BinOp (LOr, e1, e2) ->
        prune_exp e1 && prune_exp e2
    | BinOp (Eq, Var _, Const _) | BinOp (Eq, Var _, Var _) ->
        true
    | _ ->
        false


  let rec is_pure (exp : Exp.t) =
    match exp with
    | Lvar _ | Var _ | Const _ | Sizeof _ ->
        true
    | UnOp (_, exp', _) ->
        is_pure exp'
    | BinOp (_, e1, e2) ->
        is_pure e1 && is_pure e2
    | _ ->
        false


  let call_exp_conforms (exp : Exp.t) =
    match exp with Const _ | Closure _ | Var _ -> true | _ -> false


  let instr_conforms (instr : Sil.instr) =
    match instr with
    | Load {id= _; e= Lfield (exp', _, _); typ= _} when is_pure exp' ->
        true
    | Load {id= _; e= Lindex (e1, e2); typ= _} when is_pure e1 && is_pure e2 ->
        true
    | Load {id= _; e; typ= _} when is_pure e ->
        true
    | Store {e1= Lvar _; typ= _; e2= Closure _} ->
        true
    | Store {e1= Lvar _; typ= _; e2} when is_pure e2 ->
        true
    | Store {e1= Lvar _; typ= _; e2= Exn (Var _)} ->
        true
    | Store {e1= Lfield (Var _, _, _); typ= _; e2} ->
        is_pure e2
    | Store {e1= Lfield (Lvar _, _, _); typ= _; e2} ->
        is_pure e2
    | Store {e1= Lindex (Var _, index); typ= _; e2} ->
        is_pure index && is_pure e2
    | Prune (e, _, _, _) ->
        prune_exp e
    | Call ((_, _), call_exp, args, _, _) ->
        call_exp_conforms call_exp && List.for_all args ~f:(fun (exp', _) -> is_pure exp')
    | Metadata _ ->
        true
    | _ ->
        false


  let callbacks = {instr_conforms; call_exp_conforms; arg_exp_conforms= is_pure}
end

let error_counter = ref 0

let error_limit = 10000

let report_error proc_desc err_log instr msg =
  let get_loc (instr : Sil.instr) =
    match instr with Load {loc} | Store {loc} | Call (_, _, _, loc, _) -> Some loc | _ -> None
  in
  let loc_opt = get_loc instr in
  let loc = Option.value loc_opt ~default:Location.dummy in
  let ltr = [Errlog.make_trace_element 0 loc "Unable to validate this statement" []] in
  let description =
    Format.asprintf "SIL Instruction %a does not conform to the front-end invariants.\nDetails: %s"
      (Sil.pp_instr ~print_types:true Pp.text)
      instr msg
  in
  Reporting.log_issue proc_desc err_log ~loc ~ltr SILValidation IssueType.invalid_sil description


let checker (language : Language.t) ({proc_desc; err_log} : IntraproceduralAnalysis.t) =
  let {instr_conforms; call_exp_conforms; arg_exp_conforms} =
    match language with
    | Java ->
        Java.callbacks
    | Clang ->
        Clang.callbacks
    | Erlang ->
        Erlang.callbacks
    | _ ->
        (* Clang is the most general, i.e., least restrictive validator. *)
        Clang.callbacks
  in
  if !error_counter < error_limit then
    Procdesc.iter_instrs
      (fun _ instr ->
        if not (instr_conforms instr) then (
          let msg = instr_error_msg ~call_exp_conforms ~arg_exp_conforms instr in
          report_error proc_desc err_log instr msg ;
          error_counter := !error_counter + 1 ) )
      proc_desc
