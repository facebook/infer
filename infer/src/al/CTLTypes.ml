(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Transition labels used for example to switch from decl to stmt *)
type transitions =
  | AccessorForProperty of ALVar.alexp  (** decl to decl *)
  | Body  (** decl to stmt *)
  | FieldName of ALVar.alexp  (** stmt to stmt, decl to decl *)
  | Fields  (** stmt to stmt, decl to decl *)
  | InitExpr  (** decl to stmt *)
  | Super  (** decl to decl *)
  | ParameterName of ALVar.alexp  (** stmt to stmt, decl to decl *)
  | ParameterPos of ALVar.alexp  (** stmt to stmt, decl to decl *)
  | Parameters  (** stmt to stmt, decl to decl *)
  | Cond
  | PointerToDecl  (** stmt to decl *)
  | Protocol  (** decl to decl *)
  | Sibling  (** decl to decl *)
  | SourceExpr
[@@deriving compare]

let is_transition_to_successor trans =
  match trans with
  | Body
  | InitExpr
  | FieldName _
  | Fields
  | ParameterName _
  | ParameterPos _
  | Parameters
  | Cond
  | SourceExpr ->
      true
  | Super | PointerToDecl | Protocol | AccessorForProperty _ | Sibling ->
      false


(* In formulas below prefix
   "E" means "exists a path"
   "A" means "for all path" *)

type t =
  (* A ctl formula *)
  | True
  | False
  (* not really necessary but it makes it evaluation faster *)
  | Atomic of CPredicates.t
  | Not of t
  | And of t * t
  | AndWithWitnesses of t * t * CPredicates.t
  | Or of t * t
  | Implies of t * t
  | InNode of ALVar.alexp list * t
  | AX of transitions option * t
  | EX of transitions option * t
  | AF of transitions option * t
  | EF of transitions option * t
  | AG of transitions option * t
  | EG of transitions option * t
  | AU of transitions option * t * t
  | EU of transitions option * t * t
  | EH of ALVar.alexp list * t
  | ET of ALVar.alexp list * transitions option * t
  | InObjCClass of t * t
[@@deriving compare]

let has_transition phi =
  match phi with
  | True
  | False
  | Atomic _
  | Not _
  | And _
  | Or _
  | Implies _
  | InNode _
  | EH _
  | InObjCClass _
  | AndWithWitnesses _ ->
      false
  | AX (trans_opt, _)
  | AF (trans_opt, _)
  | AG (trans_opt, _)
  | AU (trans_opt, _, _)
  | EX (trans_opt, _)
  | EF (trans_opt, _)
  | EG (trans_opt, _)
  | EU (trans_opt, _, _)
  | ET (_, trans_opt, _) ->
      Option.is_some trans_opt


let pp_transition fmt trans_opt =
  let pp_aux fmt trans =
    match trans with
    | AccessorForProperty kind ->
        Format.pp_print_string fmt ("AccessorForProperty " ^ ALVar.alexp_to_string kind)
    | Body ->
        Format.pp_print_string fmt "Body"
    | FieldName name ->
        Format.pp_print_string fmt ("FieldName " ^ ALVar.alexp_to_string name)
    | Fields ->
        Format.pp_print_string fmt "Fields"
    | InitExpr ->
        Format.pp_print_string fmt "InitExpr"
    | Super ->
        Format.pp_print_string fmt "Super"
    | ParameterName name ->
        Format.pp_print_string fmt ("ParameterName " ^ ALVar.alexp_to_string name)
    | ParameterPos pos ->
        Format.pp_print_string fmt ("ParameterPos " ^ ALVar.alexp_to_string pos)
    | Parameters ->
        Format.pp_print_string fmt "Parameters"
    | Cond ->
        Format.pp_print_string fmt "Cond"
    | Protocol ->
        Format.pp_print_string fmt "Protocol"
    | PointerToDecl ->
        Format.pp_print_string fmt "PointerToDecl"
    | Sibling ->
        Format.pp_print_string fmt "Sibling"
    | SourceExpr ->
        Format.pp_print_string fmt "SourceExpr"
  in
  match trans_opt with Some trans -> pp_aux fmt trans | None -> Format.pp_print_char fmt '_'


(* a flag to print more or less in the dotty graph *)
let full_print = true

let rec pp_formula fmt phi =
  let nodes_to_string nl = List.map ~f:ALVar.alexp_to_string nl in
  match phi with
  | True ->
      Format.pp_print_string fmt "True"
  | False ->
      Format.pp_print_string fmt "False"
  | Atomic p ->
      CPredicates.pp_predicate fmt p
  | Not phi ->
      if full_print then Format.fprintf fmt "NOT(%a)" pp_formula phi
      else Format.pp_print_string fmt "NOT(...)"
  | And (phi1, phi2) ->
      if full_print then Format.fprintf fmt "(%a AND %a)" pp_formula phi1 pp_formula phi2
      else Format.pp_print_string fmt "(... AND ...)"
  | AndWithWitnesses (phi1, phi2, p) ->
      if full_print then
        Format.fprintf fmt "(%a AndWithWitnesses %a : %a)" pp_formula phi1 pp_formula phi2
          CPredicates.pp_predicate p
      else Format.pp_print_string fmt "(... AndWithWitnesses ...)"
  | Or (phi1, phi2) ->
      if full_print then Format.fprintf fmt "(%a OR %a)" pp_formula phi1 pp_formula phi2
      else Format.pp_print_string fmt "(... OR ...)"
  | Implies (phi1, phi2) ->
      Format.fprintf fmt "(%a ==> %a)" pp_formula phi1 pp_formula phi2
  | InNode (nl, phi) ->
      Format.fprintf fmt "IN-NODE %a: (%a)"
        (Pp.comma_seq Format.pp_print_string)
        (nodes_to_string nl) pp_formula phi
  | AX (trs, phi) ->
      Format.fprintf fmt "AX[->%a](%a)" pp_transition trs pp_formula phi
  | EX (trs, phi) ->
      Format.fprintf fmt "EX[->%a](%a)" pp_transition trs pp_formula phi
  | AF (trs, phi) ->
      Format.fprintf fmt "AF[->%a](%a)" pp_transition trs pp_formula phi
  | EF (trs, phi) ->
      Format.fprintf fmt "EF[->%a](%a)" pp_transition trs pp_formula phi
  | AG (trs, phi) ->
      Format.fprintf fmt "AG[->%a](%a)" pp_transition trs pp_formula phi
  | EG (trs, phi) ->
      Format.fprintf fmt "EG[->%a](%a)" pp_transition trs pp_formula phi
  | AU (trs, phi1, phi2) ->
      Format.fprintf fmt "A[->%a][%a UNTIL %a]" pp_transition trs pp_formula phi1 pp_formula phi2
  | EU (trs, phi1, phi2) ->
      Format.fprintf fmt "E[->%a][%a UNTIL %a]" pp_transition trs pp_formula phi1 pp_formula phi2
  | EH (arglist, phi) ->
      Format.fprintf fmt "EH[%a](%a)"
        (Pp.comma_seq Format.pp_print_string)
        (nodes_to_string arglist) pp_formula phi
  | ET (arglist, trans, phi) ->
      Format.fprintf fmt "ET[%a][%a](%a)"
        (Pp.comma_seq Format.pp_print_string)
        (nodes_to_string arglist) pp_transition trans pp_formula phi
  | InObjCClass (phi1, phi2) ->
      if full_print then Format.fprintf fmt "InObjCClass(%a, %a)" pp_formula phi1 pp_formula phi2
