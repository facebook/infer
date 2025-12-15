(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module CC = CongruenceClosureSolver

module Var : sig
  type t = private string [@@deriving compare, equal]

  val of_string : string -> t

  val pp : F.formatter -> t -> unit

  module Map : Stdlib.Map.S with type key = t
end = struct
  type t = string [@@deriving compare, equal]

  let of_string v = v

  let pp = F.pp_print_string

  module Map = Stdlib.Map.Make (String)
end

type subst = CC.Atom.t Var.Map.t [@@deriving compare]

let pp_subst cc fmt subst =
  let pp fmt (var, atom) = F.fprintf fmt "%a: %a" Var.pp var (CC.pp_nested_term cc) atom in
  F.fprintf fmt "{%a}" (Pp.comma_seq pp) (Var.Map.bindings subst)


let mk_subst bindings =
  (* for testing only *)
  Var.Map.of_list bindings


let lookup_subst cc subst var =
  match Var.Map.find_opt var subst with
  | Some atom ->
      atom
  | None ->
      L.die InternalError "var %a is not in subst %a" Var.pp var (pp_subst cc) subst


module SubstSet = Stdlib.Set.Make (struct
  type t = subst

  let compare = compare_subst
end)

module Pattern = struct
  type t = Var of Var.t | Term of {header: CC.header; args: t list}

  let rec pp fmt = function
    | Var v ->
        F.fprintf fmt "?%a" Var.pp v
    | Term {header; args= []} ->
        F.fprintf fmt "%a" CC.pp_header header
    | Term {header; args} ->
        F.fprintf fmt "@[<hv4>(%a@ %a)@]" CC.pp_header header (Pp.seq ~sep_html:"@ " pp) args


  let e_match ?(debug = false) cc pat atom =
    let add_or_empty subst var atom' =
      match Var.Map.find_opt var subst with
      | None ->
          if debug then F.printf "adding %a=%a@." Var.pp var (CC.pp_nested_term cc) atom' ;
          Var.Map.add var atom' subst |> SubstSet.singleton
      | Some already' when phys_equal atom' already' ->
          SubstSet.singleton subst
      | Some _ ->
          SubstSet.empty
    in
    let rec loop subst pat atom =
      let atom' = CC.representative cc atom in
      if debug then
        F.printf "e-maching %a with %a (repr is %a)@." pp pat (CC.pp_nested_term cc) atom CC.Atom.pp
          atom' ;
      match pat with
      | Var var ->
          add_or_empty subst var atom'
      | Term {header; args} ->
          loop_term_args subst header (List.rev args) atom'
    and loop_term_args subst header rev_args atom' =
      if debug then
        F.printf "e-matching term pattern header=%a args=[%a]@." CC.pp_header header
          (Pp.comma_seq pp) rev_args ;
      match rev_args with
      | [] ->
          let header_atom' = CC.representative_of_header cc header in
          if phys_equal atom' header_atom' then SubstSet.singleton subst else SubstSet.empty
      | pat :: rev_args ->
          let app_equations = CC.equiv_terms cc atom' in
          if debug then
            F.printf "app_equations(%a) = [%a]@." (CC.pp_nested_term cc) atom'
              (Pp.comma_seq (fun fmt {CC.rhs} -> CC.pp_nested_term cc fmt rhs))
              app_equations ;
          List.fold app_equations
            ~f:(fun acc {CC.left; right} ->
              let right' = CC.representative cc right in
              let left' = CC.representative cc left in
              let substs = loop subst pat right' in
              SubstSet.fold
                (fun subst acc -> SubstSet.union (loop_term_args subst header rev_args left') acc)
                substs acc )
            ~init:SubstSet.empty
    in
    loop Var.Map.empty pat atom |> SubstSet.elements


  let to_term cc subst pat =
    let rec pattern = function
      | Var var ->
          lookup_subst cc subst var |> CC.representative cc
      | Term {header; args} ->
          term_args header (List.rev args) []
    and term_args header rev_args atoms =
      match rev_args with
      | [] ->
          CC.mk_term cc header atoms
      | pat :: rev_args ->
          term_args header rev_args (pattern pat :: atoms)
    in
    pattern pat
end

module Rule = struct
  type t = {lhs: Pattern.t; rhs: Pattern.t}

  let pp fmt {lhs; rhs} = F.fprintf fmt "@[<hv>%a@ ==>@ %a@]" Pattern.pp lhs Pattern.pp rhs

  let apply ?(debug = false) cc {lhs; rhs} atom =
    let substs = Pattern.e_match cc lhs atom in
    List.iteri substs ~f:(fun i subst ->
        if debug then F.printf "subst #%d = %a@." i (pp_subst cc) subst ;
        let rhs_term = Pattern.to_term cc subst rhs in
        if debug then F.printf "rhs_term = %a@." (CC.pp_nested_term cc) rhs_term ;
        CC.merge cc atom (CC.Atom rhs_term) ) ;
    List.length substs
end
