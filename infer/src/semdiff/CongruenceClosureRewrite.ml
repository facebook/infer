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

  module Set : Stdlib.Set.S with type elt = t
end = struct
  type t = string [@@deriving compare, equal]

  let of_string v = v

  let pp = F.pp_print_string

  module Map = Stdlib.Map.Make (String)
  module Set = Stdlib.Set.Make (String)
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


  let vars pat =
    let rec aux acc = function
      | Var var ->
          Var.Set.add var acc
      | Term {args} ->
          List.fold args ~init:acc ~f:aux
    in
    aux Var.Set.empty pat |> Var.Set.elements


  let add_or_empty ~debug cc subst var atom' =
    match Var.Map.find_opt var subst with
    | None ->
        if debug then F.printf "adding %a=%a@." Var.pp var (CC.pp_nested_term cc) atom' ;
        Var.Map.add var atom' subst |> SubstSet.singleton
    | Some already' when phys_equal atom' already' ->
        SubstSet.singleton subst
    | Some _ ->
        SubstSet.empty


  let rec e_match_at_loop ~debug cc subst pat atom =
    let atom' = CC.representative cc atom in
    if debug then
      F.printf "e-maching %a with %a (repr is %a)@." pp pat (CC.pp_nested_term cc) atom CC.Atom.pp
        atom' ;
    match pat with
    | Var var ->
        add_or_empty ~debug cc subst var atom'
    | Term {header; args} ->
        loop_term_args ~debug cc subst header (List.rev args) atom'


  and loop_term_args ~debug cc subst header rev_args atom' =
    if debug then
      F.printf "e-matching term pattern header=%a args=[%a]@." CC.pp_header header (Pp.comma_seq pp)
        rev_args ;
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
            let substs = e_match_at_loop ~debug cc subst pat right' in
            SubstSet.fold
              (fun subst acc ->
                SubstSet.union (loop_term_args ~debug cc subst header rev_args left') acc )
              substs acc )
          ~init:SubstSet.empty


  let e_match_at ?(debug = false) cc pat atom =
    e_match_at_loop ~debug cc Var.Map.empty pat atom |> SubstSet.elements


  type ellipsis = {header: CC.header; arg: t}

  let pp_ellipsis fmt {header; arg} =
    F.fprintf fmt "@[<hv4>(%a@ ... %a ...)@]" CC.pp_header header pp arg


  let e_match_ellipsis_at cc {header; arg} atom =
    let rec loop kept_arg_atoms subst atom =
      let matched_atoms =
        if CC.is_equiv cc atom (header :> CC.Atom.t) then
          CC.mk_term cc header kept_arg_atoms |> CC.Atom.Set.singleton
        else CC.Atom.Set.empty
      in
      let atom' = CC.representative cc atom in
      let app_equations = CC.equiv_terms cc atom' in
      List.fold app_equations
        ~f:(fun acc {CC.left; right} ->
          let right' = CC.representative cc right in
          let left' = CC.representative cc left in
          let substs = e_match_at_loop ~debug:false cc subst arg right' in
          if SubstSet.is_empty substs then loop (right' :: kept_arg_atoms) subst left'
          else
            SubstSet.fold
              (fun subst acc -> CC.Atom.Set.union (loop kept_arg_atoms subst left') acc)
              substs acc )
        ~init:matched_atoms
    in
    loop [] Var.Map.empty atom |> CC.Atom.Set.elements


  let e_match ?(debug = false) cc pat ~f =
    match pat with
    | Var _ ->
        L.die InternalError "should not happen"
    | Term {header} ->
        CC.iter_term_roots cc header ~f:(fun atom ->
            e_match_at ~debug cc pat atom |> List.iter ~f:(fun subst -> f atom subst) )


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
  type t = Regular of {lhs: Pattern.t; rhs: Pattern.t}

  let pp fmt = function
    | Regular {lhs; rhs} ->
        F.fprintf fmt "@[<hv>%a@ ==>@ %a@]" Pattern.pp lhs Pattern.pp rhs


  let apply_at ?(debug = false) cc rule atom =
    match rule with
    | Regular {lhs; rhs} ->
        let substs = Pattern.e_match_at cc lhs atom in
        List.iteri substs ~f:(fun i subst ->
            if debug then F.printf "subst #%d = %a@." i (pp_subst cc) subst ;
            let rhs_term = Pattern.to_term cc subst rhs in
            if debug then F.printf "rhs_term = %a@." (CC.pp_nested_term cc) rhs_term ;
            CC.merge cc atom (CC.Atom rhs_term) ) ;
        List.length substs


  let rewrite_app_right_neutral cc =
    if CC.app_right_neutral_exists cc then
      CC.iter_app_roots cc ~f:(fun atom ->
          let atom' = CC.representative cc atom in
          let app_equations = CC.equiv_terms cc atom' in
          List.iter app_equations ~f:(fun {CC.left; right} ->
              if CC.is_app_right_neutral cc right then CC.merge cc atom' (CC.Atom left) ) )


  let rewrite_once ?(debug = false) cc rules =
    CC.reset_update_count cc ;
    List.iter rules ~f:(fun rule ->
        match rule with
        | Regular {lhs; rhs} ->
            Pattern.e_match cc lhs ~f:(fun atom subst ->
                let rhs_term = Pattern.to_term cc subst rhs in
                if not (CC.is_equiv cc atom rhs_term) then
                  if debug then
                    F.printf "rewriting atom %a with rule %a and subst %a@." (CC.pp_nested_term cc)
                      atom pp rule (pp_subst cc) subst ;
                CC.merge cc atom (CC.Atom rhs_term) ) ) ;
    rewrite_app_right_neutral cc ;
    CC.get_update_count cc


  exception FuelExhausted of {round_count: int}

  let full_rewrite ?(fuel = 1 lsl 10) cc rules =
    let rec loop fuel round_count =
      if fuel <= 0 then raise (FuelExhausted {round_count})
      else
        let updates = rewrite_once cc rules in
        if Int.equal updates 0 then
          (* the last round did not change anything *)
          round_count
        else (
          assert (updates > 0) ;
          loop (fuel - updates) (round_count + 1) )
    in
    loop fuel 1
end

module Parser = struct
  type buffer = string list (* striped strings to be parser *)

  type 'a m = buffer -> ('a * buffer) list

  let ret (a : 'a) : 'a m = fun buf -> [(a, buf)]

  let bind (m : 'a m) (f : 'a -> 'b m) : 'b m =
   fun buf -> List.concat_map ~f:(fun (a, buf) -> f a buf) (m buf)


  let ( let* ) = bind

  let rec char (c : char) : unit m = function
    | [] ->
        []
    | "" :: buffer ->
        char c buffer
    | hd :: buffer when Char.equal hd.[0] c ->
        let n = String.length hd in
        [((), String.sub hd ~pos:1 ~len:(n - 1) :: buffer)]
    | _ ->
        []


  let rec all ~(except : char list) : string m =
    let rec extract from str =
      if from < String.length str then
        let c = str.[from] in
        if List.mem ~equal:Char.equal except c then from else extract (from + 1) str
      else from
    in
    function
    | [] ->
        []
    | "" :: buffer ->
        all ~except buffer
    | hd :: buffer ->
        let len = extract 0 hd in
        if len > 0 then
          let n = String.length hd in
          [(String.sub hd ~pos:0 ~len, String.sub hd ~pos:len ~len:(n - len) :: buffer)]
        else []


  let ident = all ~except:['('; ')'; '?']

  let ( + ) (m : 'a m) (n : 'a m) : 'a m = fun buf -> m buf @ n buf

  type raw = Var of string | Term of {header: string; args: raw list}

  let rec raw_pattern () : raw m =
    (let* () = char '?' in
     let* var = ident in
     ret (Var var) )
    + (let* () = char '(' in
       let* header = ident in
       let* args = args () in
       ret (Term {header; args}) )
    +
    let* header = ident in
    ret (Term {header; args= []})


  and args () : raw list m =
    (let* () = char ')' in
     ret [] )
    +
    let* arg = raw_pattern () in
    let* args = args () in
    ret (arg :: args)


  let rec raw_to_pattern cc raw : Pattern.t =
    match raw with
    | Var var ->
        Var (Var.of_string var)
    | Term {header; args} ->
        let header = CC.mk_header cc header in
        let args = List.map args ~f:(raw_to_pattern cc) in
        Term {header; args}


  let parse cc str : Pattern.t option =
    let buffer = String.split_on_chars str ~on:[' '; '\n'; '\t'] in
    match raw_pattern () buffer with
    | [] ->
        None
    | [(raw, _)] ->
        Some (raw_to_pattern cc raw)
    | _ ->
        L.die InternalError "parse error"
end

let parse_pattern = Parser.parse

module TestOnly = struct
  let e_match_pattern_at = Pattern.e_match_at

  let e_match_pattern = Pattern.e_match

  let e_match_ellipsis_at = Pattern.e_match_ellipsis_at

  let pattern_to_term = Pattern.to_term

  let apply_rule_at = Rule.apply_at

  let rewrite_rules_once = Rule.rewrite_once
end
