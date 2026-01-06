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
    | Some already' when CC.is_equiv cc atom' already' ->
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


  let e_match ?(debug = false) cc pat exclude ~f =
    match pat with
    | Var _ ->
        L.die InternalError "should not happen"
    | Term {header} ->
        CC.iter_term_roots cc header ~f:(fun atom ->
            if not (List.exists exclude ~f:(CC.is_equiv cc atom)) then
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
  type t =
    | Regular of {lhs: Pattern.t; rhs: Pattern.t; exclude: CC.Atom.t list}
    | Ellipsis of Pattern.ellipsis

  let pp fmt = function
    | Regular {lhs; rhs} ->
        F.fprintf fmt "@[<hv>%a@ ==>@ %a@]" Pattern.pp lhs Pattern.pp rhs
    | Ellipsis ellipsis ->
        F.fprintf fmt "@[<hv>%a ==>@ (%a ...)@]" Pattern.pp_ellipsis ellipsis CC.pp_header
          ellipsis.header


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
    | Ellipsis ellipsis ->
        Pattern.e_match_ellipsis_at cc ellipsis atom
        |> List.iter ~f:(fun new_atom -> CC.merge cc atom (CC.Atom new_atom)) ;
        0


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
        | Regular {lhs; rhs; exclude} ->
            Pattern.e_match cc lhs exclude ~f:(fun atom subst ->
                let rhs_term = Pattern.to_term cc subst rhs in
                if not (CC.is_equiv cc atom rhs_term) then (
                  if debug then
                    F.printf "rewriting atom %a with rule %a and subst %a@." (CC.pp_nested_term cc)
                      atom pp rule (pp_subst cc) subst ;
                  CC.merge cc atom (CC.Atom rhs_term) ) )
        | Ellipsis ellipsis ->
            CC.iter_term_roots cc ellipsis.header ~f:(fun atom ->
                Pattern.e_match_ellipsis_at cc ellipsis atom
                |> List.iter ~f:(fun new_atom -> CC.merge cc atom (CC.Atom new_atom)) ) ) ;
    rewrite_app_right_neutral cc ;
    CC.get_update_count cc


  exception FuelExhausted of {round_count: int}

  let full_rewrite ?(debug = false) ?(fuel = 1 lsl 10) cc rules =
    let rec loop fuel round_count =
      if fuel <= 0 then raise (FuelExhausted {round_count})
      else (
        if debug then F.printf "full_rewrite - round %d@." round_count ;
        let updates = rewrite_once ~debug cc rules in
        if Int.equal updates 0 then
          (* the last round did not change anything *)
          round_count
        else (
          assert (updates > 0) ;
          loop (fuel - updates) (round_count + 1) ) )
    in
    loop fuel 1
end

module Parser = struct
  type state = {pos: int (* position in the input string *); col: int; line: int}

  type error_token = EndOfBuffer | Arrow

  type error_kind = Expected | Unexpected

  type error = {kind: error_kind; token: error_token; input: string; col: int; line: int}

  let pp_error fmt {kind; token; input; col; line} =
    let lines = String.split_on_chars input ~on:['\n'; '\r'] in
    let error_line = List.nth_exn lines line in
    let show_error = String.make col ' ' in
    let pp_kind fmt = function
      | Expected ->
          F.pp_print_string fmt "expected"
      | Unexpected ->
          F.pp_print_string fmt "unexpected"
    in
    let pp_token fmt = function
      | EndOfBuffer ->
          F.pp_print_string fmt "end of buffer"
      | Arrow ->
          F.pp_print_string fmt {|arrow "==>"|}
    in
    F.fprintf fmt "paser error at line %d, col %d:@.%s@.%s^@.--> %a %a " line col error_line
      show_error pp_token token pp_kind kind


  type 'a result = Choices of ('a * state) list | Error of error

  type 'a m = string -> state -> 'a result

  let ret (a : 'a) : 'a m = fun _input state -> Choices [(a, state)]

  let bind ~if_empty (m : 'a m) (f : 'a -> 'b m) : 'b m =
   fun input state ->
    match m input state with
    | Choices [] ->
        if_empty input state
    | Choices l ->
        List.fold l ~init:(Choices []) ~f:(fun res (a, state) ->
            match res with
            | Error _ ->
                res
            | Choices acc -> (
              match f a input state with
              | Error err ->
                  Error err
              | Choices l_a ->
                  Choices (l_a @ acc) ) )
    | Error err ->
        Error err


  let zero : 'a m = fun _ _ -> Choices []

  let ( let* ) m f = bind ~if_empty:zero m f

  let error kind token : 'a m = fun input {col; line} -> Error {input; col; line; kind; token}

  let ( let*! ) (m, token) f = bind ~if_empty:(error Expected token) m f

  let ( ++ ) (m : 'a m) (n : 'a m) : 'a m =
   fun input state ->
    match m input state with
    | Error err ->
        Error err
    | Choices l1 -> (
      match n input state with Error err -> Error err | Choices l2 -> Choices (l1 @ l2) )


  let step input {pos; col; line} =
    assert (pos < String.length input) ;
    match input.[pos] with
    | '\n' | '\r' ->
        {pos= pos + 1; col= 0; line= line + 1}
    | _ ->
        {pos= pos + 1; col= col + 1; line}


  let rec find_next_non_white_space_char input ({pos} as state) =
    if String.length input <= pos then None
    else if Char.is_whitespace input.[pos] then
      find_next_non_white_space_char input (step input state)
    else Some state


  let run (m : unit -> 'a m) input : ('a, error) Stdlib.result =
    match m () input {pos= 0; col= 0; line= 0} with
    | Error err ->
        Error err
    | Choices [(a, state)] -> (
      match find_next_non_white_space_char input state with
      | None ->
          Ok a
      | Some {col; line} ->
          Error {kind= Expected; token= EndOfBuffer; col; line; input} )
    | Choices _ ->
        L.die InternalError "should not happen"


  let end_of_buffer () : unit m =
   fun input state ->
    match find_next_non_white_space_char input state with
    | None ->
        ret () input state
    | Some _ ->
        zero input state


  let char (c : char) : unit m =
   fun input state ->
    match find_next_non_white_space_char input state with
    | None ->
        Choices []
    | Some state ->
        if Char.equal input.[state.pos] c then Choices [((), step input state)] else Choices []


  let reserved_chars = ['('; ')']

  let raw_ident ~expected : string m =
   fun input state ->
    let rec aux pos =
      if String.length input <= pos then pos
      else
        let c = input.[pos] in
        if Char.is_whitespace c || List.mem ~equal:Char.equal reserved_chars c then pos
        else aux (pos + 1)
    in
    match find_next_non_white_space_char input state with
    | None ->
        Choices []
    | Some state ->
        let end_pos = aux state.pos in
        if state.pos < end_pos then
          let len = end_pos - state.pos in
          let ident = String.sub input ~pos:state.pos ~len in
          let new_state = {state with pos= end_pos; col= state.col + len} in
          match expected with
          | None ->
              Choices [(ident, new_state)]
          | Some (expected, _token) when String.equal expected ident ->
              Choices [(ident, new_state)]
          | Some (_expected, token) ->
              error Expected token input state
        else Choices []


  let ellipsis : unit m =
    let* str = raw_ident ~expected:None in
    if String.equal str "..." then ret () else zero


  let rule_arrow_or_error : unit m =
    let*! _ = (raw_ident ~expected:(Some ("==>", Arrow)), Arrow) in
    ret ()


  let reserved = ["..."; "==>"]

  let var : string m =
    let* str = raw_ident ~expected:None in
    let n = String.length str in
    if Char.equal str.[0] '?' && n > 1 then ret (String.sub ~pos:1 ~len:(n - 1) str) else zero


  let ident : string m =
    let* str = raw_ident ~expected:None in
    if Char.equal str.[0] '?' || List.mem ~equal:String.equal reserved str then zero else ret str


  type raw_pattern = Var of string | Term of {header: string; args: raw_pattern list}

  let rec raw_pattern () : raw_pattern m =
    (let* var = var in
     ret (Var var) )
    ++ (let* () = char '(' in
        let* header = ident in
        let* args = args () in
        ret (Term {header; args}) )
    ++ let* header = ident in
       ret (Term {header; args= []})


  and args () : raw_pattern list m =
    (let* () = char ')' in
     ret [] )
    ++ (let* arg = raw_pattern () in
        let* args = args () in
        ret (arg :: args) )
    ++ let* () = end_of_buffer () in
       error Unexpected EndOfBuffer


  type raw_ellipsis = {header: string; arg: raw_pattern}

  let raw_ellipsis : raw_ellipsis m =
    let* () = char '(' in
    let* header = ident in
    let* () = ellipsis in
    let* arg = raw_pattern () in
    let* () = ellipsis in
    let* () = char ')' in
    ret {header; arg}


  type raw_rule = Regular of {lhs: raw_pattern; rhs: raw_pattern} | Ellipsis of raw_ellipsis

  let raw_rule () : raw_rule m =
    (let* lhs = raw_pattern () in
     let* () = rule_arrow_or_error in
     let* rhs = raw_pattern () in
     ret (Regular {lhs; rhs}) )
    ++ let* raw_ellipsis = raw_ellipsis in
       let* () = rule_arrow_or_error in
       let* () = char '(' in
       let* _ = ident in
       let* () = ellipsis in
       let* () = char ')' in
       ret (Ellipsis raw_ellipsis)


  let rec raw_to_pattern cc raw : Pattern.t =
    match raw with
    | Var var ->
        Var (Var.of_string var)
    | Term {header; args} ->
        let header = CC.mk_header cc header in
        let args = List.map args ~f:(raw_to_pattern cc) in
        Term {header; args}


  let raw_to_rule cc raw : Rule.t =
    match raw with
    | Regular {lhs; rhs} ->
        Regular {lhs= raw_to_pattern cc lhs; rhs= raw_to_pattern cc rhs; exclude= []}
    | Ellipsis {header; arg} ->
        Ellipsis {header= CC.mk_header cc header; arg= raw_to_pattern cc arg}


  let parse_pattern cc input : (Pattern.t, error) Stdlib.result =
    run raw_pattern input |> Result.map ~f:(raw_to_pattern cc)


  let parse_rule cc input : (Rule.t, error) Stdlib.result =
    run raw_rule input |> Result.map ~f:(raw_to_rule cc)
end

type parse_error = Parser.error

let pp_parse_error = Parser.pp_error

let parse_pattern = Parser.parse_pattern

let parse_rule = Parser.parse_rule

module TestOnly = struct
  let e_match_pattern_at = Pattern.e_match_at

  let e_match_pattern ?(debug = false) cc pat ~f = Pattern.e_match ~debug cc pat [] ~f

  let e_match_ellipsis_at = Pattern.e_match_ellipsis_at

  let pattern_to_term = Pattern.to_term

  let apply_rule_at = Rule.apply_at

  let rewrite_rules_once = Rule.rewrite_once
end
