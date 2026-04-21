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
module Rewrite = CongruenceClosureRewrite

let parse_rules cc str_rules : Rewrite.Rule.t list =
  List.map str_rules ~f:(fun prog ->
      match Rewrite.parse_rule cc prog with
      | Ok rule ->
          rule
      | Error err ->
          L.die InternalError "%a" Rewrite.pp_parse_error err )


let enumerate_rules =
  [ (* has_next on enumerate delegates to the underlying iterator *)
    "($builtins.py_has_next_iter ?S1 ($builtins.py_get_iter ?S2 (@enumerate ?L))) ==> \
     ($builtins.py_has_next_iter ?S1 ($builtins.py_get_iter ?S2 ?L))"
  ; (* Subscript [1] on enumerate's next element yields the next element of the underlying iterator.
       Subscript [0] (the index) is left unrewritten — when used, it blocks false equivalence. *)
    "($builtins.py_subscript ($builtins.py_next_iter ?S1 ($builtins.py_get_iter ?S2 (@enumerate \
     ?L))) ($builtins.py_make_int 1)) ==> ($builtins.py_next_iter ?S1 ($builtins.py_get_iter ?S2 \
     ?L))" ]


let dict_rules =
  [ (* Subscript [0] on items() next element yields the key = next element of keys() *)
    "($builtins.py_subscript ($builtins.py_next_iter ?S1 ($builtins.py_get_iter ?S2 (@dict_items \
     ?S3 ?D))) ($builtins.py_make_int 0)) ==> ($builtins.py_next_iter ?S1 ($builtins.py_get_iter \
     ?S2 (@dict_keys ?S3 ?D)))"
  ; (* Subscript [1] on items() next element yields the value = next element of values() *)
    "($builtins.py_subscript ($builtins.py_next_iter ?S1 ($builtins.py_get_iter ?S2 (@dict_items \
     ?S3 ?D))) ($builtins.py_make_int 1)) ==> ($builtins.py_next_iter ?S1 ($builtins.py_get_iter \
     ?S2 (@dict_values ?S3 ?D)))"
  ; (* has_next on items() delegates to keys() (same number of elements) *)
    "($builtins.py_has_next_iter ?S1 ($builtins.py_get_iter ?S2 (@dict_items ?S3 ?D))) ==> \
     ($builtins.py_has_next_iter ?S1 ($builtins.py_get_iter ?S2 (@dict_keys ?S3 ?D)))" ]


let gen_rules cc ~theta_count : Rewrite.Rule.t list =
  let theta_rules = List.init theta_count ~f:(fun i -> F.asprintf "(@theta_%d ?X ?X) ==> ?X" i) in
  parse_rules cc (("(@phi ?C ?X ?X) ==> ?X" :: theta_rules) @ enumerate_rules @ dict_rules)


(* Bisimulation: coinductive equivalence check for cyclic PEG terms (@theta).
   Uses a separate union-find (does not modify the CC) to track assumed equivalences. *)
module BisimUF = struct
  module AtomMap = Stdlib.Map.Make (CC.Atom)

  type t = {mutable parent: CC.Atom.t AtomMap.t}

  let create () = {parent= AtomMap.empty}

  let rec find uf a =
    match AtomMap.find_opt a uf.parent with
    | None ->
        a
    | Some p ->
        let root = find uf p in
        if not (phys_equal p root) then uf.parent <- AtomMap.add a root uf.parent ;
        root


  let union uf a b =
    let ra = find uf a in
    let rb = find uf b in
    if not (CC.Atom.equal ra rb) then uf.parent <- AtomMap.add ra rb uf.parent


  let is_equiv uf a b = CC.Atom.equal (find uf a) (find uf b)
end

let bisimilar cc ~(theta_headers : CC.header list) a1 a2 =
  let uf = BisimUF.create () in
  let theta_head_set =
    List.map theta_headers ~f:(fun h -> CC.representative cc (h : CC.header :> CC.Atom.t))
    |> CC.Atom.Set.of_list
  in
  let is_theta_head h = CC.Atom.Set.mem (CC.representative cc h) theta_head_set in
  let find_enode a =
    match CC.get_enode cc a with
    | Some _ as r ->
        r
    | None ->
        List.find_map theta_headers ~f:(fun (header : CC.header) ->
            CC.find_class_enode cc ~header a )
  in
  let rec check a1 a2 =
    let a1 = CC.representative cc a1 in
    let a2 = CC.representative cc a2 in
    CC.Atom.equal a1 a2 || BisimUF.is_equiv uf a1 a2
    ||
    match (find_enode a1, find_enode a2) with
    | Some {head= h1; children= cs1}, Some {head= h2; children= cs2} ->
        let h1 = CC.representative cc h1 in
        let h2 = CC.representative cc h2 in
        CC.Atom.equal h1 h2
        && Int.equal (List.length cs1) (List.length cs2)
        &&
        ( (* coinductive hypothesis: assume equivalence before recursing into children,
             but only for @theta_N (fixpoint) nodes — these create cycles in the PEG *)
          if is_theta_head h1 then BisimUF.union uf a1 a2 ;
          List.for_all2_exn cs1 cs2 ~f:check )
    | None, None ->
        false
    | _ ->
        false
  in
  check a1 a2


let check_equivalence ?(debug = false) (proc1 : Textual.ProcDesc.t) (proc2 : Textual.ProcDesc.t) =
  let cc = CC.init ~debug:false in
  let theta_counter = ref 0 in
  match
    ( TextualPeg.convert_proc ~theta_counter cc proc1
    , TextualPeg.convert_proc ~theta_counter cc proc2 )
  with
  | Ok (atom1, eqs1, loops1), Ok (atom2, eqs2, loops2) when Int.equal loops1 loops2 ->
      let rules = gen_rules cc ~theta_count:loops1 in
      let _rounds = Rewrite.Rule.full_rewrite cc rules in
      let theta_headers =
        List.init loops1 ~f:(fun i -> CC.mk_header cc (F.asprintf "@theta_%d" i))
      in
      let res = CC.is_equiv cc atom1 atom2 || bisimilar cc ~theta_headers atom1 atom2 in
      if debug then (
        F.printf "=== Rule stats ===@." ;
        List.iter rules ~f:(fun rule ->
            let count = Rewrite.Rule.fire_count rule in
            if count > 0 then F.printf "  %a: fired %d time(s)@." Rewrite.Rule.pp rule count ) ;
        if not res then (
          F.printf "=== Procedure 1 equations ===@." ;
          TextualPeg.Equations.pp cc F.std_formatter eqs1 ;
          F.printf "@.=== Procedure 2 equations ===@." ;
          TextualPeg.Equations.pp cc F.std_formatter eqs2 ;
          F.printf "@.NOT EQUIVALENT@." ;
          F.printf "atom1: %a@." (CC.pp_nested_term cc) atom1 ;
          F.printf "atom2: %a@." (CC.pp_nested_term cc) atom2 ) ) ;
      res
  | Ok _, Ok _ ->
      (* loops1 ≠ loops2: different loop structure *)
      false
  | Error msg, _ | _, Error msg ->
      if debug then F.printf "PEG conversion failed: %s@." msg ;
      false


let convert_and_print ?(debug = false) text =
  let sourcefile = Textual.SourceFile.create "test.sil" in
  match TextualParser.parse_string sourcefile text with
  | Ok module_ ->
      let procs =
        List.filter_map module_.decls ~f:(fun decl ->
            match decl with Textual.Module.Proc p -> Some p | _ -> None )
      in
      List.iter procs ~f:(fun (proc : Textual.ProcDesc.t) ->
          let cc = CC.init ~debug:false in
          match TextualPeg.convert_proc cc proc with
          | Ok (root, eqs, _loop_count) ->
              let name =
                F.asprintf "%a" Textual.QualifiedProcName.pp proc.procdecl.qualified_name
              in
              F.printf "=== %s ===@." name ;
              F.printf "Equations:@." ;
              TextualPeg.Equations.pp cc F.std_formatter eqs ;
              F.printf "PEG: %a@.@." (CC.pp_nested_term cc) root
          | Error msg ->
              F.printf "Error: %s@." msg ) ;
      if debug then ()
  | Error errs ->
      List.iter errs ~f:(fun e ->
          F.printf "Parse error: %a@." (TextualParser.pp_error sourcefile) e )
