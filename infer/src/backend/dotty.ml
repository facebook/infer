(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module L = Logging
module F = Format

(** {1 Dotty} *)

(* When false it prints only the retain cycle part of a prop.
   When true it prints the full property (maybe useful for debug) *)
let print_full_prop = ref false

type kind_of_dotty_prop =
  | Spec_precondition
  | Spec_postcondition of Prop.normal Prop.t  (** the precondition associated with the post *)
  | Lambda_pred of int * int * bool

(* the kind of links between different kinds of nodes*)
type kind_of_links =
  | LinkExpToExp
  | LinkExpToStruct
  | LinkStructToExp
  | LinkStructToStruct
  | LinkToArray
  | LinkArrayToExp
  | LinkArrayToStruct
  | LinkToSSL
  | LinkToDLL
  | LinkRetainCycle
[@@deriving compare]

(* coordinate identifies a node using two dimension: id is an numerical identifier of the node,*)
(* lambda identifies in which hpred parameter id lays in*)
type coordinate = {id: int; lambda: int} [@@deriving compare]

(* define a link between two nodes. src_fld/trg_fld define the label of the src/trg field. It is*)
(* useful for having nodes from within a struct and/or to inside a struct *)
type link =
  {kind: kind_of_links; src: coordinate; src_fld: string; trg: coordinate; trg_fld: string}
[@@deriving compare]

let equal_link = [%compare.equal : link]

(* type of the visualized boxes/nodes in the graph*)
type dotty_node =
  | Dotnil of coordinate
  (* nil box *)
  (* Dotdangling(coo,e,c): dangling box for expression e at coordinate coo and color c *)
  | Dotdangling of coordinate * Exp.t * string
  (* Dotpointsto(coo,e,c): basic memory cell box for expression e at coordinate coo and color c *)
  | Dotpointsto of coordinate * Exp.t * string
  (* Dotstruct(coo,e,l,c): struct box for expression e  with field list l at coordinate coo and color c *)
  | Dotstruct of coordinate * Exp.t * (Typ.Fieldname.t * Sil.strexp) list * string * Exp.t
  (* Dotarray(coo,e1,e2,l,t,c): array box for expression e1  with field list l at coordinate coo and color c*)
  (* e2 is the len and t is the type *)
  | Dotarray of coordinate * Exp.t * Exp.t * (Exp.t * Sil.strexp) list * Typ.t * string
  (* Dotlseg(coo,e1,e2,k,h,c): list box from e1 to e2 at coordinate coo and color c*)
  | Dotlseg of coordinate * Exp.t * Exp.t * Sil.lseg_kind * Sil.hpred list * string
  (* Dotlseg(coo,e1,e2,e3,e4,k,h,c): doubly linked-list box from with parameters (e1,e2,e3,e4) at coordinate coo and color c*)
  | Dotdllseg of
      coordinate * Exp.t * Exp.t * Exp.t * Exp.t * Sil.lseg_kind * Sil.hpred list * string

let mk_coordinate i l = {id= i; lambda= l}

let mk_link k s sf t tf = {kind= k; src= s; src_fld= sf; trg= t; trg_fld= tf}

(* list of dangling boxes*)
let dangling_dotboxes = ref []

(* list of nil boxes*)
let nil_dotboxes = ref []

let exps_neq_zero = ref []

(* list of fields in the structs *)
let fields_structs = ref []

let struct_exp_nodes = ref []

(* general unique counter to assign a different number to boxex,           *)
(* clusters,subgraphs etc.                                                 *)
let dotty_state_count = ref 0

let spec_counter = ref 0

let post_counter = ref 0

let lambda_counter = ref 0

let proposition_counter = ref 0

let target_invisible_arrow_pre = ref 0

let current_pre = ref 0

let spec_id = ref 0

let invisible_arrows = ref false

let print_stack_info = ref false

(* replace a dollar sign in a name with a D. We need this because dotty get confused if there is*)
(* a dollar sign i a label*)
let strip_special_chars b =
  let b = Bytes.of_string b in
  let replace st c c' =
    if Bytes.contains st c then (
      let idx = String.index_exn (Bytes.to_string st) c in
      try Bytes.set st idx c' ; st with Invalid_argument _ ->
        L.internal_error "@\n@\nstrip_special_chars: Invalid argument!@\n@." ;
        assert false )
    else st
  in
  let s0 = replace b '(' 'B' in
  let s1 = replace s0 '$' 'D' in
  let s2 = replace s1 '#' 'H' in
  let s3 = replace s2 '&' 'E' in
  let s4 = replace s3 '@' 'A' in
  let s5 = replace s4 ')' 'B' in
  let s6 = replace s5 '+' 'P' in
  let s7 = replace s6 '-' 'M' in
  Bytes.to_string s7


let rec strexp_to_string pe coo f se =
  match se with
  | Sil.Eexp (Exp.Lvar pvar, _) ->
      (Pvar.pp pe) f pvar
  | Sil.Eexp (Exp.Var id, _) ->
      if !print_full_prop then Ident.pp f id else ()
  | Sil.Eexp (e, _) ->
      if !print_full_prop then (Sil.pp_exp_printenv pe) f e else F.pp_print_char f '_'
  | Sil.Estruct (ls, _) ->
      F.fprintf f " STRUCT | { %a } " (struct_to_dotty_str pe coo) ls
  | Sil.Earray (e, idx, _) ->
      F.fprintf f " ARRAY[%a] | { %a } " (Sil.pp_exp_printenv pe) e (get_contents pe coo) idx


and struct_to_dotty_str pe coo f ls : unit =
  match ls with
  | [] ->
      ()
  | [(fn, se)] ->
      F.fprintf f "{ <%s%iL%i> %s: %a } " (Typ.Fieldname.to_string fn) coo.id coo.lambda
        (Typ.Fieldname.to_string fn) (strexp_to_string pe coo) se
  | (fn, se) :: ls' ->
      F.fprintf f " { <%s%iL%i> %s: %a } | %a" (Typ.Fieldname.to_string fn) coo.id coo.lambda
        (Typ.Fieldname.to_string fn) (strexp_to_string pe coo) se (struct_to_dotty_str pe coo) ls'


and get_contents_sexp pe coo f se =
  match se with
  | Sil.Eexp (e', _) ->
      (Sil.pp_exp_printenv pe) f e'
  | Sil.Estruct (se', _) ->
      F.fprintf f "| { %a }" (struct_to_dotty_str pe coo) se'
  | Sil.Earray (e', [], _) ->
      F.fprintf f "(ARRAY Size: %a) | { }" (Sil.pp_exp_printenv pe) e'
  | Sil.Earray (e', (idx, a) :: linner, _) ->
      F.fprintf f "(ARRAY Size: %a) | { %a: %a | %a }" (Sil.pp_exp_printenv pe) e'
        (Sil.pp_exp_printenv pe) idx (strexp_to_string pe coo) a (get_contents pe coo) linner


and get_contents_single pe coo f (e, se) =
  let e_no_special_char = strip_special_chars (Exp.to_string e) in
  F.fprintf f "{ <%s> %a : %a }" e_no_special_char (Sil.pp_exp_printenv pe) e
    (get_contents_sexp pe coo) se


and get_contents pe coo f = function
  | [] ->
      ()
  | [idx_se] ->
      (get_contents_single pe coo) f idx_se
  | idx_se :: l ->
      F.fprintf f "%a | %a" (get_contents_single pe coo) idx_se (get_contents pe coo) l


(* true if node is the source node of the expression e*)
let is_source_node_of_exp e node =
  match node with Dotpointsto (_, e', _) -> Exp.equal e e' | _ -> false


(* given a node returns its coordinates and the expression. Return -1 in case the expression doesn't*)
(* make sense for that case *)
let get_coordinate_and_exp dotnode =
  match dotnode with
  | Dotnil coo ->
      (coo, Exp.minus_one)
  | Dotarray (coo, _, _, _, _, _) ->
      (coo, Exp.minus_one)
  | Dotpointsto (coo, b, _)
  | Dotlseg (coo, b, _, _, _, _)
  | Dotdllseg (coo, b, _, _, _, _, _, _)
  | Dotstruct (coo, b, _, _, _)
  | Dotdangling (coo, b, _) ->
      (coo, b)


(* true if a node is of a Dotstruct *)
let is_not_struct node = match node with Dotstruct _ -> false | _ -> true

(* returns the id field of the coordinate of node *)
let get_coordinate_id node =
  let coo = fst (get_coordinate_and_exp node) in
  coo.id


let rec look_up_for_back_pointer e dotnodes lambda =
  match dotnodes with
  | [] ->
      []
  | Dotdllseg (coo, _, _, _, e4, _, _, _) :: dotnodes' ->
      if Exp.equal e e4 && Int.equal lambda coo.lambda then [coo.id + 1]
      else look_up_for_back_pointer e dotnodes' lambda
  | _ :: dotnodes' ->
      look_up_for_back_pointer e dotnodes' lambda


(* get the nodes corresponding to an expression and a lambda*)
let rec select_nodes_exp_lambda dotnodes e lambda =
  match dotnodes with
  | [] ->
      []
  | node :: l' ->
      let coo, e' = get_coordinate_and_exp node in
      if Exp.equal e e' && Int.equal lambda coo.lambda then
        node :: select_nodes_exp_lambda l' e lambda
      else select_nodes_exp_lambda l' e lambda


(* look-up the coordinate id in the list of dotnodes those nodes which correspond to expression e*)
(* this is written in this strange way for legacy reason. It should be changed a bit*)
let look_up dotnodes e lambda =
  let r = select_nodes_exp_lambda dotnodes e lambda in
  let r' = List.map ~f:get_coordinate_id r in
  r' @ look_up_for_back_pointer e dotnodes lambda


let reset_proposition_counter () = proposition_counter := 0

let reset_dotty_spec_counter () = spec_counter := 0

let color_to_str (c: Pp.color) =
  match c with
  | Black ->
      "black"
  | Blue ->
      "blue"
  | Green ->
      "green"
  | Orange ->
      "orange"
  | Red ->
      "red"


let make_dangling_boxes pe allocated_nodes (sigma_lambda: (Sil.hpred * int) list) =
  let exp_color hpred (exp: Exp.t) =
    if Pp.equal_color (pe.Pp.cmap_norm (Obj.repr hpred)) Pp.Red then Pp.Red
    else pe.Pp.cmap_norm (Obj.repr exp)
  in
  let get_rhs_predicate (hpred, lambda) =
    let n = !dotty_state_count in
    incr dotty_state_count ;
    let coo = mk_coordinate n lambda in
    match hpred with
    | Sil.Hpointsto (_, Sil.Eexp (e, _), _) when not (Exp.equal e Exp.zero) && !print_full_prop ->
        let e_color_str = color_to_str (exp_color hpred e) in
        [Dotdangling (coo, e, e_color_str)]
    | Sil.Hlseg (_, _, _, e2, _) when not (Exp.equal e2 Exp.zero) ->
        let e2_color_str = color_to_str (exp_color hpred e2) in
        [Dotdangling (coo, e2, e2_color_str)]
    | Sil.Hdllseg (_, _, _, e2, e3, _, _) ->
        let e2_color_str = color_to_str (exp_color hpred e2) in
        let e3_color_str = color_to_str (exp_color hpred e3) in
        let ll =
          if not (Exp.equal e2 Exp.zero) then [Dotdangling (coo, e2, e2_color_str)] else []
        in
        if not (Exp.equal e3 Exp.zero) then Dotdangling (coo, e3, e3_color_str) :: ll else ll
    | Sil.Hpointsto (_, _, _) | _ ->
        []
    (* arrays and struct do not give danglings*)
  in
  let is_allocated d =
    match d with
    | Dotdangling (_, e, _) ->
        List.exists
          ~f:(fun a ->
            match a with
            | Dotpointsto (_, e', _)
            | Dotarray (_, _, e', _, _, _)
            | Dotlseg (_, e', _, _, _, _)
            | Dotdllseg (_, e', _, _, _, _, _, _) ->
                Exp.equal e e'
            | _ ->
                false )
          allocated_nodes
    | _ ->
        false
    (*this should never happen since d must be a dangling node *)
  in
  let rec filter_duplicate l seen_exp =
    match l with
    | [] ->
        []
    | Dotdangling (coo, e, color) :: l' ->
        if List.exists ~f:(Exp.equal e) seen_exp then filter_duplicate l' seen_exp
        else Dotdangling (coo, e, color) :: filter_duplicate l' (e :: seen_exp)
    | box :: l' ->
        box :: filter_duplicate l' seen_exp
    (* this case cannot happen*)
  in
  let rec subtract_allocated candidate_dangling =
    match candidate_dangling with
    | [] ->
        []
    | d :: candidates ->
        if is_allocated d then subtract_allocated candidates
        else d :: subtract_allocated candidates
  in
  let candidate_dangling = List.concat_map ~f:get_rhs_predicate sigma_lambda in
  let candidate_dangling = filter_duplicate candidate_dangling [] in
  let dangling = subtract_allocated candidate_dangling in
  dangling_dotboxes := dangling


let rec dotty_mk_node pe sigma =
  let n = !dotty_state_count in
  incr dotty_state_count ;
  let do_hpred_lambda exp_color = function
    | ( Sil.Hpointsto (e, Sil.Earray (e', l, _), Exp.Sizeof {typ= {Typ.desc= Tarray {elt= t}}})
      , lambda ) ->
        incr dotty_state_count ;
        (* increment once more n+1 is the box for the array *)
        let e_color_str = color_to_str (exp_color e) in
        let e_color_str' = color_to_str (exp_color e') in
        [ Dotpointsto (mk_coordinate n lambda, e, e_color_str)
        ; Dotarray (mk_coordinate (n + 1) lambda, e, e', l, t, e_color_str') ]
    | Sil.Hpointsto (e, Sil.Estruct (l, _), te), lambda ->
        incr dotty_state_count ;
        (* increment once more n+1 is the box for the struct *)
        let e_color_str = color_to_str (exp_color e) in
        (*      [Dotpointsto((mk_coordinate n lambda), e, l, true, e_color_str)] *)
        [ Dotpointsto (mk_coordinate n lambda, e, e_color_str)
        ; Dotstruct (mk_coordinate (n + 1) lambda, e, l, e_color_str, te) ]
    | Sil.Hpointsto (e, _, _), lambda ->
        let e_color_str = color_to_str (exp_color e) in
        if List.mem ~equal:Exp.equal !struct_exp_nodes e then []
        else [Dotpointsto (mk_coordinate n lambda, e, e_color_str)]
    | Sil.Hlseg (k, hpara, e1, e2, _), lambda ->
        incr dotty_state_count ;
        (* increment once more n+1 is the box for last element of the list *)
        let eq_color_str = color_to_str (exp_color e1) in
        [Dotlseg (mk_coordinate n lambda, e1, e2, k, hpara.Sil.body, eq_color_str)]
    | Sil.Hdllseg (k, hpara_dll, e1, e2, e3, e4, _), lambda ->
        let e1_color_str = color_to_str (exp_color e1) in
        incr dotty_state_count ;
        (* increment once more n+1 is the box for e4 *)
        [ Dotdllseg
            (mk_coordinate n lambda, e1, e2, e3, e4, k, hpara_dll.Sil.body_dll, e1_color_str) ]
  in
  match sigma with
  | [] ->
      []
  | (hpred, lambda) :: sigma' ->
      let exp_color (exp: Exp.t) =
        if Pp.equal_color (pe.Pp.cmap_norm (Obj.repr hpred)) Pp.Red then Pp.Red
        else pe.Pp.cmap_norm (Obj.repr exp)
      in
      do_hpred_lambda exp_color (hpred, lambda) @ dotty_mk_node pe sigma'


let set_exps_neq_zero pi =
  let f = function
    | Sil.Aneq (e, Exp.Const (Const.Cint i)) when IntLit.iszero i ->
        exps_neq_zero := e :: !exps_neq_zero
    | _ ->
        ()
  in
  exps_neq_zero := [] ;
  List.iter ~f pi


let box_dangling e =
  let entry_e =
    List.filter
      ~f:(fun b -> match b with Dotdangling (_, e', _) -> Exp.equal e e' | _ -> false)
      !dangling_dotboxes
  in
  match entry_e with [] -> None | Dotdangling (coo, _, _) :: _ -> Some coo.id | _ -> None


(* NOTE: this cannot be possible since entry_e can be composed only by Dotdangling, see def of entry_e*)
(* construct a Dotnil and returns it's id *)
let make_nil_node lambda =
  let n = !dotty_state_count in
  incr dotty_state_count ;
  nil_dotboxes := Dotnil (mk_coordinate n lambda) :: !nil_dotboxes ;
  n


let compute_fields_struct sigma =
  fields_structs := [] ;
  let rec do_strexp se in_struct =
    match se with
    | Sil.Eexp (e, _) ->
        if in_struct then fields_structs := e :: !fields_structs else ()
    | Sil.Estruct (l, _) ->
        List.iter ~f:(fun e -> do_strexp e true) (snd (List.unzip l))
    | Sil.Earray (_, l, _) ->
        List.iter ~f:(fun e -> do_strexp e false) (snd (List.unzip l))
  in
  let rec fs s =
    match s with
    | [] ->
        ()
    | Sil.Hpointsto (_, se, _) :: s' ->
        do_strexp se false ; fs s'
    | _ :: s' ->
        fs s'
  in
  fs sigma


let compute_struct_exp_nodes sigma =
  struct_exp_nodes := [] ;
  let rec sen s =
    match s with
    | [] ->
        ()
    | Sil.Hpointsto (e, Sil.Estruct _, _) :: s' ->
        struct_exp_nodes := e :: !struct_exp_nodes ;
        sen s'
    | _ :: s' ->
        sen s'
  in
  sen sigma


(* returns the expression of a node*)
let get_node_exp n = snd (get_coordinate_and_exp n)

let is_nil e prop = Exp.equal e Exp.zero || Prover.check_equal (Tenv.create ()) prop e Exp.zero

(* an edge is in cycle *)
let in_cycle cycle edge =
  match cycle with
  | Some cycle' ->
      let fn, se = edge in
      List.exists
        ~f:(fun (_, fn', se') -> Typ.Fieldname.equal fn fn' && Sil.equal_strexp se se')
        cycle'
  | _ ->
      false


let node_in_cycle cycle node =
  match (cycle, node) with
  | Some _, Dotstruct (_, _, l, _, _) ->
      (* only struct nodes can be in cycle *)
      List.exists ~f:(in_cycle cycle) l
  | _ ->
      false


(* compute a list of (kind of link, field name, coo.id target, name_target) *)
let rec compute_target_struct_fields dotnodes list_fld p f lambda cycle =
  let find_target_one_fld (fn, se) =
    match se with
    | Sil.Eexp (e, _)
      -> (
        if is_nil e p then
          let n' = make_nil_node lambda in
          if !print_full_prop then [(LinkStructToExp, Typ.Fieldname.to_string fn, n', "")] else []
        else
          let nodes_e = select_nodes_exp_lambda dotnodes e lambda in
          match nodes_e with
          | [] -> (
            match box_dangling e with
            | None ->
                []
            | Some n' ->
                [(LinkStructToExp, Typ.Fieldname.to_string fn, n', "")] )
          | [node] | [Dotpointsto _; node] | [node; Dotpointsto _] ->
              let n = get_coordinate_id node in
              if List.mem ~equal:Exp.equal !struct_exp_nodes e then
                let e_no_special_char = strip_special_chars (Exp.to_string e) in
                let link_kind =
                  if in_cycle cycle (fn, se) && not !print_full_prop then LinkRetainCycle
                  else LinkStructToStruct
                in
                [(link_kind, Typ.Fieldname.to_string fn, n, e_no_special_char)]
              else [(LinkStructToExp, Typ.Fieldname.to_string fn, n, "")]
          | _ ->
              (* by construction there must be at most 2 nodes for an expression*)
              L.internal_error "@\n Too many nodes! Error! @\n@." ;
              assert false )
    | Sil.Estruct (_, _) ->
        [] (* inner struct are printed by print_struc function *)
    | Sil.Earray _ ->
        []
    (* inner arrays are printed by print_array function *)
  in
  match list_fld with
  | [] ->
      []
  | a :: list_fld' ->
      let targets_a = find_target_one_fld a in
      targets_a @ compute_target_struct_fields dotnodes list_fld' p f lambda cycle


(* compute a list of (kind of link, field name, coo.id target, name_target) *)
let rec compute_target_array_elements dotnodes list_elements p f lambda =
  let find_target_one_element (idx, se) =
    match se with
    | Sil.Eexp (e, _)
      -> (
        if is_nil e p then
          let n' = make_nil_node lambda in
          [(LinkArrayToExp, Exp.to_string idx, n', "")]
        else
          let nodes_e = select_nodes_exp_lambda dotnodes e lambda in
          match nodes_e with
          | [] -> (
            match box_dangling e with
            | None ->
                []
            | Some n' ->
                [(LinkArrayToExp, Exp.to_string idx, n', "")] )
          | [node] | [Dotpointsto _; node] | [node; Dotpointsto _] ->
              let n = get_coordinate_id node in
              if List.mem ~equal:Exp.equal !struct_exp_nodes e then
                let e_no_special_char = strip_special_chars (Exp.to_string e) in
                [(LinkArrayToStruct, Exp.to_string idx, n, e_no_special_char)]
              else [(LinkArrayToExp, Exp.to_string idx, n, "")]
          | _ ->
              (* by construction there must be at most 2 nodes for an expression*)
              L.internal_error "@\nToo many nodes! Error!@\n@." ;
              assert false )
    | Sil.Estruct (_, _) ->
        [] (* inner struct are printed by print_struc function *)
    | Sil.Earray _ ->
        []
    (* inner arrays are printed by print_array function *)
  in
  match list_elements with
  | [] ->
      []
  | a :: list_ele' ->
      let targets_a = find_target_one_element a in
      targets_a @ compute_target_array_elements dotnodes list_ele' p f lambda


let compute_target_from_eexp dotnodes e p lambda =
  if is_nil e p then
    let n' = make_nil_node lambda in
    [(LinkExpToExp, n', "")]
  else
    let nodes_e = select_nodes_exp_lambda dotnodes e lambda in
    let nodes_e_no_struct = List.filter ~f:is_not_struct nodes_e in
    let trg = List.map ~f:get_coordinate_id nodes_e_no_struct in
    match trg with
    | [] -> (
      match box_dangling e with None -> [] | Some n -> [(LinkExpToExp, n, "")] )
    | _ ->
        List.map ~f:(fun n -> (LinkExpToExp, n, "")) trg


(* build the set of edges between nodes *)
let rec dotty_mk_set_links dotnodes sigma p f cycle =
  let make_links_for_arrays e lie lambda sigma' =
    (* used for both Earray and ENarray*)
    let src = look_up dotnodes e lambda in
    match src with
    | [] ->
        assert false
    | n :: nl ->
        let target_list = compute_target_array_elements dotnodes lie p f lambda in
        (* below it's n+1 because n is the address, n+1 is the actual array node*)
        let ff n =
          List.map
            ~f:(fun (k, lab_src, m, lab_trg) ->
              mk_link k
                (mk_coordinate (n + 1) lambda)
                (strip_special_chars lab_src) (mk_coordinate m lambda)
                (strip_special_chars lab_trg) )
            target_list
        in
        let links_from_elements = List.concat_map ~f:ff (n :: nl) in
        let trg_label = strip_special_chars (Exp.to_string e) in
        let lnk =
          mk_link LinkToArray (mk_coordinate n lambda) "" (mk_coordinate (n + 1) lambda) trg_label
        in
        lnk :: links_from_elements @ dotty_mk_set_links dotnodes sigma' p f cycle
  in
  match sigma with
  | [] ->
      []
  | (Sil.Hpointsto (e, Sil.Earray (_, lie, _), _), lambda) :: sigma' ->
      make_links_for_arrays e lie lambda sigma'
  | (Sil.Hpointsto (e, Sil.Estruct (lfld, _), _), lambda) :: sigma'
    -> (
      let src = look_up dotnodes e lambda in
      match src with
      | [] ->
          assert false
      | nl ->
          (* L.out "@\n@\n List of nl= "; List.iter ~f:(L.out " %i ") nl; L.out "@.@.@."; *)
          let target_list = compute_target_struct_fields dotnodes lfld p f lambda cycle in
          let ff n =
            List.map
              ~f:(fun (k, lab_src, m, lab_trg) ->
                mk_link k (mk_coordinate n lambda) lab_src (mk_coordinate m lambda) lab_trg )
              target_list
          in
          let nodes_e = select_nodes_exp_lambda dotnodes e lambda in
          let address_struct_id =
            get_coordinate_id (List.hd_exn (List.filter ~f:(is_source_node_of_exp e) nodes_e))
          in
          (* we need to exclude the address node from the source of fields. no fields should start from there*)
          let nl' = List.filter ~f:(fun id -> address_struct_id <> id) nl in
          let links_from_fields = List.concat_map ~f:ff nl' in
          let lnk_from_address_struct =
            if !print_full_prop then
              let trg_label = strip_special_chars (Exp.to_string e) in
              [ mk_link LinkExpToStruct
                  (mk_coordinate address_struct_id lambda)
                  ""
                  (mk_coordinate (address_struct_id + 1) lambda)
                  trg_label ]
            else []
          in
          lnk_from_address_struct @ links_from_fields
          @ dotty_mk_set_links dotnodes sigma' p f cycle )
  | (Sil.Hpointsto (e, Sil.Eexp (e', _), _), lambda) :: sigma'
    -> (
      let src = look_up dotnodes e lambda in
      match src with
      | [] ->
          assert false
      | nl ->
          if !print_full_prop then
            let target_list = compute_target_from_eexp dotnodes e' p lambda in
            let ff n =
              List.map
                ~f:(fun (k, m, lab_target) ->
                  mk_link k (mk_coordinate n lambda) "" (mk_coordinate m lambda)
                    (strip_special_chars lab_target) )
                target_list
            in
            let ll = List.concat_map ~f:ff nl in
            ll @ dotty_mk_set_links dotnodes sigma' p f cycle
          else dotty_mk_set_links dotnodes sigma' p f cycle )
  | (Sil.Hlseg (_, _, e1, e2, _), lambda) :: sigma'
    -> (
      let src = look_up dotnodes e1 lambda in
      match src with
      | [] ->
          assert false
      | n :: _ ->
          let _, m, lab = List.hd_exn (compute_target_from_eexp dotnodes e2 p lambda) in
          let lnk =
            mk_link LinkToSSL (mk_coordinate (n + 1) lambda) "" (mk_coordinate m lambda) lab
          in
          lnk :: dotty_mk_set_links dotnodes sigma' p f cycle )
  | (Sil.Hdllseg (_, _, e1, e2, e3, _, _), lambda) :: sigma' ->
      let src = look_up dotnodes e1 lambda in
      match src with
      | [] ->
          assert false
      | n :: _ ->
          (* n is e1's box  and n+1 is e4's box *)
          let targetF = look_up dotnodes e3 lambda in
          let target_Flink =
            match targetF with
            | [] ->
                []
            | m :: _ ->
                [mk_link LinkToDLL (mk_coordinate (n + 1) lambda) "" (mk_coordinate m lambda) ""]
          in
          let targetB = look_up dotnodes e2 lambda in
          let target_Blink =
            match targetB with
            | [] ->
                []
            | m :: _ ->
                [mk_link LinkToDLL (mk_coordinate n lambda) "" (mk_coordinate m lambda) ""]
          in
          target_Blink @ target_Flink @ dotty_mk_set_links dotnodes sigma' p f cycle


let print_kind f kind =
  incr dotty_state_count ;
  match kind with
  | Spec_precondition ->
      incr dotty_state_count ;
      current_pre := !dotty_state_count ;
      F.fprintf f "@\n PRE%iL0 [label=\"PRE %i \",  style=filled, color= yellow]@\n"
        !dotty_state_count !spec_counter ;
      print_stack_info := true
  | Spec_postcondition _ ->
      F.fprintf f "@\n POST%iL0 [label=\"POST %i \",  style=filled, color= yellow]@\n"
        !dotty_state_count !post_counter ;
      print_stack_info := true
  | Lambda_pred (no, lev, array) ->
    match array with
    | false ->
        F.fprintf f "%s @\n state%iL%i [label=\"INTERNAL STRUCTURE %i \",  %s]@\n"
          "style=dashed; color=blue" !dotty_state_count !lambda_counter !lambda_counter
          "style=filled, color= lightblue" ;
        F.fprintf f "state%iL%i -> state%iL%i [color=\"lightblue \"  arrowhead=none] @\n"
          !dotty_state_count !lambda_counter no lev
    | true ->
        F.fprintf f "%s @\n state%iL%i [label=\"INTERNAL STRUCTURE %i \",  %s]@\n"
          "style=dashed; color=blue" !dotty_state_count !lambda_counter !lambda_counter
          "style=filled, color= lightblue" ;
        (* F.fprintf f "state%iL%i -> struct%iL%i:%s [color=\"lightblue \"  arrowhead=none] @\n"
             !dotty_state_count !lambda_counter no lev lab;*)
        incr dotty_state_count


(* print a link between two nodes in the graph *)
let dotty_pp_link f link =
  let n1 = link.src.id in
  let lambda1 = link.src.lambda in
  let n2 = link.trg.id in
  let lambda2 = link.trg.lambda in
  let src_fld = link.src_fld in
  let trg_fld = link.trg_fld in
  match (n2, link.kind) with
  | 0, _ when !print_full_prop ->
      F.fprintf f "state%iL%i -> state%iL%i[label=\"%s DANG\", color= red];@\n" n1 lambda1 n2
        lambda2 src_fld
  | _, LinkToArray when !print_full_prop ->
      F.fprintf f "state%iL%i -> struct%iL%i:%s%iL%i[label=\"\"]@\n" n1 lambda1 n2 lambda2 trg_fld
        n2 lambda2
  | _, LinkExpToStruct when !print_full_prop ->
      F.fprintf f "state%iL%i -> struct%iL%i:%s%iL%i[label=\"\"]@\n" n1 lambda1 n2 lambda2 trg_fld
        n2 lambda2
  | _, LinkStructToExp when !print_full_prop ->
      F.fprintf f "struct%iL%i:%s%iL%i -> state%iL%i[label=\"\"]@\n" n1 lambda1 src_fld n1 lambda1
        n2 lambda2
  | _, LinkRetainCycle ->
      F.fprintf f "struct%iL%i:%s%iL%i -> struct%iL%i:%s%iL%i[label=\"\", color= red]@\n" n1
        lambda1 src_fld n1 lambda1 n2 lambda2 trg_fld n2 lambda2
  | _, LinkStructToStruct when !print_full_prop ->
      F.fprintf f "struct%iL%i:%s%iL%i -> struct%iL%i:%s%iL%i[label=\"\"]@\n" n1 lambda1 src_fld n1
        lambda1 n2 lambda2 trg_fld n2 lambda2
  | _, LinkArrayToExp when !print_full_prop ->
      F.fprintf f "struct%iL%i:%s -> state%iL%i[label=\"\"]@\n" n1 lambda1 src_fld n2 lambda2
  | _, LinkArrayToStruct when !print_full_prop ->
      F.fprintf f "struct%iL%i:%s -> struct%iL%i[label=\"\"]@\n" n1 lambda1 src_fld n2 lambda2
  | _, _ ->
      if !print_full_prop then
        F.fprintf f "state%iL%i -> state%iL%i[label=\"%s\"];@\n" n1 lambda1 n2 lambda2 src_fld
      else ()


(* given the list of nodes and links get rid of spec nodes that are not pointed to by anybody*)
let filter_useless_spec_dollar_box (nodes: dotty_node list) (links: link list) =
  let tmp_nodes = ref nodes in
  let tmp_links = ref links in
  let remove_links_from ln =
    List.filter ~f:(fun n' -> not (List.mem ~equal:equal_link ln n')) !tmp_links
  in
  let remove_node n ns =
    List.filter
      ~f:(fun n' ->
        match n' with Dotpointsto _ -> get_coordinate_id n' <> get_coordinate_id n | _ -> true )
      ns
  in
  let rec boxes_pointed_by n lns =
    match lns with
    | [] ->
        []
    | l :: ln' ->
        let n_id = get_coordinate_id n in
        if Int.equal l.src.id n_id && String.equal l.src_fld "" then
          (*L.out "@\n Found link (%i,%i)" l.src.id l.trg.id;*)
          l :: boxes_pointed_by n ln'
        else boxes_pointed_by n ln'
  in
  let rec boxes_pointing_at n lns =
    match lns with
    | [] ->
        []
    | l :: ln' ->
        let n_id = get_coordinate_id n in
        if Int.equal l.trg.id n_id && String.equal l.trg_fld "" then
          (*L.out "@\n Found link (%i,%i)" l.src.id l.trg.id;*)
          l :: boxes_pointing_at n ln'
        else boxes_pointing_at n ln'
  in
  let is_spec_variable = function
    | Exp.Var id ->
        Ident.is_normal id && Ident.equal_name (Ident.get_name id) Ident.name_spec
    | _ ->
        false
  in
  let handle_one_node node =
    match node with
    | Dotpointsto _ ->
        let e = get_node_exp node in
        if is_spec_variable e then
          let links_from_node = boxes_pointed_by node links in
          let links_to_node = boxes_pointing_at node links in
          if List.is_empty links_to_node then (
            tmp_links := remove_links_from links_from_node ;
            tmp_nodes := remove_node node !tmp_nodes )
    | _ ->
        ()
  in
  List.iter ~f:handle_one_node nodes ;
  (!tmp_nodes, !tmp_links)


(* print a struct node *)
let rec print_struct f pe e te l coo c =
  let print_type =
    match te with
    | Exp.Sizeof {typ}
      -> (
        let str_t = Typ.to_string typ in
        match Str.split_delim (Str.regexp_string Config.anonymous_block_prefix) str_t with
        | [_; _] ->
            "BLOCK object"
        | _ ->
            str_t )
    | _ ->
        Exp.to_string te
  in
  let n = coo.id in
  let lambda = coo.lambda in
  let e_no_special_char = strip_special_chars (Exp.to_string e) in
  F.fprintf f "subgraph structs_%iL%i {@\n" n lambda ;
  if !print_full_prop then
    F.fprintf f
      " node [%s]; @\n struct%iL%i [label=\"{<%s%iL%i> STRUCT: %a } | %a\" ] fontcolor=%s@\n"
      "shape=record" n lambda e_no_special_char n lambda (Sil.pp_exp_printenv pe) e
      (struct_to_dotty_str pe coo) l c
  else
    F.fprintf f
      " node [%s]; @\n struct%iL%i [label=\"{<%s%iL%i> OBJECT: %s } | %a\" ] fontcolor=%s@\n"
      "shape=record" n lambda e_no_special_char n lambda print_type (struct_to_dotty_str pe coo) l
      c ;
  F.fprintf f "}@\n"


and print_array f pe e1 e2 l coo c =
  let n = coo.id in
  let lambda = coo.lambda in
  let e_no_special_char = strip_special_chars (Exp.to_string e1) in
  F.fprintf f "subgraph structs_%iL%i {@\n" n lambda ;
  F.fprintf f
    " node [%s]; @\n struct%iL%i [label=\"{<%s%iL%i> ARRAY| SIZE: %a } | %a\" ] fontcolor=%s@\n"
    "shape=record" n lambda e_no_special_char n lambda (Sil.pp_exp_printenv pe) e2
    (get_contents pe coo) l c ;
  F.fprintf f "}@\n"


and print_sll f pe nesting k e1 coo =
  let n = coo.id in
  let lambda = coo.lambda in
  let n' = !dotty_state_count in
  incr dotty_state_count ;
  ( match k with
  | Sil.Lseg_NE ->
      F.fprintf f
        "subgraph cluster_%iL%i { %s node [style=filled,color=white];  label=\"list NE\";" n'
        lambda "style=filled; color=lightgrey;"
  | Sil.Lseg_PE ->
      F.fprintf f
        "subgraph cluster_%iL%i { %s node [style=filled,color=white];   label=\"list PE\";" n'
        lambda "style=filled; color=lightgrey;" ) ;
  F.fprintf f "state%iL%i [label=\"%a\"]@\n" n lambda (Sil.pp_exp_printenv pe) e1 ;
  let n' = !dotty_state_count in
  incr dotty_state_count ;
  F.fprintf f "state%iL%i [label=\"... \" style=filled color=lightgrey] @\n" n' lambda ;
  F.fprintf f "state%iL%i -> state%iL%i [label=\" \"] @\n" n lambda n' lambda ;
  F.fprintf f "state%iL%i [label=\" \"] @\n" (n + 1) lambda ;
  F.fprintf f "state%iL%i -> state%iL%i [label=\" \"] }" n' lambda (n + 1) lambda ;
  incr lambda_counter ;
  pp_dotty f (Lambda_pred (n + 1, lambda, false))
    (Prop.normalize (Tenv.create ()) (Prop.from_sigma nesting))
    None


and print_dll f pe nesting k e1 e4 coo =
  let n = coo.id in
  let lambda = coo.lambda in
  let n' = !dotty_state_count in
  incr dotty_state_count ;
  ( match k with
  | Sil.Lseg_NE ->
      F.fprintf f "subgraph cluster_%iL%i { %s node [style=filled,color=white];  label=\"%s\";" n'
        lambda "style=filled; color=lightgrey;" "doubly-linked list NE"
  | Sil.Lseg_PE ->
      F.fprintf f "subgraph cluster_%iL%i { %s node [style=filled,color=white];  label=\"%s\";" n'
        lambda "style=filled; color=lightgrey;" "doubly-linked list PE" ) ;
  F.fprintf f "state%iL%i [label=\"%a\"]@\n" n lambda (Sil.pp_exp_printenv pe) e1 ;
  let n' = !dotty_state_count in
  incr dotty_state_count ;
  F.fprintf f "state%iL%i [label=\"... \" style=filled color=lightgrey] @\n" n' lambda ;
  F.fprintf f "state%iL%i -> state%iL%i [label=\" \"]@\n" n lambda n' lambda ;
  F.fprintf f "state%iL%i -> state%iL%i [label=\" \"]@\n" n' lambda n lambda ;
  F.fprintf f "state%iL%i [label=\"%a\"]@\n" (n + 1) lambda (Sil.pp_exp_printenv pe) e4 ;
  F.fprintf f "state%iL%i -> state%iL%i [label=\" \"]@\n" (n + 1) lambda n' lambda ;
  F.fprintf f "state%iL%i -> state%iL%i [label=\" \"]}@\n" n' lambda (n + 1) lambda ;
  incr lambda_counter ;
  pp_dotty f (Lambda_pred (n', lambda, false))
    (Prop.normalize (Tenv.create ()) (Prop.from_sigma nesting))
    None


and dotty_pp_state f pe cycle dotnode =
  let dotty_exp coo e c is_dangling =
    let n = coo.id in
    let lambda = coo.lambda in
    if is_dangling then
      F.fprintf f "state%iL%i [label=\"%a \", color=red, style=dashed, fontcolor=%s]@\n" n lambda
        (Sil.pp_exp_printenv pe) e c
    else
      F.fprintf f "state%iL%i [label=\"%a\" fontcolor=%s]@\n" n lambda (Sil.pp_exp_printenv pe) e c
  in
  match dotnode with
  | Dotnil coo when !print_full_prop ->
      F.fprintf f "state%iL%i [label=\"NIL \", color=green, style=filled]@\n" coo.id coo.lambda
  | Dotdangling (coo, e, c) when !print_full_prop ->
      dotty_exp coo e c true
  | Dotpointsto (coo, e1, c) when !print_full_prop ->
      dotty_exp coo e1 c false
  | Dotstruct (coo, e1, l, c, te) ->
      let l' =
        if !print_full_prop then l else List.filter ~f:(fun edge -> in_cycle cycle edge) l
      in
      print_struct f pe e1 te l' coo c
  | Dotarray (coo, e1, e2, l, _, c) when !print_full_prop ->
      print_array f pe e1 e2 l coo c
  | Dotlseg (coo, e1, _, Sil.Lseg_NE, nesting, _) when !print_full_prop ->
      print_sll f pe nesting Sil.Lseg_NE e1 coo
  | Dotlseg (coo, e1, _, Sil.Lseg_PE, nesting, _) when !print_full_prop ->
      print_sll f pe nesting Sil.Lseg_PE e1 coo
  | Dotdllseg (coo, e1, _, _, e4, Sil.Lseg_NE, nesting, _) when !print_full_prop ->
      print_dll f pe nesting Sil.Lseg_NE e1 e4 coo
  | Dotdllseg (coo, e1, _, _, e4, Sil.Lseg_PE, nesting, _) when !print_full_prop ->
      print_dll f pe nesting Sil.Lseg_PE e1 e4 coo
  | _ ->
      ()


(* Build the graph data structure to be printed *)
and build_visual_graph f pe p cycle =
  let sigma = p.Prop.sigma in
  compute_fields_struct sigma ;
  compute_struct_exp_nodes sigma ;
  (* L.out "@\n@\n Computed fields structs: ";
     List.iter ~f:(fun e -> L.out " %a " (Sil.pp_exp_printenv pe) e) !fields_structs;
     L.out "@\n@.";
     L.out "@\n@\n Computed exp structs nodes: ";
     List.iter ~f:(fun e -> L.out " %a " (Sil.pp_exp_printenv pe) e) !struct_exp_nodes;
     L.out "@\n@."; *)
  let sigma_lambda = List.map ~f:(fun hp -> (hp, !lambda_counter)) sigma in
  let nodes = dotty_mk_node pe sigma_lambda in
  if !print_full_prop then make_dangling_boxes pe nodes sigma_lambda ;
  let links = dotty_mk_set_links nodes sigma_lambda p f cycle in
  filter_useless_spec_dollar_box nodes links


and display_pure_info f pe prop =
  let print_invisible_objects () =
    for j = 1 to 4 do
      F.fprintf f "  inv_%i%i [style=invis]@\n" !spec_counter j ;
      F.fprintf f "  inv_%i%i%i [style=invis]@\n" !spec_counter j j ;
      F.fprintf f "  inv_%i%i%i%i [style=invis]@\n" !spec_counter j j j
    done ;
    for j = 1 to 4 do
      F.fprintf f "  state_pi_%i -> inv_%i%i [style=invis]@\n" !proposition_counter !spec_counter j ;
      F.fprintf f "  inv_%i%i -> inv_%i%i%i [style=invis]@\n" !spec_counter j !spec_counter j j ;
      F.fprintf f "  inv_%i%i%i -> inv_%i%i%i%i [style=invis]@\n" !spec_counter j j !spec_counter j
        j j
    done
  in
  let pure = Prop.get_pure prop in
  F.fprintf f "subgraph {@\n" ;
  F.fprintf f
    " node [shape=box]; @\n state_pi_%i [label=\"STACK \\n\\n %a\" color=orange style=filled]@\n"
    !proposition_counter (Prop.pp_pi pe) pure ;
  if !invisible_arrows then print_invisible_objects () ;
  F.fprintf f "}@\n"


(** Pretty print a proposition in dotty format. *)
and pp_dotty f kind (prop_: Prop.normal Prop.t) cycle =
  incr proposition_counter ;
  let pe, prop =
    match kind with
    | Spec_postcondition pre ->
        target_invisible_arrow_pre := !proposition_counter ;
        let diff =
          Propgraph.compute_diff Black (Propgraph.from_prop pre) (Propgraph.from_prop prop_)
        in
        let cmap_norm = Propgraph.diff_get_colormap false diff in
        let cmap_foot = Propgraph.diff_get_colormap true diff in
        let pe = {(Prop.prop_update_obj_sub Pp.text pre) with cmap_norm; cmap_foot} in
        (* add stack vars from pre *)
        let pre_stack = fst (Prop.sigma_get_stack_nonstack true pre.Prop.sigma) in
        let prop = Prop.set prop_ ~sigma:(pre_stack @ prop_.Prop.sigma) in
        (pe, Prop.normalize (Tenv.create ()) prop)
    | _ ->
        let pe = Prop.prop_update_obj_sub Pp.text prop_ in
        (pe, prop_)
  in
  dangling_dotboxes := [] ;
  nil_dotboxes := [] ;
  set_exps_neq_zero prop.Prop.pi ;
  incr dotty_state_count ;
  F.fprintf f "@\n subgraph cluster_prop_%i { color=black @\n" !proposition_counter ;
  print_kind f kind ;
  if !print_stack_info then (
    display_pure_info f pe prop ;
    print_stack_info := false ) ;
  (* F.fprintf f "@\n subgraph cluster_%i { color=black @\n" !dotty_state_count; *)
  let nodes, links = build_visual_graph f pe prop cycle in
  let all_nodes = nodes @ !dangling_dotboxes @ !nil_dotboxes in
  if !print_full_prop then List.iter ~f:(dotty_pp_state f pe cycle) all_nodes
  else
    List.iter
      ~f:(fun node -> if node_in_cycle cycle node then dotty_pp_state f pe cycle node)
      all_nodes ;
  List.iter ~f:(dotty_pp_link f) links ;
  (* F.fprintf f "@\n } @\n"; *)
  F.fprintf f "@\n } @\n"


let pp_dotty_one_spec f pre posts =
  post_counter := 0 ;
  incr spec_counter ;
  incr proposition_counter ;
  incr dotty_state_count ;
  F.fprintf f "@\n subgraph cluster_%i { color=blue @\n" !dotty_state_count ;
  incr dotty_state_count ;
  F.fprintf f "@\n state%iL0 [label=\"SPEC %i \",  style=filled, color= lightblue]@\n"
    !dotty_state_count !spec_counter ;
  spec_id := !dotty_state_count ;
  invisible_arrows := true ;
  pp_dotty f Spec_precondition pre None ;
  invisible_arrows := false ;
  List.iter
    ~f:(fun (po, _) ->
      incr post_counter ;
      pp_dotty f (Spec_postcondition pre) po None ;
      for j = 1 to 4 do
        F.fprintf f "  inv_%i%i%i%i -> state_pi_%i [style=invis]@\n" !spec_counter j j j
          !target_invisible_arrow_pre
      done )
    posts ;
  F.fprintf f "@\n } @\n"


(********** START of Print interprocedural cfgs in dotty format  *)
(********** Print control flow graph (in dot form) for fundec to channel. You have to compute an
            interprocedural cfg first. *)

let pp_cfgnodename pname fmt (n: Procdesc.Node.t) =
  F.fprintf fmt "\"%s_%d\""
    (Escape.escape_dotty (Typ.Procname.to_filename pname))
    (Procdesc.Node.get_id n :> int)


let pp_etlist byvals fmt etl =
  List.iteri
    ~f:(fun index (id, typ) ->
      let byval_mark =
        if Typ.is_pointer typ && List.mem byvals index ~equal:Int.equal then "(byval)" else ""
      in
      Format.fprintf fmt " %a:%a%s" Mangled.pp id (Typ.pp_full Pp.text) typ byval_mark )
    etl


let pp_var_list fmt etl =
  List.iter
    ~f:(fun (id, ty) -> Format.fprintf fmt " %a:%a" Mangled.pp id (Typ.pp_full Pp.text) ty)
    etl


let pp_local_list fmt etl = List.iter ~f:(Procdesc.pp_local fmt) etl

let pp_cfgnodelabel pdesc fmt (n: Procdesc.Node.t) =
  let pp_label fmt n =
    match Procdesc.Node.get_kind n with
    | Procdesc.Node.Start_node pname ->
        let pname_string = Escape.escape_dotty (Typ.Procname.to_string pname) in
        let attributes = Procdesc.get_attributes pdesc in
        let byvals = attributes.ProcAttributes.by_vals in
        Format.fprintf fmt "Start %s\\nFormals: %a\\nLocals: %a" pname_string (pp_etlist byvals)
          (Procdesc.get_formals pdesc) pp_local_list (Procdesc.get_locals pdesc) ;
        if not (List.is_empty (Procdesc.get_captured pdesc)) then
          Format.fprintf fmt "\\nCaptured: %a" pp_var_list (Procdesc.get_captured pdesc) ;
        let method_annotation = attributes.ProcAttributes.method_annotation in
        if not (Annot.Method.is_empty method_annotation) then
          Format.fprintf fmt "\\nAnnotation: %a" (Annot.Method.pp pname_string) method_annotation
    | Procdesc.Node.Exit_node pname ->
        Format.fprintf fmt "Exit %s" (Escape.escape_dotty (Typ.Procname.to_string pname))
    | Procdesc.Node.Join_node ->
        Format.pp_print_char fmt '+'
    | Procdesc.Node.Prune_node (is_true_branch, if_kind, _) ->
        Format.fprintf fmt "Prune (%b branch, %s)" is_true_branch (Sil.if_kind_to_string if_kind)
    | Procdesc.Node.Stmt_node s ->
        Format.fprintf fmt " %s" s
    | Procdesc.Node.Skip_node s ->
        Format.fprintf fmt "Skip %s" s
  in
  let instr_string i =
    let pp f = Sil.pp_instr Pp.text f i in
    let str = F.asprintf "%t" pp in
    Escape.escape_dotty str
  in
  let pp_instrs fmt instrs =
    List.iter ~f:(fun i -> F.fprintf fmt " %s\\n " (instr_string i)) instrs
  in
  let instrs = Procdesc.Node.get_instrs n in
  F.fprintf fmt "%d: %a \\n  %a" (Procdesc.Node.get_id n :> int) pp_label n pp_instrs instrs


let pp_cfgnodeshape fmt (n: Procdesc.Node.t) =
  match Procdesc.Node.get_kind n with
  | Procdesc.Node.Start_node _ | Procdesc.Node.Exit_node _ ->
      F.pp_print_string fmt "color=yellow style=filled"
  | Procdesc.Node.Prune_node _ ->
      F.fprintf fmt "shape=\"invhouse\""
  | Procdesc.Node.Skip_node _ ->
      F.fprintf fmt "color=\"gray\""
  | Procdesc.Node.Stmt_node _ ->
      F.fprintf fmt "shape=\"box\""
  | _ ->
      ()


let pp_cfgnode pdesc fmt (n: Procdesc.Node.t) =
  let pname = Procdesc.get_proc_name pdesc in
  F.fprintf fmt "%a [label=\"%a\" %a]@\n\t@\n" (pp_cfgnodename pname) n (pp_cfgnodelabel pdesc) n
    pp_cfgnodeshape n ;
  let print_edge n1 n2 is_exn =
    let color = if is_exn then "[color=\"red\" ]" else "" in
    match Procdesc.Node.get_kind n2 with
    | Procdesc.Node.Exit_node _ when is_exn ->
        (* don't print exception edges to the exit node *)
        ()
    | _ ->
        F.fprintf fmt "@\n\t %a -> %a %s;" (pp_cfgnodename pname) n1 (pp_cfgnodename pname) n2
          color
  in
  List.iter ~f:(fun n' -> print_edge n n' false) (Procdesc.Node.get_succs n) ;
  List.iter ~f:(fun n' -> print_edge n n' true) (Procdesc.Node.get_exn n)


(* * print control flow graph (in dot form) for fundec to channel let      *)
(* print_cfg_channel (chan : out_channel) (fd : fundec) = let pnode (s:    *)
(* stmt) = fprintf chan "%a@\n" d_cfgnode s in forallStmts pnode fd *      *)
(* Print control flow graph (in dot form) for fundec to file let           *)
(* print_cfg_filename (filename : string) (fd : fundec) = let chan =       *)
(* open_out filename in begin print_cfg_channel chan fd; close_out chan;   *)
(* end                                                                     *)
(* Print the extra information related to the inteprocedural aspect, ie.,  *)
(* special node, and call / return edges                                   *)
let print_icfg source fmt cfg =
  let print_node pdesc node =
    let loc = Procdesc.Node.get_loc node in
    if Config.dotty_cfg_libs || SourceFile.equal loc.Location.file source then
      F.fprintf fmt "%a@\n" (pp_cfgnode pdesc) node
  in
  Cfg.iter_all_nodes ~sorted:true cfg ~f:print_node


let write_icfg_dotty_to_file source cfg fname =
  let chan = Out_channel.create fname in
  let fmt = Format.formatter_of_out_channel chan in
  (* avoid phabricator thinking this file was generated by substituting substring with %s *)
  F.fprintf fmt "/* %@%s */@\ndigraph cfg {@\n" "generated" ;
  print_icfg source fmt cfg ;
  F.fprintf fmt "}@\n" ;
  Out_channel.close chan


let print_icfg_dotty source cfg =
  let fname =
    match Config.icfg_dotty_outfile with
    | Some file ->
        file
    | None when Config.frontend_tests ->
        SourceFile.to_abs_path source ^ ".test.dot"
    | None ->
        DB.filename_to_string
          (DB.Results_dir.path_to_filename (DB.Results_dir.Abs_source_dir source)
             [Config.dotty_output])
  in
  write_icfg_dotty_to_file source cfg fname


(********** END of Printing dotty files ***********)

(** Dotty printing for specs *)
let pp_speclist_dotty f (splist: Prop.normal BiabductionSummary.spec list) =
  let pp_simple_saved = !Config.pp_simple in
  Config.pp_simple := true ;
  reset_proposition_counter () ;
  reset_dotty_spec_counter () ;
  F.fprintf f "@\n@\n@\ndigraph main { @\nnode [shape=box]; @\n" ;
  F.fprintf f "@\n compound = true; @\n" ;
  (*  F.fprintf f "@\n size=\"12,7\"; ratio=fill; @\n"; *)
  List.iter
    ~f:(fun s ->
      pp_dotty_one_spec f
        (BiabductionSummary.Jprop.to_prop s.BiabductionSummary.pre)
        s.BiabductionSummary.posts )
    splist ;
  F.fprintf f "@\n}" ;
  Config.pp_simple := pp_simple_saved


let pp_speclist_to_file (filename: DB.filename) spec_list =
  let pp_simple_saved = !Config.pp_simple in
  Config.pp_simple := true ;
  let outc = Out_channel.create (DB.filename_to_string (DB.filename_add_suffix filename ".dot")) in
  let fmt = F.formatter_of_out_channel outc in
  let () = F.fprintf fmt "#### Dotty version:  ####@\n%a@\n@\n" pp_speclist_dotty spec_list in
  Out_channel.close outc ;
  Config.pp_simple := pp_simple_saved


let pp_speclist_dotty_file (filename: DB.filename) spec_list =
  try pp_speclist_to_file filename spec_list with exn when SymOp.exn_not_failure exn -> ()
