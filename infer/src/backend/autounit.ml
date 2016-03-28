(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Generate unit tests automatically from specs *)

module L = Logging
module F = Format

let (++) = Sil.Int.add
let (--) = Sil.Int.sub

module IdMap = Map.Make (Ident) (** maps from identifiers *)

(** Constraint solving module *)
module Constraint : sig
  (** Collect constraints on [vars] from [pi], and return a satisfying instantiation *)
  val solve_from_pure : Sil.atom list -> Ident.t list -> Sil.Int.t IdMap.t
end = struct
  (** flag for debug mode of the module *)
  let debug = false

  (** denote a range of values [bottom] <= val <= [top], except [excluded] *)
  type range =
    { mutable bottom: Sil.Int.t option; (** lower bound *)
      mutable excluded: Sil.Int.t list; (** individual values not in range *)
      mutable top: Sil.Int.t option; (** upper bound *) }

  type eval = range IdMap.t (** evaluation for variables *)

  (** create a new range *)
  let new_range () =
    { bottom = None; excluded = []; top = None }

  (** pretty print a range *)
  let pp_range id fmt rng =
    let pp_opt fmt = function
      | None -> F.fprintf fmt "_"
      | Some n -> Sil.Int.pp fmt n in
    F.fprintf fmt "%a <= %a <= %a [%a]" pp_opt rng.bottom (Ident.pp pe_text) id pp_opt rng.top (pp_comma_seq Sil.Int.pp) rng.excluded

  (** pretty print an evaluation *)
  let pp_eval fmt ev =
    let do_id id rng =
      F.fprintf fmt "%a@." (pp_range id) rng in
    IdMap.iter do_id ev

  (** create a new evaluation *)
  let new_eval vars : eval =
    let ev = ref IdMap.empty in
    let add_var id =
      ev := IdMap.add id (new_range ()) !ev in
    IList.iter add_var vars;
    !ev

  let gt_bottom i r =
    match r.bottom with
    | None -> true
    | Some j -> Sil.Int.gt i j

  let geq_bottom i r =
    match r.bottom with
    | None -> true
    | Some j -> Sil.Int.geq i j

  let lt_top i r =
    match r.top with
    | None -> true
    | Some j -> Sil.Int.lt i j

  let leq_top i r =
    match r.top with
    | None -> true
    | Some j -> Sil.Int.leq i j

  (** normalize [r]: the excluded elements must be strictly between bottom and top *)
  let normalize r =
    r.excluded <- IList.filter (fun i -> geq_bottom i r && leq_top i r) r.excluded;
    let rec normalize_bottom () = match r.bottom with
      | None -> ()
      | Some i ->
          if IList.mem Sil.Int.eq i r.excluded then begin
            r.excluded <- IList.filter (Sil.Int.neq i) r.excluded;
            r.bottom <- Some (i ++ Sil.Int.one);
            normalize_bottom ()
          end in
    let rec normalize_top () = match r.top with
      | None -> ()
      | Some i ->
          if IList.mem Sil.Int.eq i r.excluded then begin
            r.excluded <- IList.filter (Sil.Int.neq i) r.excluded;
            r.top <- Some (i -- Sil.Int.one);
            normalize_top ()
          end in
    normalize_bottom ();
    normalize_top ()

  (** global variable set to true every time a range is changed *)
  let changed = ref false

  let mark_changed id r =
    changed := true;
    if debug then F.fprintf F.std_formatter "changed: %a@." (pp_range id) r

  (** exclude one element from the range *)
  let add_excluded r id i =
    if geq_bottom i r && leq_top i r && not (IList.mem Sil.Int.eq i r.excluded)
    then begin
      r.excluded <- i :: r.excluded;
      normalize r;
      mark_changed id r;
    end

  (** make the bottom of the range >= [i] *)
  let add_bottom r id i =
    if gt_bottom i r then
      begin
        r.bottom <- Some i;
        normalize r;
        mark_changed id r
      end

  (** make the top of the range <= [i] *)
  let add_top r id i =
    if lt_top i r then
      begin
        r.top <- Some i;
        normalize r;
        mark_changed id r
      end

  (** choose an element in the range *)
  let choose id rng =
    if debug then F.fprintf F.std_formatter "choosing %a@." (pp_range id) rng;
    let found = ref None in
    let num_iter = IList.length rng.excluded in
    let try_candidate candidate =
      if geq_bottom candidate rng && leq_top candidate rng && not (IList.mem Sil.Int.eq candidate rng.excluded)
      then (found := Some candidate; rng.bottom <- Some candidate; rng.top <- Some candidate; rng.excluded <- []) in
    let search_up () =
      let base = match rng.bottom with None -> Sil.Int.zero | Some n -> n in
      for i = 0 to num_iter do
        if !found = None then
          let candidate = Sil.Int.add base (Sil.Int.of_int i) in
          try_candidate candidate
      done in
    let search_down () =
      let base = match rng.top with None -> Sil.Int.zero | Some n -> n in
      for i = 0 to num_iter do
        if !found = None then
          let candidate = Sil.Int.sub base (Sil.Int.of_int i) in
          try_candidate candidate
      done in
    search_up ();
    if !found = None then search_down ();
    if !found = None then
      (L.err "Constraint Error: empty range %a@." (pp_range id) rng;
       rng.top <- Some Sil.Int.zero;
       rng.bottom <- Some Sil.Int.zero;
       rng.excluded <- [])

  (** return the solution if the id is solved (has unique solution) *)
  let solved ev id =
    let rng = IdMap.find id ev in
    match rng.bottom, rng.top with
    | Some n1, Some n2 when Sil.Int.eq n1 n2 -> Some n1
    | _ -> None

  let rec pi_iter do_le do_lt do_neq pi =
    let do_atom a = match a with
      | Sil.Aeq (Sil.BinOp (Sil.Le, e1, e2), Sil.Const (Sil.Cint i)) when Sil.Int.isone i ->
          do_le e1 e2
      | Sil.Aeq (Sil.BinOp (Sil.Lt, e1, e2), Sil.Const (Sil.Cint i)) when Sil.Int.isone i ->
          do_lt e1 e2
      | Sil.Aeq _ -> ()
      | Sil.Aneq (e1, e2) ->
          do_neq e1 e2 in
    changed := false;
    IList.iter do_atom pi;
    if !changed then pi_iter do_le do_lt do_neq pi

  (** Collect constraints on [vars] from [pi], and return a satisfying instantiation *)
  let solve_from_pure pi vars =
    if debug then F.fprintf F.std_formatter "solve_from_pure pi: %a@." (Prop.pp_pi pe_text) pi;
    let vars_fav = Sil.fav_from_list vars in
    let atom_is_relevant a =
      let fav = Sil.atom_fav a in
      Sil.fav_for_all fav (fun id -> Sil.fav_mem vars_fav id) in
    let pi_relevant = IList.filter atom_is_relevant pi in
    let ev = new_eval vars in
    let update_top rng id n_op = match rng.top, n_op with
      | Some _, Some n -> add_top rng id n
      | _ -> () in
    let update_bottom rng id n_op = match rng.bottom, n_op with
      | Some _, Some n -> add_bottom rng id n
      | _ -> () in
    let (+++) n1_op n2_op = match n1_op, n2_op with
      | Some n1, Some n2 -> Some (Sil.Int.add n1 n2)
      | _ -> None in
    let (---) n1_op n2_op = match n1_op, n2_op with
      | Some n1, Some n2 -> Some (Sil.Int.sub n1 n2)
      | _ -> None in
    let do_le e1 e2 = match e1, e2 with
      | Sil.Var id, Sil.Const (Sil.Cint n) ->
          let rng = IdMap.find id ev in
          add_top rng id n
      | Sil.BinOp (Sil.MinusA, Sil.Var id1, Sil.Var id2), Sil.Const (Sil.Cint n) ->
          let rng1 = IdMap.find id1 ev in
          let rng2 = IdMap.find id2 ev in
          update_top rng1 id1 (rng2.top +++ (Some n));
          update_bottom rng2 id2 (rng1.bottom --- (Some n))
      | Sil.BinOp (Sil.PlusA, Sil.Var id1, Sil.Var id2), Sil.Const (Sil.Cint n) ->
          let rng1 = IdMap.find id1 ev in
          let rng2 = IdMap.find id2 ev in
          update_top rng1 id1 (Some n --- rng2.bottom);
          update_top rng2 id2 (Some n --- rng1.bottom)
      | _ -> if debug then assert false in
    let do_lt e1 e2 = match e1, e2 with
      | Sil.Const (Sil.Cint n), Sil.Var id ->
          let rng = IdMap.find id ev in
          add_bottom rng id (n ++ Sil.Int.one)
      | Sil.Const (Sil.Cint n), Sil.BinOp (Sil.PlusA, Sil.Var id1, Sil.Var id2) ->
          let rng1 = IdMap.find id1 ev in
          let rng2 = IdMap.find id2 ev in
          update_bottom rng1 id1 (Some (n ++ Sil.Int.one) --- rng2.top);
          update_bottom rng2 id2 (Some (n ++ Sil.Int.one) --- rng1.top)
      | _ -> if debug then assert false in
    let rec do_neq e1 e2 = match e1, e2 with
      | Sil.Var id, Sil.Const (Sil.Cint n)
      | Sil.Const(Sil.Cint n), Sil.Var id ->
          let rng = IdMap.find id ev in
          add_excluded rng id n
      | Sil.Var id1, Sil.Var id2 ->
          (match solved ev id1, solved ev id2 with
           | None, None -> ()
           | Some _, Some _ -> ()
           | Some n1, None ->
               do_neq (Sil.exp_int n1) e2
           | None, Some n2 ->
               do_neq e1 (Sil.exp_int n2))
      | Sil.Var id1, Sil.BinOp(Sil.PlusA, Sil.Var id2, Sil.Const (Sil.Cint n)) ->
          (match solved ev id1, solved ev id2 with
           | None, None -> ()
           | Some _, Some _ -> ()
           | Some n1, None ->
               do_neq (Sil.exp_int (n1 -- n)) (Sil.Var id2)
           | None, Some n2 ->
               do_neq (Sil.Var id1) (Sil.exp_int (n2 ++ n)))
      | _ -> if debug then assert false in
    let do_ident id =
      if debug then F.fprintf F.std_formatter "constraints before doing %a:@.%a@." (Ident.pp pe_text) id pp_eval ev;
      let rng = IdMap.find id ev in
      pi_iter do_le do_lt do_neq pi_relevant;
      choose id rng in
    IList.iter do_ident vars;
    if debug then F.fprintf F.std_formatter "solution to pure constraints:@.%a@." pp_eval ev;
    let solution = IdMap.map (function { bottom = Some n } -> n | _ -> assert false) ev in
    solution
end

type varinfo =
  { typ: Sil.typ; (* type of the variable *)
    alloc: bool (* whether the variable needs allocation (on lhs of |->, lists) *)
  }

type idmap = varinfo IdMap.t (* map from identifier to varinfo *)

let extend_idmap id vinfo idmap =
  let vinfo' =
    try
      let old_vinfo = IdMap.find id !idmap in
      { old_vinfo with alloc = old_vinfo.alloc || vinfo.alloc }
    with Not_found -> vinfo in
  idmap := IdMap.add id vinfo' !idmap

let pe = pe_text

(** Given a sigma, create an idmap *)
let create_idmap sigma : idmap =
  let idmap = ref IdMap.empty in
  let rec do_exp e typ = match e, typ with
    | Sil.Const _, _ -> ()
    | Sil.Var id, _ ->
        extend_idmap id { typ = typ; alloc = false } idmap
    | Sil.BinOp (Sil.PlusA, e1, e2), _ ->
        do_exp e1 typ;
        do_exp e2 typ
    | Sil.BinOp (Sil.PlusPI, e1, e2), _ ->
        do_exp e1 typ;
        do_exp e2 (Sil.Tint Sil.IULong)
    | Sil.Lfield (e1, _, _), _ ->
        do_exp e1 typ
    | Sil.Sizeof _, _ -> ()
    | _ ->
        L.err "Unmatched exp: %a : %a@." (Sil.pp_exp pe) e (Sil.pp_typ_full pe) typ;
        assert false in
  let rec do_se se typ = match se, typ with
    | Sil.Eexp (e, _), _ ->
        do_exp e typ
    | Sil.Estruct (fsel, _), Sil.Tstruct { Sil.instance_fields } ->
        do_struct fsel instance_fields
    | Sil.Earray (size, esel, _), Sil.Tarray (typ, _) ->
        do_se (Sil.Eexp (size, Sil.inst_none)) (Sil.Tint Sil.IULong);
        do_array esel typ
    | _ ->
        L.err "Unmatched sexp: %a : %a@." (Sil.pp_sexp pe) se (Sil.pp_typ_full pe) typ;
        assert false
  and do_struct fsel ftal = match fsel, ftal with
    | [], _ -> ()
    | (f1, se) :: fsel', (f2, typ, _) :: ftl' when Ident.fieldname_equal f1 f2 ->
        do_se se typ;
        do_struct fsel' ftl'
    | _ :: _, _ :: ftal' ->
        do_struct fsel ftal'
    | _:: _, [] -> assert false
  and do_array esel typ = match esel with
    | (e, se):: esel' ->
        do_se (Sil.Eexp (e, Sil.inst_none)) (Sil.Tint Sil.IULong);
        do_se se typ;
        do_array esel' typ
    | [] -> () in
  let do_lhs_e e t = match e with
    | Sil.Var id ->
        extend_idmap id { typ = t; alloc = true } idmap
    | _ -> () in
  let do_hpred = function
    | Sil.Hpointsto (e, se, Sil.Sizeof (typ, _)) ->
        do_lhs_e e (Sil.Tptr (typ, Sil.Pk_pointer));
        do_se se typ
    | Sil.Hlseg (_, _, e, f, el) ->
        do_lhs_e e (Sil.Tptr (Sil.Tvoid, Sil.Pk_pointer));
        do_se (Sil.Eexp (f, Sil.inst_none)) (Sil.Tptr (Sil.Tvoid, Sil.Pk_pointer));
        IList.iter (fun e -> do_se (Sil.Eexp (e, Sil.inst_none)) Sil.Tvoid) el
    | hpred ->
        L.err "do_hpred not implemented %a@." (Sil.pp_hpred pe) hpred in
  IList.iter do_hpred sigma;
  !idmap

module Code : sig
  type t
  val add_from_pp : t -> (Format.formatter -> unit -> unit) -> unit
  val add_line : t -> string -> unit
  val append : t -> t -> unit
  val empty : unit -> t
  val pp : F.formatter -> t -> unit
  val set_indent : string -> unit
  (* val to_list : t -> string list *)
end = struct
  type t = string list ref
  let indent = ref ""
  let to_list code =
    IList.rev !code
  let pp fmt code =
    let doit line = F.fprintf fmt "%s@\n" line in
    IList.iter doit (to_list code);
    F.fprintf fmt "@."
  let empty () = ref []
  let add_line code l =
    code := (!indent ^ l) :: !code
  let add_from_pp code pp =
    add_line code (pp_to_string pp ())
  let append code1 code2 =
    code1 := !code2 @ !code1
  let set_indent s =
    indent := s
end

type code = Code.t

(** pretty print generated code *)
let pp_code = Code.pp

(** pretty print an ident in C *)
let pp_id_c fmt id =
  let name = Ident.get_name id in
  let stamp = Ident.get_stamp id in
  let varname = Ident.name_to_string name in
  F.fprintf fmt "%s%d" varname stamp

(** pretty print an expression in C *)
let rec pp_exp_c pe fmt = function
  | Sil.Lfield (e, f, _) ->
      F.fprintf fmt "&(%a->%a)" (pp_exp_c pe) e Ident.pp_fieldname f
  | Sil.Var id ->
      pp_id_c fmt id
  | e ->
      Sil.pp_exp pe fmt e

(** pretty print a type in C *)
let pp_typ_c pe typ =
  let pp_nil _ () = () in
  Sil.pp_type_decl pe pp_nil pp_exp_c typ

(** Convert a pvar to a string by just extracting the name *)
let to_string pvar =
  Mangled.to_string (Pvar.get_name pvar)

(** pretty print an expression list in C *)
let pp_exp_list_c pe f expl =
  (pp_seq (pp_exp_c pe)) f expl

(** Name of the recusive function for a specific list para *)
let mk_lseg_name id proc_name spec_num =
  "mk_lseg_" ^ string_of_int id ^ Procname.to_string proc_name ^ "_spec" ^ string_of_int spec_num

let mk_size_name id =
  "_size_" ^ string_of_int id

let pp_texp_for_malloc fmt =
  let rec handle_arr_size typ = match typ with
    | Sil.Tvar _ | Sil.Tint _ | Sil.Tfloat _ | Sil.Tvoid | Sil.Tfun _ ->
        typ
    | Sil.Tptr (t, pk) ->
        Sil.Tptr (handle_arr_size t, pk)
    | Sil.Tstruct struct_typ ->
        let instance_fields =
          IList.map (fun (f, t, a) -> (f, handle_arr_size t, a)) struct_typ.Sil.instance_fields in
        Sil.Tstruct { struct_typ with Sil.instance_fields }
    | Sil.Tarray (t, e) ->
        Sil.Tarray (handle_arr_size t, e) in
  function
  | Sil.Sizeof (typ, _) ->
      let typ' = handle_arr_size typ in
      F.fprintf fmt "sizeof(%a)" (pp_typ_c pe) typ'
  | e -> pp_exp_c pe fmt e

(* generate code for sigma *)
let gen_sigma code proc_name spec_num env sigma =
  let post_code = Code.empty () in
  let rec do_strexp code' base need_deref = function
    | Sil.Eexp (e, _) ->
        let lhs = if need_deref then "(*"^base^")" else base in
        let pp f () = F.fprintf f "%s = %a;" lhs (pp_exp_c pe) e in
        Code.add_from_pp code' pp
    | Sil.Estruct (fsel, _) ->
        let accessor = if need_deref then "->" else "." in
        IList.iter (fun (f, se) -> do_strexp code' (base ^ accessor ^ Ident.fieldname_to_string f) false se) fsel
    | Sil.Earray (_, esel, _) ->
        IList.iter (fun (e, se) ->
            let pp f () = F.fprintf f "%a" (pp_exp_c pe) e in
            let index = pp_to_string pp () in
            do_strexp code' (base ^ "[" ^ index ^ "]") false se) esel in

  let gen_hpred = function
    | Sil.Hpointsto (Sil.Lvar pvar, se, _) ->
        let base = to_string pvar in
        do_strexp post_code base false se
    | Sil.Hpointsto (Sil.Var id, se, te) ->
        let pp1 f () =
          F.fprintf f "%a = malloc(%a);" pp_id_c id pp_texp_for_malloc te in
        let pp2 f () =
          F.fprintf f "if(%a == NULL) exit(12);" pp_id_c id in
        Code.add_from_pp code pp1;
        Code.add_from_pp code pp2;
        let pp3 f () = F.fprintf f "%a" pp_id_c id in
        let base = pp_to_string pp3 () in
        do_strexp post_code base true se
    | Sil.Hlseg (_, hpar, Sil.Var id, f, el) ->
        let hpara_id = Sil.Predicates.get_hpara_id env hpar in
        let size_var = mk_size_name hpara_id in
        let mk_name = mk_lseg_name hpara_id proc_name spec_num in
        let pp_el fmt el =
          if el != [] then pp_exp_list_c pe fmt el in
        let pp1 fmt () =
          F.fprintf fmt "int %s = 42;" size_var in
        let pp2 fmt () =
          F.fprintf fmt "%a = %s(%s, %a%a);" pp_id_c id mk_name size_var (pp_exp_c pe) f pp_el el in
        Code.add_from_pp code pp1;
        Code.add_from_pp code pp2
    | hpred ->
        L.err "gen_hpred not implemented: %a@." (Sil.pp_hpred pe) hpred in
  IList.iter gen_hpred sigma;
  Code.append code post_code

(* generate code corresponding to equalities in the pure part *)
let gen_init_equalities code pure =
  let do_atom = function
    | Sil.Aeq (Sil.Var id, e) ->
        let pp f () = F.fprintf f "%a = %a;" pp_id_c id (pp_exp_c pe) e in
        Code.add_from_pp code pp
    | _ -> () in
  IList.iter do_atom pure

(** generate variable declarations *)
let gen_var_decl code idmap parameters =
  let do_parameter (name, typ) =
    let pp_name f () = Mangled.pp f name in
    let pp f () = F.fprintf f "%a;" (Sil.pp_type_decl pe pp_name pp_exp_c) typ in
    Code.add_from_pp code pp in
  let do_vinfo id { typ } =
    let pp_var f () = pp_id_c f id in
    let pp f () = F.fprintf f "%a;" (Sil.pp_type_decl pe pp_var pp_exp_c) typ in
    Code.add_from_pp code pp in
  IList.iter do_parameter parameters;
  IdMap.iter do_vinfo idmap

(** initialize variables not requiring allocation *)
let gen_init_vars code solutions idmap =
  let get_const id c =
    try Sil.Cint (IdMap.find id solutions)
    with Not_found -> c in
  let do_vinfo id { typ = typ; alloc = alloc } =
    if not alloc then
      let const = match typ with
        | Sil.Tint _ | Sil.Tvoid ->
            get_const id (Sil.Cint Sil.Int.zero)
        | Sil.Tfloat _ ->
            Sil.Cfloat 0.0
        | Sil.Tptr _ ->
            get_const id (Sil.Cint Sil.Int.zero)
        | Sil.Tfun _ ->
            Sil.Cint Sil.Int.zero
        | typ ->
            L.err "do_vinfo type undefined: %a@." (Sil.pp_typ_full pe) typ;
            assert false in
      let pp fmt () =
        F.fprintf fmt "%a = (%a) %a;"
          pp_id_c id (Sil.pp_typ_full pe) typ (Sil.pp_exp pe) (Sil.Const const) in
      Code.add_from_pp code pp in
  IdMap.iter do_vinfo idmap

(** apply a filter to the identifiers of an idmap *)
let filter_idmap filter idmap =
  let idmap' = ref IdMap.empty in
  IdMap.iter (fun id x -> if filter id then idmap' := IdMap.add id x !idmap') idmap;
  !idmap'

let pp_svars fmt svars =
  if svars != [] then F.fprintf fmt "%a" (pp_comma_seq pp_id_c) svars


let gen_hpara code proc_name spec_num env id hpara =
  let mk_name = mk_lseg_name id proc_name spec_num in
  let size_name = mk_size_name id in
  let pp1 f () =
    F.fprintf f "void* %s(int %s, void* %a%a) {"
      mk_name size_name pp_id_c hpara.Sil.next pp_svars hpara.Sil.svars in
  let pp2 f () =
    F.fprintf f "%a= %s(%s -1 , %a%a);"
      pp_id_c hpara.Sil.next mk_name size_name pp_id_c hpara.Sil.next pp_svars hpara.Sil.svars in
  let line1 = pp_to_string pp1 () in
  let idmap = create_idmap hpara.Sil.body in
  let idmap_ex =
    let filter i =
      IList.exists (Ident.equal i) hpara.Sil.evars in
    filter_idmap filter idmap in
  let idmap_no_next =
    let filter i =
      not (Ident.equal i hpara.Sil.next) in
    filter_idmap filter idmap in
  let line11 = "if ("^size_name^" == 0) {" in
  let line12 = "return " ^ (pp_to_string pp_id_c hpara.Sil.next) ^ ";" in
  let line13 ="} else {" in
  let line14 = pp_to_string pp2 () in
  let line2 = "return " ^ (pp_to_string pp_id_c hpara.Sil.root) ^ ";" in
  let line3 = "}" in
  Code.add_line code line1;
  Code.set_indent "  ";
  gen_var_decl code idmap_no_next [];
  Code.add_line code line11;
  Code.set_indent "    ";
  Code.add_line code line12;
  Code.set_indent "  ";
  Code.add_line code line13;
  Code.set_indent "    ";
  Code.add_line code line14;
  gen_init_vars code IdMap.empty idmap_ex;
  gen_sigma code proc_name spec_num env hpara.Sil.body;
  Code.add_line code line2;
  Code.set_indent "  ";
  Code.add_line code line3;
  Code.set_indent "";
  Code.add_line code line3;
  Code.add_line code ""

let gen_hpara_dll _ _ _ _ _ _ = assert false

(** Generate epilog for the test case *)
let gen_epilog code proc_name (parameters : (Mangled.t * Sil.typ) list) =
  let pp_parameter fmt (name, _) = Mangled.pp fmt name in
  let pp f () = F.fprintf f "%a(%a);" Procname.pp proc_name (pp_comma_seq pp_parameter) parameters in
  let line1 = pp_to_string pp () in
  let line2 = "}" in
  Code.add_line code line1;
  Code.set_indent "";
  Code.add_line code line2

let test_function_name proc_name spec_num =
  "unit_test_" ^ Procname.to_string proc_name ^ "_spec" ^ string_of_int spec_num

(** Generate prolog for test case *)
let gen_prolog code fname proc_name spec_num =
  let pp f () =
    let fun_name = test_function_name proc_name spec_num in
    F.fprintf f "/**@\n *@\n * Procedure: %a()@\n * File: %s@\n" Procname.pp proc_name fname;
    F.fprintf f " * Unit Test: %s()@\n * Created: %a@\n *@\n */@\n@\n" fun_name pp_current_time ();
    F.fprintf f "void %s() {" fun_name in
  Code.add_from_pp code pp;
  Code.set_indent "  "

let solve_constraints pure idmap =
  let vars = ref [] in
  let do_vinfo id { alloc } =
    if not alloc then vars := !vars @ [id] in
  IdMap.iter do_vinfo idmap;
  Constraint.solve_from_pure pure !vars

(** generate a unit test form a spec *)
let genunit fname proc_name spec_num parameters spec =
  let pre = Specs.Jprop.to_prop spec.Specs.pre in
  let pure = Prop.get_pure pre in
  let sigma = Prop.get_sigma pre in
  let env = Prop.prop_pred_env pre in
  let idmap = create_idmap sigma in
  let code = Code.empty () in
  Sil.Predicates.iter env
    (gen_hpara code proc_name spec_num env)
    (gen_hpara_dll code proc_name spec_num env);
  gen_prolog code fname proc_name spec_num;
  gen_var_decl code idmap parameters;
  gen_init_vars code (solve_constraints pure idmap) idmap;
  gen_init_equalities code pure;
  gen_sigma code proc_name spec_num env sigma;
  gen_epilog code proc_name parameters;
  code

(** generate code for a main calling all the unit test functions passed as argument *)
let genmain proc_numspecs_list =
  let code = Code.empty () in
  let do_one_proc (proc_name, num_specs) =
    for i = 1 to num_specs do
      let test_fun_name = test_function_name proc_name i in
      let line = test_fun_name ^ "();" in
      Code.add_line code line done in
  Code.add_line code "int main() {";
  Code.set_indent "  ";
  IList.iter do_one_proc proc_numspecs_list;
  Code.add_line code "printf(\"unit test terminated\\n\");";
  Code.add_line code "return 0;";
  Code.set_indent "";
  Code.add_line code "}";
  code
