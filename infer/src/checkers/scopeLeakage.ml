(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format
module Hashtbl = Caml.Hashtbl
module VarMap = Hashtbl.Make (Var)

let pp_comma_sep fmt () = F.pp_print_string fmt ", "

(* A module for parsing a JSON configuration into a custom data type. *)
module AnalysisConfig : sig
  (** A class name and the method names of that class that are known to "generate" a scope. That is,
      return objects of a known scope. *)
  type classname_methods = {classname: string; methods: string list}

  (** Lists of generators for the different types of scopes. *)
  type generators = classname_methods list

  (** Class names for the different types of scopes. *)
  type scope = {classname: string; generators: generators}

  type must_not_hold_pair = {holder: string; held: string}

  type t =
    {annotation_classname: string; scopes: scope list; must_not_hold_pairs: must_not_hold_pair list}

  val empty : t

  val parse : Yojson.Basic.t -> t
  (** Parses a JSON configuration into a custom data type. *)

  val pp : F.formatter -> t -> unit
end = struct
  type classname_methods = {classname: string; methods: string list}

  type generators = classname_methods list

  type scope = {classname: string; generators: generators}

  type must_not_hold_pair = {holder: string; held: string}

  type t =
    {annotation_classname: string; scopes: scope list; must_not_hold_pairs: must_not_hold_pair list}

  let empty = {annotation_classname= ""; scopes= []; must_not_hold_pairs= []}

  let pp_method fs methodname = F.fprintf fs {|"%s"|} methodname

  let pp_methods fs methods = F.pp_print_list ~pp_sep:pp_comma_sep pp_method fs methods

  let pp_generator fs {classname; methods} =
    F.fprintf fs {| { "classname": "%s", "methods": [%a] } |} classname pp_methods methods


  let pp_generators fs generators = F.pp_print_list ~pp_sep:pp_comma_sep pp_generator fs generators

  let pp_must_not_hold_pair fs {holder; held} =
    F.fprintf fs {| { "holder": "%s", "held": "%s" } |} holder held


  let pp_must_not_hold_pairs fs pairs =
    F.pp_print_list ~pp_sep:pp_comma_sep pp_must_not_hold_pair fs pairs


  let pp_scope fs {classname; generators} =
    F.fprintf fs {| { "classname": "%s", "generators": [%a] } |} classname pp_generators generators


  let pp_scopes fs scopes = F.pp_print_list ~pp_sep:pp_comma_sep pp_scope fs scopes

  let pp fs config =
    F.fprintf fs
      {|"scope-leakage-config" : {
  "annot-classname": "%s",
  "scopes": [
    %a
  ],
  "must-not-hold": {%a}
}
      |}
      config.annotation_classname pp_scopes config.scopes pp_must_not_hold_pairs
      config.must_not_hold_pairs


  (** Finds a named JSON node in a map and aborts with an informative message otherwise. *)
  let find_node map key node =
    try Hashtbl.find map key
    with _ ->
      L.die UserError "Missing key \"%s\" in association node %a!@\n" key Yojson.Basic.pp node


  (* Converts a JSON `Assoc into a Hashtbl. *)
  let json_assoc_list_to_map assoc_list =
    let result = Hashtbl.create 10 in
    List.iter assoc_list ~f:(fun (key, node) -> Hashtbl.add result key node) ;
    result


  (* Converts a JSON string list node into a list of strings. *)
  let json_list_to_string_list node =
    match node with
    | `List nodes ->
        List.map nodes ~f:Yojson.Basic.Util.to_string
    | _ ->
        L.die UserError "Failed parsing a list of strings from %a!@\n" Yojson.Basic.pp node


  (** node is a JSON entry of the form "classname" : string, "methods": [list of strings]. *)
  let parse_classname_methods node =
    match node with
    | `Assoc assoc_list ->
        let node_as_map = json_assoc_list_to_map assoc_list in
        let classname = Yojson.Basic.Util.to_string (find_node node_as_map "classname" node) in
        let methods = json_list_to_string_list (find_node node_as_map "methods" node) in
        {classname; methods}
    | _ ->
        L.die UserError "Failed parsing a classname+methods node from %a!@\n" Yojson.Basic.pp node


  let parse_generators node =
    match node with
    | `List list_node ->
        List.map list_node ~f:parse_classname_methods
    | _ ->
        L.die UserError "Failed parsing a list of classname+methods list from %a!@\n"
          Yojson.Basic.pp node


  let parse_scope node =
    match node with
    | `Assoc generators_list ->
        let node_as_map = json_assoc_list_to_map generators_list in
        let classname_node = find_node node_as_map "classname" node in
        let generators_node = find_node node_as_map "generators" node in
        { classname= Yojson.Basic.Util.to_string classname_node
        ; generators= parse_generators generators_node }
    | _ ->
        L.die UserError "Failed parsing scope node from %a!@\n" Yojson.Basic.pp node


  let parse_scope_list node =
    match node with
    | `List node_list ->
        List.map node_list ~f:parse_scope
    | _ ->
        L.die UserError "Failed parsing a list of scopes from %a" Yojson.Basic.pp node


  let parse_must_not_hold_pair node =
    match node with
    | `Assoc node_assoc_list ->
        let node_as_map = json_assoc_list_to_map node_assoc_list in
        let left_node = find_node node_as_map "holds" node in
        let right_node = find_node node_as_map "held" node in
        {holder= Yojson.Basic.Util.to_string left_node; held= Yojson.Basic.Util.to_string right_node}
    | _ ->
        L.die UserError "Failed parsing a must-not-hold pair from %a!@\n" Yojson.Basic.pp node


  let parse_must_not_hold node =
    match node with
    | `List node_list ->
        List.map node_list ~f:parse_must_not_hold_pair
    | _ ->
        L.die UserError "Failed parsing a must-not-hold pair from %a!@\n" Yojson.Basic.pp node


  let parse node =
    match node with
    | `Assoc node_assoc_list ->
        let node_as_map = json_assoc_list_to_map node_assoc_list in
        let annot_classname_node = find_node node_as_map "annot-classname" node in
        let scopes_node = find_node node_as_map "scopes" node in
        let must_not_hold_node = find_node node_as_map "must-not-hold" node in
        { annotation_classname= Yojson.Basic.Util.to_string annot_classname_node
        ; scopes= parse_scope_list scopes_node
        ; must_not_hold_pairs= parse_must_not_hold must_not_hold_node }
    | `List [] ->
        L.debug Analysis Verbose "scope-leakage-config is empty!@\n" ;
        empty
    | _ ->
        L.die UserError "Failed parsing a scope-leakage-config node from %a!@\n" Yojson.Basic.pp
          node
end

(** Parse the configuration once and for all. *)
let config =
  if Config.is_checker_enabled ScopeLeakage then AnalysisConfig.parse Config.scope_leakage_config
  else AnalysisConfig.empty


(** A module for defining scopes and basic operations on scopes as well as extracting scopes from
    type annotations. *)
module Scope : sig
  type t

  val bottom : t
  (** Represents an empty set of scopes. *)

  val is_bottom : t -> bool

  val join : t -> t -> t

  val pp : F.formatter -> t -> unit

  val of_type : Tenv.t -> Typ.t -> t
  (** Infers a scope for the given type. *)

  val must_not_hold : t -> t -> bool
  (** must_not_hold [holder_scope] [held_scope] is true if objects of scope [holder_scope] are not
      allowed to hold (transitively via object fields) objects of scope [held_scope]. *)

  val of_generator_procname : Procname.t -> t
  (** Matches a procedure name with the scope of the returned object (or bottom if the scope of the
      returned object is unknown) by matching it against the set of known generators. *)
end = struct
  (* In order to allow a scope abstraction, we slightly abuse the set of scopes
     by adding a bottom element and a top element.
     A more precise option is to define another type that holds a set
     of scopes, but the added precision is not needed.
  *)
  type t = Bottom | Type of Typ.Name.t | Top [@@deriving equal]

  let bottom = Bottom

  let annotation_class = config.annotation_classname

  let scope_class_names = List.map config.scopes ~f:(fun scope -> scope.AnalysisConfig.classname)

  let is_bottom scope = equal scope bottom

  let leq ~lhs ~rhs =
    if equal lhs rhs then true
    else match (lhs, rhs) with Bottom, _ | _, Top -> true | _, _ -> false


  let join x y = if leq ~lhs:x ~rhs:y then y else if leq ~lhs:y ~rhs:x then x else Top

  let pp fs s =
    F.pp_print_string fs
      (match s with Type typ -> Typ.Name.name typ | Bottom -> "Bottom" | Top -> "Unknown")


  (** Returns the scope corresponding to the class implementing it, or Bottom if the given name does
      not correspond to any such class. *)
  let of_scope_class_name name =
    if List.mem scope_class_names name ~equal:String.equal then
      Type (Typ.Name.Java.from_string name)
    else bottom


  let must_not_hold_scopes =
    List.map config.must_not_hold_pairs ~f:(fun {AnalysisConfig.holder; held} ->
        (of_scope_class_name holder, of_scope_class_name held) )


  let must_not_hold s1 s2 =
    if equal s1 Top then List.exists must_not_hold_scopes ~f:(fun (_, held) -> equal s2 held)
    else if equal s2 Top then
      List.exists must_not_hold_scopes ~f:(fun (holder, _) -> equal s1 holder)
    else
      List.exists must_not_hold_scopes ~f:(fun (holder, held) -> equal s1 holder && equal s2 held)


  (** Matches a parameter like "value = OuterScope.class" with the corresponding scope. *)
  let of_annot_param = function
    | {Annot.name= Some "value"; value= Class typ} when PatternMatch.type_is_class typ ->
        of_scope_class_name (PatternMatch.get_type_name typ)
    | _ ->
        Bottom


  (* Joins the set of scopes appearing in the parameters.
  *)
  let of_annot {Annot.class_name; parameters} =
    if String.equal class_name annotation_class then
      List.fold parameters ~init:Bottom ~f:(fun accum_scope annot_param ->
          let curr_scope = of_annot_param annot_param in
          join curr_scope accum_scope )
    else bottom


  (** Traverses the given type to find a Struct.t, if one exists. *)
  let rec inner_struct_of_type tenv {Typ.desc} =
    match desc with
    | Tstruct struct_name ->
        Tenv.lookup tenv struct_name
    | Tptr (ptr_base_typ, _) ->
        inner_struct_of_type tenv ptr_base_typ
    | Tarray {elt} ->
        inner_struct_of_type tenv elt
    | _ ->
        None


  (** Returns the scope corresponding to the given struct based on its annotations. *)
  let of_struct_annots s =
    let open Struct in
    List.fold s.annots ~init:bottom ~f:(fun accum_scope annot ->
        let curr_scope = of_annot annot in
        join curr_scope accum_scope )


  (** Returns the join of the scopes of all super-types of a given type name, based on their
      annotations. *)
  let of_supertypes tenv typname =
    Tenv.fold_supers tenv typname ~init:bottom ~f:(fun _ struct_opt accum_scope ->
        match struct_opt with
        | Some super_struct ->
            join accum_scope (of_struct_annots super_struct)
        | None ->
            accum_scope )


  (** Traverses the type to find an element associated with a type name, if one exists. *)
  let rec inner_typename {Typ.desc} =
    match desc with
    | Tstruct struct_name ->
        Some struct_name
    | Tptr (ptr_base_typ, _) ->
        inner_typename ptr_base_typ
    | Tarray {elt} ->
        inner_typename elt
    | _ ->
        None


  (** Returns the scope associated with the given type based on its own annotations and the
      annotations of its super-types. *)
  let of_type tenv typ =
    let annots_scope =
      match inner_struct_of_type tenv typ with Some s -> of_struct_annots s | None -> bottom
    in
    let supers_scope =
      match inner_typename typ with Some typname -> of_supertypes tenv typname | None -> bottom
    in
    join annots_scope supers_scope


  let matches_classname_methods classname' methodname' {AnalysisConfig.classname; methods} =
    String.equal classname classname' && List.exists methods ~f:(String.equal methodname')


  (** Checks whether the given classname and methodname exist in the list of generators. *)
  let matches_generators classname methodname generators =
    List.exists generators ~f:(matches_classname_methods classname methodname)


  (** Returns the scope for which there exists a generator that matches the given Java procedure
      name, if one exists, and bottom otherwise. *)
  let match_javaname scopes (jname : Procname.Java.t) =
    let curr_classname = Procname.Java.get_class_name jname in
    let curr_method = Procname.Java.get_method jname in
    match
      List.find scopes ~f:(fun scope ->
          matches_generators curr_classname curr_method scope.AnalysisConfig.generators )
    with
    | Some scope ->
        of_scope_class_name scope.AnalysisConfig.classname
    | None ->
        bottom


  (** Given a config, generates a function that matches a procedure name with the scope of the
      returned object (or bottom if the scope of the returned object is unknown). *)
  let of_generator_procname_with_config {AnalysisConfig.scopes} procname =
    match procname with Procname.Java jname -> match_javaname scopes jname | _ -> bottom


  let of_generator_procname = of_generator_procname_with_config config
end

(** A mapping from pointer variables to scopes. *)
module VarToScope = struct
  open VarMap

  type t = Scope.t VarMap.t

  let create size : t = VarMap.create size

  let find_or_bottom tbl k = find_opt tbl k |> Option.value ~default:Scope.bottom

  let join tbl k d =
    let d' = find_or_bottom tbl k in
    let joined_scope = Scope.join d d' in
    if not (Scope.is_bottom joined_scope) (* Keep the mapping sparse. *) then
      replace tbl k joined_scope
    else (* d' is bottom, meaning there is no key in the table. *)
      ()


  let join_list tbl entries = List.iter entries ~f:(fun (var, scope) -> join tbl var scope)

  let pp fmt tbl =
    let pp_pair fmt (fst, snd) = F.fprintf fmt "%a: %a" Var.pp fst Scope.pp snd in
    F.fprintf fmt "@[<hv>[%a]@]" (F.pp_print_seq ~pp_sep:pp_comma_sep pp_pair) (VarMap.to_seq tbl)
end

module Summary = VarToScope

(** A flow-insensitive alias analysis between local pointer variables. That is, only assignments
    between pointer variables are considered.

    The analysis is based on a union-find-style data structure where an alias set is represented by
    a single variable, and all other variables in that set reference the representative variable. To
    obtain a near-linear time complexity, we need to merge small alias sets into larger ones, which
    requires maintaining size information. We skip that, since we expect the alias sets to be small
    in practice and not worth the additional time/space for maintaining size counters. *)
module Aliasing = struct
  open VarMap

  let iter = VarMap.iter

  (** Returns the representative of the given variable's alias set. *)
  let rec get_rep aliasing var =
    match find_opt aliasing var with Some higher -> get_rep aliasing higher | None -> var


  (** Infers a pair of aliased variables in case of a direct assignment between pointer variables. *)
  let alias_of_instr instr =
    let open Sil in
    let open Exp in
    match instr with
    | Load {id= lvar; typ; e= Lvar rvar} when Typ.is_pointer typ ->
        Some (Var.of_id lvar, Var.of_pvar rvar)
    | Store {e1= Lvar lhs; e2= Var rhs} ->
        Some (Var.of_pvar lhs, Var.of_id rhs)
    | _ ->
        None


  (** A flow-insensitive alias analysis over local variables alone. *)
  let of_proc_desc proc_desc =
    let varToRep = create 100 in
    Procdesc.iter_instrs
      (fun _ instr ->
        match alias_of_instr instr with
        | Some (v1, v2) ->
            let v1_rep = get_rep varToRep v1 in
            let v2_rep = get_rep varToRep v2 in
            if not (Var.equal v1_rep v2_rep) then replace varToRep v1_rep v2_rep
        | None ->
            () )
      proc_desc ;
    (* Fully compress the aliasing chains. *)
    filter_map_inplace (fun k _ -> Some (get_rep varToRep k)) varToRep ;
    varToRep
end

(** Sets the scope of any given variable to the join of the scopes of all other members of its alias
    set. *)
let join_scopes_across_alias_sets aliasing scoping : unit =
  (* Join all scopes into the representative of the alias set. *)
  Aliasing.iter
    (fun v rep ->
      let v_scope = VarToScope.find_or_bottom scoping v in
      VarToScope.join scoping rep v_scope )
    aliasing ;
  (* Now copy the representative's scope to all other members of its alias set.
  *)
  Aliasing.iter
    (fun v rep ->
      let rep_scope = VarToScope.find_or_bottom scoping rep in
      VarToScope.join scoping v rep_scope )
    aliasing


(* The variable to which we should assign a scope to in the given expression. *)
let rec scope_target_var_of_expr (e : Exp.t) =
  match e with
  | Var id ->
      Some (Var.of_id id)
  | Lvar pvar ->
      Some (Var.of_pvar pvar)
  | Lfield (e, _, _) | Cast (_, e) | Exn e | Lindex (e, _) ->
      scope_target_var_of_expr e
  | Const _ | Closure _ | Sizeof _ | UnOp _ | BinOp _ ->
      None


let apply_to_expr (e : Exp.t) scope =
  scope_target_var_of_expr e |> Option.map ~f:(fun v -> (v, scope))


let apply_summary procname analyze_dependency ret_exp args =
  match analyze_dependency procname with
  | Some (proc_desc, summary) ->
      let all_formals =
        (Procdesc.get_ret_var proc_desc, Procdesc.get_ret_type proc_desc)
        :: Procdesc.get_pvar_formals proc_desc
      in
      let all_actuals = ret_exp :: args in
      if equal_int (List.length all_formals) (List.length all_actuals) then
        List.fold2_exn all_formals all_actuals ~init:[]
          ~f:(fun updates (formal_pvar, _) (actual, _) ->
            let formal_scope = VarToScope.find_or_bottom summary (Var.of_pvar formal_pvar) in
            apply_to_expr actual formal_scope
            |> Option.fold ~init:updates ~f:(fun accum exp_update -> exp_update :: accum) )
      else (* TODO: handle vararg procedures. *)
        []
  | None ->
      L.debug Analysis Verbose "@[<v2> ScopeLeakage: no summary found for procname `%a`@]@,"
        Procname.pp procname ;
      []


(** Copied from SimpleShape.ml. I think this should be shared in, e.g., PatternMatch. *)
let procname_of_exp (e : Exp.t) : Procname.t option =
  match e with Closure {name} | Const (Cfun name) -> Some name | _ -> None


(** Local inference of scopes for left-hand side variables from individual instructions. *)
let exec_instr tenv analyze_dependency instr =
  match (instr : Sil.instr) with
  | Load {id; typ} when Typ.is_pointer typ ->
      [(Var.of_id id, Scope.of_type tenv typ)]
  | Store {e1= Lvar pvar; typ} when Typ.is_pointer typ ->
      [(Var.of_pvar pvar, Scope.of_type tenv typ)]
  (* Uncomment this case once the analysis handles scope joins more precisely,
       otherwise we will consistently see instances of Top scope.
     | Store {e1= Lfield (Var lvar, _, _); typ} when Typ.is_pointer typ ->
         [(Var.of_id lvar, Scope.of_type tenv typ)] *)
  | Sil.Call ((ret_id, ret_typ), call_exp, actuals, _, _) ->
      procname_of_exp call_exp
      |> Option.value_map ~default:[] ~f:(fun callee_pname ->
             (* Check for a scope-generator procname and only if this fails check
                the scope of the return type. *)
             let gen_scope = Scope.of_generator_procname callee_pname in
             if Scope.is_bottom gen_scope then
               apply_summary callee_pname analyze_dependency (Exp.Var ret_id, ret_typ) actuals
             else [(Var.of_id ret_id, gen_scope)] )
  | _ ->
      []


(** A local analysis that assigns a scope to each left-hand side variable. *)
let locally_assign_scopes_to_vars proc_desc tenv analyze_dependency _aliasing =
  let exec_instr_in_context = exec_instr tenv analyze_dependency in
  let result = VarToScope.create 100 in
  Procdesc.iter_instrs
    (fun _ instr ->
      let entries = exec_instr_in_context instr in
      VarToScope.join_list result entries )
    proc_desc ;
  result


(** Checks whether an assignment to a field violates the scope nesting restriction. *)
let report_bad_field_assignments err_log proc_desc scoping =
  Procdesc.iter_instrs
    (fun _ instr ->
      match instr with
      | Store {e1= Lfield (Var lpvar, fldname, lvar_type); e2= Var rvar; loc; typ}
        when Typ.is_pointer typ ->
          let lvar = Var.of_id lpvar in
          let lvar_scope = VarToScope.find_or_bottom scoping lvar in
          let rvar_scope = VarToScope.find_or_bottom scoping (Var.of_id rvar) in
          if Scope.must_not_hold lvar_scope rvar_scope then
            let ltr = [Errlog.make_trace_element 0 loc "assignment of scoped object" []] in
            let lvar_type_str = PatternMatch.get_type_name lvar_type in
            let description =
              Format.asprintf
                "Field %s.%s exists in an object of scope %a and should not retain an object of \
                 scope %a"
                lvar_type_str
                (Fieldname.get_field_name fldname)
                Scope.pp lvar_scope Scope.pp rvar_scope
            in
            Reporting.log_issue proc_desc err_log ~loc ~ltr ScopeLeakage IssueType.scope_leakage
              description
      | _ ->
          () )
    proc_desc


let _pp_vars fs vars = F.pp_print_list ~pp_sep:pp_comma_sep (Pvar.pp Pp.text) fs vars

(** Maintain only entries for the (pointer-typed) procedure's parameter variables and the return
    variable. *)
let to_summary proc_desc scoping =
  let ret_pvar_typ = (Procdesc.get_ret_var proc_desc, Procdesc.get_ret_type proc_desc) in
  let summary_vars = ret_pvar_typ :: Procdesc.get_pvar_formals proc_desc in
  let summary = VarToScope.create (1 + List.length summary_vars) in
  List.iter summary_vars ~f:(fun (pvar, pvar_typ) ->
      if Typ.is_pointer pvar_typ then
        let var = Var.of_pvar pvar in
        let var_scope = VarToScope.find_or_bottom scoping var in
        VarToScope.join summary var var_scope ) ;
  summary


(** Un-prefix if we need to debug the configuration parsing code. *)
let _print_config () = L.debug Analysis Quiet "%a@\n" AnalysisConfig.pp config

(** Checks whether the given procedure does not violate the scope nesting restriction. *)
let checker {InterproceduralAnalysis.proc_desc; tenv; err_log; analyze_dependency} =
  let aliasing = Aliasing.of_proc_desc proc_desc in
  let scoping = locally_assign_scopes_to_vars proc_desc tenv analyze_dependency aliasing in
  join_scopes_across_alias_sets aliasing scoping ;
  report_bad_field_assignments err_log proc_desc scoping ;
  let summary = to_summary proc_desc scoping in
  Some summary
