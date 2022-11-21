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

  let pp_comma_sep fmt () = F.pp_print_string fmt ", "

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
    with _ -> L.die UserError "Missing key \"%s\" in association node %a" key Yojson.Basic.pp node


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
        L.die UserError "Failed parsing a list of strings from %a" Yojson.Basic.pp node


  (** node is a JSON entry of the form "classname" : string, "methods": [list of strings]. *)
  let parse_classname_methods node =
    match node with
    | `Assoc assoc_list ->
        let node_as_map = json_assoc_list_to_map assoc_list in
        let classname = Yojson.Basic.Util.to_string (find_node node_as_map "classname" node) in
        let methods = json_list_to_string_list (find_node node_as_map "methods" node) in
        {classname; methods}
    | _ ->
        L.die UserError "Failed parsing a classname+methods node from %a" Yojson.Basic.pp node


  let parse_generators node =
    match node with
    | `List list_node ->
        List.map list_node ~f:parse_classname_methods
    | _ ->
        L.die UserError "Failed parsing a list of classname+methods list from %a" Yojson.Basic.pp
          node


  let parse_scope node =
    match node with
    | `Assoc generators_list ->
        let node_as_map = json_assoc_list_to_map generators_list in
        let classname_node = find_node node_as_map "classname" node in
        let generators_node = find_node node_as_map "generators" node in
        { classname= Yojson.Basic.Util.to_string classname_node
        ; generators= parse_generators generators_node }
    | _ ->
        L.die UserError "Failed parsing scope node from %a" Yojson.Basic.pp node


  (** node has the form "application-scope": string, "context-scope": string, "user-scope": string. *)
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
        L.die UserError "Failed parsing a must-not-hold pair from %a" Yojson.Basic.pp node


  let parse_must_not_hold node =
    match node with
    | `List node_list ->
        List.map node_list ~f:parse_must_not_hold_pair
    | _ ->
        L.die UserError "Failed parsing a must-not-hold pair from %a" Yojson.Basic.pp node


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
        L.debug Analysis Verbose "scope-leakage-config is empty" ;
        {annotation_classname= ""; scopes= []; must_not_hold_pairs= []}
    | _ ->
        L.die UserError "Failed parsing a scope-leakage-config node from %a" Yojson.Basic.pp node
end

(** Parse the configuration once and for all. *)
let config = AnalysisConfig.parse Config.scope_leakage_config

(** A module for defining scopes and basic operations on scopes as well as extracting scopes from
    type annotations. *)
module AbsScope : sig
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
    List.exists must_not_hold_scopes ~f:(fun (holder, held) -> equal s1 holder && equal s2 held)
    || equal s2 Top


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
  include Hashtbl.Make (Var)

  let find_or_bottom tbl k =
    match find_opt tbl k with Some scope -> scope | None -> AbsScope.bottom


  let join tbl k d =
    match find_opt tbl k with Some d' -> replace tbl k (AbsScope.join d d') | None -> add tbl k d
end

(** A flow-insensitive alias analysis between local pointer variables. That is, only assignments
    between pointer variables are considered.

    The analysis is based on a union-find-style data structure where an alias set is represented by
    a single variable, and all other variables in that set reference the representative variable. To
    obtain a near-linear time complexity, we need to merge small alias sets into larger ones, which
    requires maintaining size information. We skip that, since we expect the alias sets to be small
    in practice and not worth the additional time/space for maintaining size counters. *)
module Aliasing = struct
  include Hashtbl.Make (Var)

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
    | Store {e1= Lvar lvar; e2= Var rvar} ->
        Some (Var.of_pvar lvar, Var.of_id rvar)
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
let join_scopes_for_alias_sets aliasing scoping : unit =
  (* Join all scopes into the representative of the alias set. *)
  VarToScope.iter
    (fun v rep ->
      let v_scope = VarToScope.find_or_bottom scoping v in
      VarToScope.join scoping rep v_scope )
    aliasing ;
  (* Now copy the representative's scope to all other members in its alias set.
     filter_map_inplace is safe here, since we don't change the set of keys.
  *)
  VarToScope.filter_map_inplace
    (fun v _ ->
      let rep = Aliasing.get_rep aliasing v in
      let rep_scope = VarToScope.find_or_bottom scoping rep in
      Some rep_scope )
    scoping


(** Local inference of scopes for left-hand side variables from individual instructions. *)
let scope_of_instr tenv instr =
  match (instr : Sil.instr) with
  | Load {id= lvar; typ} when Typ.is_pointer typ ->
      Some (Var.of_id lvar, AbsScope.of_type tenv typ)
  | Store {e1= Lvar lvar; typ} when Typ.is_pointer typ ->
      Some (Var.of_pvar lvar, AbsScope.of_type tenv typ)
  | Sil.Call ((ret_id, ret_type), Const (Cfun callee_pname), _, _, _) ->
      (* Check for a scope-generator procname and only if this fails check
         the scope of the return type. *)
      let ret_scope =
        let gen_scope = AbsScope.of_generator_procname callee_pname in
        if AbsScope.is_bottom gen_scope then AbsScope.of_type tenv ret_type else gen_scope
      in
      Some (Var.of_id ret_id, ret_scope)
  | Sil.Call ((ret_id, ret_type), _, _, _, _) ->
      Some (Var.of_id ret_id, AbsScope.of_type tenv ret_type)
  | _ ->
      None


(** A local analysis that assigns a scope to each left-hand side variable. *)
let locally_assign_scopes_to_vars proc_desc tenv =
  let scope_of_instr_with_tenv = scope_of_instr tenv in
  let result = VarToScope.create 100 in
  Procdesc.iter_instrs
    (fun _ instr ->
      match scope_of_instr_with_tenv instr with
      | Some (var, scope) ->
          VarToScope.join result var scope
      | None ->
          () )
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
          let rvar_scope = VarToScope.find scoping (Var.of_id rvar) in
          if AbsScope.must_not_hold lvar_scope rvar_scope then
            let ltr = [Errlog.make_trace_element 0 loc "storage of user-scoped field" []] in
            let lvar_type_str = PatternMatch.get_type_name lvar_type in
            let description =
              Format.asprintf
                "Assignee of field %s.%s has scope %a and should not be assigned an object of \
                 scope %a!"
                lvar_type_str
                (Fieldname.get_field_name fldname)
                AbsScope.pp lvar_scope AbsScope.pp rvar_scope
            in
            Reporting.log_issue proc_desc err_log ~loc ~ltr ScopeLeakage IssueType.scope_leakage
              description
      | _ ->
          () )
    proc_desc


(** Un-prefix if we need to debug the configuration parsing code. *)
let _print_config () = L.debug Analysis Quiet "%a\n" AnalysisConfig.pp config

(** Checks whether the given procedure does not violate the scope nesting restriction. *)
let checker {IntraproceduralAnalysis.proc_desc; tenv; err_log} =
  let scoping = locally_assign_scopes_to_vars proc_desc tenv in
  let aliasing = Aliasing.of_proc_desc proc_desc in
  join_scopes_for_alias_sets aliasing scoping ;
  report_bad_field_assignments err_log proc_desc scoping
