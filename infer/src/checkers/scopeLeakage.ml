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

let pp_loc_opt fmt loc_opt =
  match loc_opt with
  | Some loc ->
      Location.pp_file_pos fmt loc
  | None ->
      F.pp_print_string fmt "(unknown source location)"


(* Converts an expression that amounts to a pointer variable a Var
   and returns None for any other type of expression. *)
let rec var_of_ptr_exp (exp_var : Exp.t) =
  match exp_var with
  | Var id ->
      Some (Var.of_id id)
  | Exn exp' ->
      var_of_ptr_exp exp'
  | Lvar pvar ->
      Some (Var.of_pvar pvar)
  | Cast (typ, obj_exp) when Typ.is_pointer typ ->
      var_of_ptr_exp obj_exp
  | _ ->
      None


(* A module for parsing a JSON configuration into a custom data type. *)
module AnalysisConfig : sig
  (** A class name and the method names of that class that are known to "generate" a scope. That is,
      return objects of a known scope. *)
  type classname_methods = {classname: string; methods: string list}

  (** Lists of generators for the different types of scopes. *)
  type generators = classname_methods list

  (** Information defining each scope. *)
  type scope_def = {classname: string; generators: generators}

  type must_not_hold_pair = {holder: string; held: string}

  type t =
    { annotation_classname: string
    ; scope_defs: scope_def list
    ; must_not_hold_pairs: must_not_hold_pair list }

  val empty : t

  val parse : Yojson.Safe.t -> t
  (** Parses a JSON configuration into a custom data type. *)

  val pp : F.formatter -> t -> unit
end = struct
  type classname_methods = {classname: string; methods: string list}

  type generators = classname_methods list

  type scope_def = {classname: string; generators: generators}

  type must_not_hold_pair = {holder: string; held: string}

  type t =
    { annotation_classname: string
    ; scope_defs: scope_def list
    ; must_not_hold_pairs: must_not_hold_pair list }

  let empty = {annotation_classname= ""; scope_defs= []; must_not_hold_pairs= []}

  let pp fmt config =
    let pp_list pp_elt = F.pp_print_list ~pp_sep:pp_comma_sep pp_elt in
    let pp_methods fmt methods =
      let pp_method fmt methodname = F.fprintf fmt {|"%s"|} methodname in
      (pp_list pp_method) fmt methods
    in
    let pp_generator fmt {classname; methods} =
      F.fprintf fmt {| { "classname": "%s", "methods": [%a] } |} classname pp_methods methods
    in
    let pp_scope fmt {classname; generators} =
      F.fprintf fmt {| { "classname": "%s", "generators": [%a] } |} classname (pp_list pp_generator)
        generators
    in
    let pp_must_not_hold_pair fmt {holder; held} =
      F.fprintf fmt {| { "holder": "%s", "held": "%s" } |} holder held
    in
    F.fprintf fmt
      {|"scope-leakage-config" : {
  "annot-classname": "%s",
  "scopes": [
    %a
  ],
  "must-not-hold": {%a}
}
      |}
      config.annotation_classname (pp_list pp_scope) config.scope_defs
      (pp_list pp_must_not_hold_pair) config.must_not_hold_pairs


  (** Finds a named JSON node in a map and aborts with an informative message otherwise. *)
  let find_node map key node =
    try Hashtbl.find map key
    with _ ->
      L.die UserError "Missing key \"%s\" in association node %a!@\n" key Yojson.Safe.pp node


  (* Converts a JSON `Assoc into a Hashtbl. *)
  let json_assoc_list_to_map assoc_list =
    let result = Hashtbl.create 10 in
    List.iter assoc_list ~f:(fun (key, node) -> Hashtbl.add result key node) ;
    result


  (* Converts a JSON string list node into a list of strings. *)
  let json_list_to_string_list node =
    match node with
    | `List nodes ->
        List.map nodes ~f:Yojson.Safe.Util.to_string
    | _ ->
        L.die UserError "Failed parsing a list of strings from %a!@\n" Yojson.Safe.pp node


  (** node is a JSON entry of the form "classname" : string, "methods": [list of strings]. *)
  let parse_classname_methods node =
    match node with
    | `Assoc assoc_list ->
        let node_as_map = json_assoc_list_to_map assoc_list in
        let classname = Yojson.Safe.Util.to_string (find_node node_as_map "classname" node) in
        let methods = json_list_to_string_list (find_node node_as_map "methods" node) in
        {classname; methods}
    | _ ->
        L.die UserError "Failed parsing a classname+methods node from %a!@\n" Yojson.Safe.pp node


  let parse_generators node =
    match node with
    | `List list_node ->
        List.map list_node ~f:parse_classname_methods
    | _ ->
        L.die UserError "Failed parsing a list of classname+methods list from %a!@\n" Yojson.Safe.pp
          node


  let parse_scope node =
    match node with
    | `Assoc generators_list ->
        let node_as_map = json_assoc_list_to_map generators_list in
        let classname_node = find_node node_as_map "classname" node in
        let generators_node = find_node node_as_map "generators" node in
        { classname= Yojson.Safe.Util.to_string classname_node
        ; generators= parse_generators generators_node }
    | _ ->
        L.die UserError "Failed parsing scope node from %a!@\n" Yojson.Safe.pp node


  let parse_scope_list node =
    match node with
    | `List node_list ->
        List.map node_list ~f:parse_scope
    | _ ->
        L.die UserError "Failed parsing a list of scopes from %a" Yojson.Safe.pp node


  let parse_must_not_hold_pair node =
    match node with
    | `Assoc node_assoc_list ->
        let node_as_map = json_assoc_list_to_map node_assoc_list in
        let left_node = find_node node_as_map "holds" node in
        let right_node = find_node node_as_map "held" node in
        {holder= Yojson.Safe.Util.to_string left_node; held= Yojson.Safe.Util.to_string right_node}
    | _ ->
        L.die UserError "Failed parsing a must-not-hold pair from %a!@\n" Yojson.Safe.pp node


  let parse_must_not_hold node =
    match node with
    | `List node_list ->
        List.map node_list ~f:parse_must_not_hold_pair
    | _ ->
        L.die UserError "Failed parsing a must-not-hold pair from %a!@\n" Yojson.Safe.pp node


  (** Basic semantic checks. *)
  let validate {scope_defs; must_not_hold_pairs} =
    let scope_names = List.map scope_defs ~f:(fun {classname} -> classname) in
    List.iter must_not_hold_pairs ~f:(fun {holder; held} ->
        if not (List.mem scope_names holder ~equal:equal_string) then
          L.die UserError
            "Failed validating scope-leakage-config: scope name %s appearing in 'must-not-hold' is \
             not listed in as a scope!@\n"
            holder ;
        if not (List.mem scope_names held ~equal:equal_string) then
          L.die UserError
            "Failed validating scope-leakage-config: scope name %s appearing in 'must-not-hold' is \
             not listed in as a scope!@\n"
            held )


  let parse node =
    match node with
    | `Assoc node_assoc_list ->
        let node_as_map = json_assoc_list_to_map node_assoc_list in
        let annot_classname_node = find_node node_as_map "annot-classname" node in
        let scopes_node = find_node node_as_map "scopes" node in
        let must_not_hold_node = find_node node_as_map "must-not-hold" node in
        let result =
          { annotation_classname= Yojson.Safe.Util.to_string annot_classname_node
          ; scope_defs= parse_scope_list scopes_node
          ; must_not_hold_pairs= parse_must_not_hold must_not_hold_node }
        in
        validate result ;
        result
    | `List [] ->
        L.debug Analysis Verbose "scope-leakage-config is empty!@\n" ;
        empty
    | _ ->
        L.die UserError "Failed parsing a scope-leakage-config node from %a!@\n" Yojson.Safe.pp node
end

(** Parse the configuration into a global. *)
let config =
  if Config.is_checker_enabled ScopeLeakage then AnalysisConfig.parse Config.scope_leakage_config
  else AnalysisConfig.empty


(** Retrieves the optional location from a struct corresponding to a Java type. *)
let get_struct_loc {Struct.class_info} =
  match (class_info : Struct.ClassInfo.t) with JavaClassInfo {loc} -> loc | _ -> None


(* A module for representing declarations of scopes. *)
module ScopeDeclaration : sig
  type t

  val for_annotation : Typ.Name.t option -> Location.t option -> t

  val for_super : Typ.Name.t -> Location.t option -> t

  val for_generator : Procname.t -> Location.t -> t

  val simplicity_level : t -> int
  (* A level, between 0 and 2, which represents the simplicity of the declaration in terms of explaining an error to a user. *)

  val pp : F.formatter -> t -> unit

  val pp_debug : F.formatter -> t -> unit

  val trace_elem : t -> Errlog.loc_trace_elem
end = struct
  type declaration_kind =
    | ViaAnnotation of Typ.Name.t option
    | ViaSupertype of Typ.Name.t
    | ViaGenerator of Procname.t

  type t = {kind: declaration_kind; loc: Location.t option}

  let pp fmt {kind; loc= _} =
    match kind with
    | ViaAnnotation (Some typename) ->
        F.fprintf fmt "Scope declared via annotation on %a" Typ.Name.pp typename
    | ViaAnnotation None ->
        F.fprintf fmt "Scope declared via annotation"
    | ViaSupertype super ->
        F.fprintf fmt "Scope declared via annotation on super-type %a" Typ.Name.pp super
    | ViaGenerator gen_procname ->
        F.fprintf fmt "Scope declared via call to %a" Procname.pp gen_procname


  let pp_debug fmt ({loc} as decl) = F.fprintf fmt "%a: %a" pp_loc_opt loc pp decl

  let trace_elem {kind; loc} =
    let trace_loc = Option.value loc ~default:Location.dummy in
    let msg =
      match kind with
      | ViaAnnotation (Some typename) ->
          "Scope declared via annotation on " ^ Typ.Name.to_string typename
      | ViaAnnotation None ->
          "Scope declared via annotation"
      | ViaSupertype super ->
          "Scope declared via annotation on super-type " ^ Typ.Name.to_string super
      | ViaGenerator gen_procname ->
          "Scope declared via call to " ^ Procname.to_simplified_string gen_procname
    in
    Errlog.make_trace_element 0 trace_loc msg []


  let simplicity_level {kind} =
    match kind with ViaAnnotation _ -> 0 | ViaSupertype _ -> 1 | ViaGenerator _ -> 2


  let for_annotation typename loc = {kind= ViaAnnotation typename; loc}

  let for_super super_name loc = {kind= ViaSupertype super_name; loc}

  let for_generator procname loc = {kind= ViaGenerator procname; loc= Some loc}
end

module ScopeAccess : sig
  type access_kind =
    | ViaField of Fieldname.t
    | ViaParameter of Procname.t * int
    | ViaReturnExp of Procname.t

  type t = {kind: access_kind; loc: Location.t option}

  val for_field : Fieldname.t -> Location.t option -> t

  val for_parameter : Procname.t -> int -> Location.t option -> t

  val for_return_exp : Procname.t -> Location.t option -> t

  val pp : F.formatter -> t -> unit

  val pp_debug : F.formatter -> t -> unit

  val trace_elem : t -> Errlog.loc_trace_elem
end = struct
  type access_kind =
    | ViaField of Fieldname.t
    | ViaParameter of Procname.t * int
    | ViaReturnExp of Procname.t

  type t = {kind: access_kind; loc: Location.t option}

  let for_field fldname loc = {kind= ViaField fldname; loc}

  let for_parameter procname pos loc = {kind= ViaParameter (procname, pos); loc}

  let for_return_exp procname loc = {kind= ViaReturnExp procname; loc}

  let pp fmt {kind; loc= _} =
    match kind with
    | ViaField fld ->
        F.fprintf fmt "Scoped object assigned to field %a" Fieldname.pp fld
    | ViaParameter (procname, position) ->
        F.fprintf fmt "Scoped object passed via parameter %i of %a" position Procname.pp procname
    | ViaReturnExp procname ->
        F.fprintf fmt "Scoped object returned from %a" Procname.pp procname


  let pp_debug fmt ({loc} as access) = F.fprintf fmt "%a: %a" pp_loc_opt loc pp access

  let trace_elem {kind; loc} =
    let trace_loc = Option.value loc ~default:Location.dummy in
    let msg =
      match kind with
      | ViaField fld ->
          Printf.sprintf "Scoped object assigned to field %s" (Fieldname.to_string fld)
      | ViaParameter (procname, position) ->
          Printf.sprintf "Scoped object passed via parameter %i of %s" position
            (Procname.to_simplified_string procname)
      | ViaReturnExp procname ->
          Printf.sprintf "Scoped object returned from %s" (Procname.to_simplified_string procname)
    in
    Errlog.make_trace_element 0 trace_loc msg []
end

(** A module for defining scopes and basic operations on scopes as well as extracting scopes from
    type annotations. *)
module Scope : sig
  type explained_scope

  type t = explained_scope list

  val bottom : t
  (** Represents an empty set of scopes. *)

  val is_bottom : t -> bool

  val join : t -> t -> t

  val of_type : Tenv.t -> Typ.t -> t
  (** Infers a scope for the given type. *)

  val make_for_generator : Typ.Name.t -> Procname.t -> Location.t -> t

  val must_not_hold_violations : t -> t -> (explained_scope * explained_scope) list

  val of_generator_procname : Procname.t -> Typ.Name.t option
  (** Matches a procedure name with the scope of the returned object (or None if the scope of the
      returned object is unknown) by matching it against the set of known generators. *)

  val extend_with_parameter : t -> Procname.t -> int -> Location.t option -> t

  val extend_with_return_exp : t -> Procname.t -> Location.t option -> t

  val extend_with_field_access : t -> Fieldname.t -> Location.t option -> t

  val get_field_path : explained_scope -> Fieldname.t list

  val pp : F.formatter -> t -> unit

  (* val pp_explained_scope : F.formatter -> explained_scope -> unit *)

  val pp_explained_scope_debug : F.formatter -> explained_scope -> unit

  val pp_explained_type : F.formatter -> explained_scope -> unit

  val trace : explained_scope -> Errlog.loc_trace
end = struct
  type explained_scope = {typname: Typ.Name.t; path: ScopeAccess.t list; dst: ScopeDeclaration.t}

  (** Note that the length of the declared list and the accessed list are expected to be quite short
      (on the order of single digits). The explanations in the 'declared' field are always
      degenerate. That is, their 'path' field is empty. *)
  type t = explained_scope list

  let bottom = []

  let is_bottom scopes = List.is_empty scopes

  let field_depth {path} =
    List.count path ~f:(fun {ScopeAccess.kind} ->
        match kind with ScopeAccess.ViaField _ -> true | _ -> false )


  let is_declared explained = equal_int 0 (field_depth explained)

  (* Returns the names of the fields appearing in the path component,
     in the same order. *)
  let get_field_path {path} =
    List.rev_filter_map path ~f:(function ScopeAccess.{kind= ViaField f} -> Some f | _ -> None)


  let pp_explained_scope fmt {typname; path; dst} =
    let pp_access_path fmt access_path =
      F.pp_print_list ~pp_sep:pp_comma_sep ScopeAccess.pp fmt access_path
    in
    F.fprintf fmt "%a:[%a.%a]" Typ.Name.pp typname pp_access_path path ScopeDeclaration.pp dst


  let pp_explained_scope_debug fmt {typname; path; dst} =
    let pp_newline_sep fmt () = F.pp_print_string fmt "\n" in
    let pp_access_path fmt access_path =
      F.pp_print_list ~pp_sep:pp_newline_sep ScopeAccess.pp_debug fmt access_path
    in
    F.fprintf fmt "%a:[%a.\n%a]" Typ.Name.pp typname pp_access_path path ScopeDeclaration.pp_debug
      dst


  let trace {path; dst} =
    let path_trace = List.map path ~f:(fun access -> ScopeAccess.trace_elem access) in
    let dst_trace = [ScopeDeclaration.trace_elem dst] in
    path_trace @ dst_trace


  let pp_explained_type fmt {typname} = F.fprintf fmt "%s" (Typ.Name.name typname)

  let pp fmt scopes =
    let pp_list pp_single fmt l =
      if List.is_empty l then F.fprintf fmt "[]"
      else F.pp_print_list ~pp_sep:pp_comma_sep pp_single fmt l
    in
    let pp_explained_list fmt accessed_list = pp_list pp_explained_scope fmt accessed_list in
    F.fprintf fmt "%a" pp_explained_list scopes


  (* Retains one (the simplest) explained scope per scope type name. The order of the output list
     is non-deterministic. *)
  let normalize scopes =
    let simpler explained1 explained2 =
      (* Lexicographic simplicity measure: first we prefer shorter field paths,
         then overall access paths, and finally simplier declarations.
         We multiply by exponents of 2 to allow bit-shift optimizations.
      *)
      let lex_level ({path; dst} as explained) =
        (256 * field_depth explained)
        + (4 * List.length path)
        + ScopeDeclaration.simplicity_level dst
      in
      lex_level explained1 <= lex_level explained2
    in
    (* Create a map from scope type names to the simplest explained scopes that reference them. *)
    let module TypenameMap = Hashtbl.Make (Typ.Name) in
    let scope_map = TypenameMap.create 3 in
    List.iter scopes ~f:(fun scope ->
        let typ = scope.typname in
        match TypenameMap.find_opt scope_map typ with
        | None ->
            TypenameMap.add scope_map typ scope
        | Some scope' ->
            if simpler scope scope' then TypenameMap.replace scope_map typ scope ) ;
    (* Now, dump the simplest scopes to a list. *)
    TypenameMap.to_seq_values scope_map |> Seq.fold_left (fun accum scope -> scope :: accum) []


  let join scopes1 scopes2 = normalize (scopes1 @ scopes2)

  let extend_with_parameter scopes procname pos loc =
    List.map scopes ~f:(fun scope ->
        let parameter_access = ScopeAccess.for_parameter procname pos loc in
        {scope with path= parameter_access :: scope.path} )


  let extend_with_return_exp scopes procname loc =
    List.map scopes ~f:(fun scope ->
        let return_access = ScopeAccess.for_return_exp procname loc in
        {scope with path= return_access :: scope.path} )


  let extend_with_field_access scopes fldname loc =
    List.map scopes ~f:(fun scope ->
        let field_access = ScopeAccess.for_field fldname loc in
        {scope with path= field_access :: scope.path} )


  (** TODO: figure out if I can hide this inside of_scope_class_name (whether it's evaluated just
      once). *)
  let scope_class_names =
    List.map config.scope_defs ~f:(fun scope -> scope.AnalysisConfig.classname)


  (** Returns the scope corresponding to the class implementing it, or None if the given name does
      not correspond to any such class. *)
  let of_scope_class_name name =
    if List.mem scope_class_names name ~equal:String.equal then
      Some (Typ.Name.Java.from_string name)
    else None


  (** Converts the string-based relation in the configuration to a list of typename pairs. *)
  let must_not_hold_relation =
    List.map config.must_not_hold_pairs ~f:(fun {AnalysisConfig.holder; held} ->
        ( of_scope_class_name holder |> Option.value_exn
        , of_scope_class_name held |> Option.value_exn ) )


  (* Given a pair of explained scope lists, lists the pair of 'declared-scope x any-scope' from their
     Cartesian product that would cause a violation. That is, if storing the right-hand scope into an object
     with the left-hand scope would yield a scope leak. *)
  let must_not_hold_violations left_scopes right_scopes =
    let enumerate_product lst1 lst2 =
      List.fold lst1 ~init:[] ~f:(fun accum1 elem1 ->
          List.fold lst2 ~init:accum1 ~f:(fun accum2 elem2 -> (elem1, elem2) :: accum2) )
    in
    let single_scopes_prodct = enumerate_product left_scopes right_scopes in
    (* Checks whether the two given scope type names are included in the must_not_hold list
       of the analysis configuration. *)
    let mem_must_not_hold typname1 typname2 =
      List.exists must_not_hold_relation ~f:(fun (holder, held) ->
          Typ.Name.equal typname1 holder && Typ.Name.equal typname2 held )
    in
    List.filter single_scopes_prodct ~f:(fun (left_explained, right_explained) ->
        is_declared left_explained
        && mem_must_not_hold left_explained.typname right_explained.typname )


  (** Matches a parameter like "value = OuterScope.class" with the corresponding scope. *)
  let of_annot_param = function
    | {Annot.name= Some "value"; value= Class typ} when PatternMatch.type_is_class typ ->
        of_scope_class_name (PatternMatch.get_type_name typ)
    | _ ->
        None


  (** Traverses the given type to return a Struct.t, if one exists. *)
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
  let of_struct_annots typename struc =
    (* Lists the scopes appearing in the parameters. *)
    let of_annot {Annot.class_name; parameters} =
      if String.equal class_name config.annotation_classname then
        List.filter_map parameters ~f:of_annot_param
      else []
    in
    let struct_scope_types = List.concat_map struc.Struct.annots ~f:of_annot in
    let loc = get_struct_loc struc in
    List.map struct_scope_types ~f:(fun scope_type ->
        {typname= scope_type; path= []; dst= ScopeDeclaration.for_annotation typename loc} )


  (** Lists the scopes of all super-types of a given type name, based on their annotations. *)
  let of_supertypes tenv typname =
    Tenv.fold_supers tenv typname ~init:[] ~f:(fun super_typname struct_opt accum_scopes ->
        match struct_opt with
        (* fold_supers also traverses typname itself, so we need to exclude it explicitly. *)
        | Some super_struct when not (Typ.equal_name typname super_typname) ->
            let super_scopes = of_struct_annots (Some super_typname) super_struct in
            let extended_super_scopes =
              List.map super_scopes ~f:(fun {typname; path} ->
                  let super_decl =
                    ScopeDeclaration.for_super super_typname (get_struct_loc super_struct)
                  in
                  {typname; path; dst= super_decl} )
            in
            extended_super_scopes @ accum_scopes
        | _ ->
            accum_scopes )


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


  (** Lists the scopes associated with the given type based on its own annotations and the
      annotations of its super-types. *)
  let of_type tenv typ =
    let typename_opt = inner_typename typ in
    let annots_scope =
      match inner_struct_of_type tenv typ with
      | Some s ->
          of_struct_annots typename_opt s
      | None ->
          bottom
    in
    let supers_scope =
      match typename_opt with Some typname -> of_supertypes tenv typname | None -> bottom
    in
    annots_scope @ supers_scope


  (** Returns the scope for which there exists a generator that matches the given Java procedure
      name, if one exists, and bottom otherwise. *)
  let match_javaname scopes (jname : Procname.Java.t) =
    let open AnalysisConfig in
    let curr_classname = Procname.Java.get_class_name jname in
    let curr_method = Procname.Java.get_method jname in
    (* Checks whether curr_classname+curr_method exist in the list of generators. *)
    let match_generators generators =
      let matches_classname_methods {classname; methods} =
        String.equal classname curr_classname && List.exists methods ~f:(String.equal curr_method)
      in
      List.exists generators ~f:matches_classname_methods
    in
    List.find scopes ~f:(fun scope -> match_generators scope.generators)
    |> Option.bind ~f:(fun scope -> of_scope_class_name scope.classname)


  (** Given a config, generates a function that matches a procedure name with the scope of the
      returned object (or None if the scope of the returned object is unknown). *)
  let of_generator_procname_with_config {AnalysisConfig.scope_defs} procname =
    match procname with Procname.Java jname -> match_javaname scope_defs jname | _ -> None


  let of_generator_procname = of_generator_procname_with_config config

  let make_for_generator gen_typename procname loc =
    let generator_decl = ScopeDeclaration.for_generator procname loc in
    let generator_scope = {typname= gen_typename; path= []; dst= generator_decl} in
    [generator_scope]
end

(** A flow-insensitive alias analysis between local pointer variables. That is, only assignments
    between pointer variables are considered.

    The analysis is based on a union-find-style data structure where an alias set is represented by
    a single variable, and all other variables in that set reference the representative variable. To
    obtain a near-linear time complexity, we need to merge small alias sets into larger ones, which
    requires maintaining size information. We skip that, since we expect the alias sets to be small
    in practice and not worth the additional time/space for maintaining size counters. *)
module Aliasing = struct
  open VarMap

  (** Returns the representative of the given variable's alias set. *)
  let rec get_rep aliasing var =
    match find_opt aliasing var with Some higher -> get_rep aliasing higher | None -> var


  (** Infers a pair of aliased variables in case of a direct assignment between pointer variables. *)
  let alias_of_instr instr =
    let open Sil in
    let open Exp in
    match instr with
    | Load {id= lvar; typ; e} when Typ.is_pointer typ ->
        Option.bind (var_of_ptr_exp e) ~f:(fun rvar -> Some (Var.of_id lvar, rvar))
    | Store {e1= Lvar lhs; e2} ->
        Option.bind (var_of_ptr_exp e2) ~f:(fun rvar -> Some (Var.of_pvar lhs, rvar))
    (* Built-in casts are essentially assignments. *)
    | Call ((ret_id, _), Const (Cfun name), [(var_exp, _); (_, _)], _, _)
      when Procname.equal name BuiltinDecl.__cast ->
        Option.bind (var_of_ptr_exp var_exp) ~f:(fun rvar -> Some (Var.of_id ret_id, rvar))
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


  let pp fmt aliasing =
    VarMap.iter (fun k v -> F.fprintf fmt "%a -> %a, " Var.pp k Var.pp v) aliasing
end

(** A mapping from pointer variables to scopes. The mapping maintains the scopes only for the
    representative of each alias set. *)
module VarToScope : sig
  type t

  val create : Var.t VarMap.t -> int -> t

  val find_or_bottom : t -> Var.t -> Scope.t

  val join : t -> Var.t -> Scope.t -> unit

  val join_list : t -> (Var.t * Scope.t) list -> unit

  val pp : F.formatter -> t -> unit
end = struct
  open VarMap

  type t = {aliasing: Var.t VarMap.t; env: Scope.t VarMap.t}

  let create aliasing size : t = {aliasing; env= VarMap.create size}

  let find_or_bottom {aliasing; env} var =
    let var = Aliasing.get_rep aliasing var in
    find_opt env var |> Option.value ~default:Scope.bottom


  let join abs var scope =
    let var = Aliasing.get_rep abs.aliasing var in
    let current_scope = find_or_bottom abs var in
    let joined_scope = Scope.join scope current_scope in
    if not (Scope.is_bottom joined_scope) (* Keep the mapping sparse. *) then
      replace abs.env var joined_scope
    else (* joined_scope is bottom, meaning there is no key in the table. *)
      ()


  let join_list abs entries = List.iter entries ~f:(fun (var, scope) -> join abs var scope)

  let pp fmt {env} =
    let pp_pair fmt (fst, snd) = F.fprintf fmt "%a: %a" Var.pp fst Scope.pp snd in
    let pp_sep fmt () = F.pp_print_string fmt ",@," in
    F.fprintf fmt "@[<hv>[%a]@]" (F.pp_print_seq ~pp_sep pp_pair) (VarMap.to_seq env)
end

module Summary = VarToScope

(* The top-level variable to which we should assign a scope in the given
   expression, if this expression indeed starts with a variable. *)
let rec scope_target_var_of_ptr_expr (e : Exp.t) =
  match e with
  | Var id ->
      Some (Var.of_id id)
  | Lvar pvar ->
      Some (Var.of_pvar pvar)
  | Lfield (e, _, _) | Cast (_, e) | Exn e | Lindex (e, _) ->
      scope_target_var_of_ptr_expr e
  | Const _ | Closure _ | Sizeof _ | UnOp _ | BinOp _ ->
      None


(* If [e] is an expression that starts with a variable, returns an update pair
   to associate the given scopes with it. Otherwise, returns an empty list
    of updates. *)
let update_of_expr (e : Exp.t) (scopes : Scope.explained_scope list) =
  match scope_target_var_of_ptr_expr e with Some v -> [(v, scopes)] | None -> []


(* Creates a summary that assigns scopes to parameters based on the annotations
   associated with their types. *)
let type_based_summary attributes tenv procname =
  let result = VarToScope.create (VarMap.create 0) 0 in
  let formals = ProcAttributes.get_pvar_formals attributes in
  (* First, assign the scope of the return variable. *)
  let ret_var = Pvar.get_ret_pvar procname in
  let attributes = Attributes.load_exn procname in
  let ret_typ = ProcAttributes.to_return_type attributes in
  let ret_scope = Scope.of_type tenv ret_typ in
  VarToScope.join result (Var.of_pvar ret_var) ret_scope ;
  (* Second, assign the scopes of the parameters. *)
  List.iter formals ~f:(fun (pvar, typ) ->
      let scope = Scope.of_type tenv typ in
      VarToScope.join result (Var.of_pvar pvar) scope ) ;
  result


(* Retrieves a summary for the given procedure, if one exists, and generates a
   type-based summary otherwise. *)
let get_or_make_summary attributes tenv procname analyze_dependency =
  match analyze_dependency procname with
  | Ok summary ->
      summary
  | Error _ ->
      type_based_summary attributes tenv procname


(* Returns a list of scope updates for the (top-level) variables in the actual
   arguments and return variable of a function call. *)
let apply_summary tenv loc procname analyze_dependency ret_exp args =
  match Attributes.load procname with
  | Some attributes ->
      let summary = get_or_make_summary attributes tenv procname analyze_dependency in
      let ret_var = Pvar.get_ret_pvar procname in
      let formals = ProcAttributes.get_pvar_formals attributes in
      if equal_int (List.length formals) (List.length args) then
        let returned_var_scope = VarToScope.find_or_bottom summary (Var.of_pvar ret_var) in
        let extended_returned_var_scope =
          Scope.extend_with_return_exp returned_var_scope procname loc
        in
        let (return_updates : (Var.t * Scope.explained_scope list) list) =
          update_of_expr ret_exp extended_returned_var_scope
        in
        let formal_actual_pairs = List.zip_exn formals args in
        let arg_updates =
          List.foldi formal_actual_pairs ~init:[]
            ~f:(fun idx updates (formal_param, actual_param) ->
              let formal_pvar, _ = formal_param in
              let actual_expr, _ = actual_param in
              let formal_var_scope = VarToScope.find_or_bottom summary (Var.of_pvar formal_pvar) in
              let extended_var_scope =
                Scope.extend_with_parameter formal_var_scope procname idx loc
              in
              update_of_expr actual_expr extended_var_scope @ updates )
        in
        return_updates @ arg_updates
      else (* TODO: handle vararg procedures. *)
        []
  | None ->
      []


(** Copied from LineageShape.ml. I think this should be shared in, e.g., PatternMatch. *)
let procname_of_exp (e : Exp.t) : Procname.t option =
  match e with Closure {name} | Const (Cfun name) -> Some name | _ -> None


let exec_instr scope_env tenv analyze_dependency instr =
  match (instr : Sil.instr) with
  | Load {id; typ} when Typ.is_pointer typ ->
      let lhs_var = Var.of_id id in
      let typ_scope = Scope.of_type tenv typ in
      [(lhs_var, typ_scope)]
  | Store {e1= Lvar pvar; typ} when Typ.is_pointer typ ->
      let lhs_var = Var.of_pvar pvar in
      [(lhs_var, Scope.of_type tenv typ)]
  | Store {e1= Lfield (var_exp, fldname, _); typ; e2; loc} when Typ.is_pointer typ ->
      let lhs_var = Option.value_exn (var_of_ptr_exp var_exp) in
      let rhs_typ_scope = Scope.of_type tenv typ in
      let extended_typ_scope = Scope.extend_with_field_access rhs_typ_scope fldname (Some loc) in
      let rhs_updates =
        match scope_target_var_of_ptr_expr e2 with
        | Some rhs_var ->
            let rhs_var_scope = VarToScope.find_or_bottom scope_env rhs_var in
            let extended_rhs_scope =
              Scope.extend_with_field_access rhs_var_scope fldname (Some loc)
            in
            [(lhs_var, extended_rhs_scope)]
        | None ->
            []
      in
      rhs_updates @ [(lhs_var, extended_typ_scope)]
  | Call ((ret_id, _), call_exp, actuals, loc, _) ->
      procname_of_exp call_exp
      |> Option.value_map ~default:[] ~f:(fun callee_pname ->
             (* Check for a scope-generator procname and only if this fails use the summary. *)
             let opt_typename = Scope.of_generator_procname callee_pname in
             match opt_typename with
             | Some gen_typename ->
                 let generator_scope = Scope.make_for_generator gen_typename callee_pname loc in
                 [(Var.of_id ret_id, generator_scope)]
             | None ->
                 apply_summary tenv (Some loc) callee_pname analyze_dependency (Exp.Var ret_id)
                   actuals )
  | _ ->
      []


(** An analysis that assigns scopes to pointer variables. *)
let assign_scopes_to_vars proc_desc tenv analyze_dependency aliasing =
  let result = VarToScope.create aliasing 100 in
  let exec_instr_in_context = exec_instr result tenv analyze_dependency in
  let analysis_iteration () =
    Procdesc.iter_instrs
      (fun _ instr ->
        let updates = exec_instr_in_context instr in
        VarToScope.join_list result updates )
      proc_desc
  in
  (* Infers scopes via declarations and function summaries. *)
  analysis_iteration () ;
  (* Propagates scopes across stores to fields. *)
  analysis_iteration () ;
  result


(** Checks whether an assignment to a field yields a scope leak. *)
let report_bad_field_assignments err_log proc_desc scoping =
  let pp_field_path fmt path =
    let pp_dot_sep fmt () = F.pp_print_string fmt "." in
    F.pp_print_list ~pp_sep:pp_dot_sep Fieldname.pp fmt path
  in
  Procdesc.iter_instrs
    (fun _ instr ->
      match instr with
      | Store {e1= Lfield (lvar_exp, fldname, lvar_type); e2; loc; typ} when Typ.is_pointer typ -> (
          let lvar = Option.value_exn (var_of_ptr_exp lvar_exp) in
          match var_of_ptr_exp e2 with
          | Some rvar ->
              let lvar_type_str = PatternMatch.get_type_name lvar_type in
              let lvar_scope = VarToScope.find_or_bottom scoping lvar in
              let rvar_scope = VarToScope.find_or_bottom scoping rvar in
              let violations = Scope.must_not_hold_violations lvar_scope rvar_scope in
              List.iter violations ~f:(fun (left, right) ->
                  let access_path = fldname :: Scope.get_field_path right in
                  let leak_level = List.length access_path in
                  let ltr =
                    Scope.trace left @ Scope.trace right
                    @ [Errlog.make_trace_element 0 loc "Assignment of scoped object to field" []]
                  in
                  let description =
                    if equal_int leak_level 1 then
                      Format.asprintf
                        "Class '%s' has scope '%a' and must not retain an object of scope '%a' via \
                         the field '%a'."
                        lvar_type_str Scope.pp_explained_type left Scope.pp_explained_type right
                        Fieldname.pp fldname
                    else
                      Format.asprintf
                        "Class '%s' has scope '%a' and must not retain an object of scope '%a' via \
                         the field path '%a' (%i-level leak)."
                        lvar_type_str Scope.pp_explained_type left Scope.pp_explained_type right
                        pp_field_path access_path leak_level
                  in
                  L.debug Analysis Verbose
                    "Detected violation at %a with\nLeft scope: %a\nRight scope: %a\n"
                    (Sil.pp_instr ~print_types:false Pp.text)
                    instr Scope.pp_explained_scope_debug left Scope.pp_explained_scope_debug right ;
                  Reporting.log_issue proc_desc err_log ~loc ~ltr ScopeLeakage
                    IssueType.scope_leakage description )
          | None ->
              () )
      | _ ->
          () )
    proc_desc


let _pp_vars fmt vars = F.pp_print_list ~pp_sep:pp_comma_sep (Pvar.pp Pp.text) fmt vars

(** Retain only entries for the (pointer-typed) procedure's parameter variables and the return
    variable. *)
let to_summary proc_desc scoping =
  let ret_pvar_typ = (Procdesc.get_ret_var proc_desc, Procdesc.get_ret_type proc_desc) in
  let summary_vars = ret_pvar_typ :: Procdesc.get_pvar_formals proc_desc in
  let empty_aliasing = VarMap.create 0 in
  let summary = VarToScope.create empty_aliasing (1 + List.length summary_vars) in
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
  L.debug Analysis Verbose "SCOPE LEAKAGE: Starting analysis of %a\n" Procname.pp
    (Procdesc.get_proc_name proc_desc) ;
  let aliasing = Aliasing.of_proc_desc proc_desc in
  L.debug Analysis Verbose "SCOPE LEAKAGE: Aliasing= %a\n" Aliasing.pp aliasing ;
  let scope_env = assign_scopes_to_vars proc_desc tenv analyze_dependency aliasing in
  report_bad_field_assignments err_log proc_desc scope_env ;
  let summary = to_summary proc_desc scope_env in
  L.debug Analysis Verbose "SCOPE LEAKAGE: Summary of %a= %a\n" Procname.pp
    (Procdesc.get_proc_name proc_desc)
    VarToScope.pp summary ;
  Some summary
