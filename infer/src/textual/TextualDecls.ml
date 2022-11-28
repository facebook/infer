(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Textual

module QualifiedNameHashtbl = Hashtbl.Make (struct
  type t = qualified_procname

  let equal = equal_qualified_procname

  let hash = hash_qualified_procname
end)

type t =
  { globals: Global.t VarName.Hashtbl.t
  ; procnames: (ProcDecl.t * bool) QualifiedNameHashtbl.t
        (** the boolean records whether an implementation was given *)
  ; structs: Struct.t TypeName.Hashtbl.t
  ; sourcefile: SourceFile.t }

let init sourcefile =
  { globals= VarName.Hashtbl.create 17
  ; procnames= QualifiedNameHashtbl.create 17
  ; structs= TypeName.Hashtbl.create 17
  ; sourcefile }


type error =
  | FieldDeclaredTwice of qualified_fieldname
  | GlobalDeclaredTwice of Global.t
  | NodeImplementedTwice of qualified_procname * NodeName.t
  | ParameterDeclatedTwice of qualified_procname * VarName.t
  | ProcImplementedTwice of qualified_procname
  | StructDeclaredTwice of TypeName.t

let pp_error sourcefile fmt err =
  F.fprintf fmt "%a: SIL consistency error: " SourceFile.pp sourcefile ;
  match err with
  | FieldDeclaredTwice qualified_fieldname ->
      F.fprintf fmt "field %a is declared twice in type %a" FieldName.pp qualified_fieldname.name
        TypeName.pp qualified_fieldname.enclosing_class
  | GlobalDeclaredTwice global ->
      F.fprintf fmt "global %a is declared twice in the same file" VarName.pp global.name
  | NodeImplementedTwice (qualified_procname, label) ->
      F.fprintf fmt "node %a is implemented twice in the same function %a" NodeName.pp label
        pp_qualified_procname qualified_procname
  | ParameterDeclatedTwice (qualified_procname, varname) ->
      F.fprintf fmt "parameter %a is declared twice in the same function %a" VarName.pp varname
        pp_qualified_procname qualified_procname
  | ProcImplementedTwice qualified_procname ->
      F.fprintf fmt "function %a is implemented twice in the same file" pp_qualified_procname
        qualified_procname
  | StructDeclaredTwice tname ->
      F.fprintf fmt "type %a is declared twice in the same file" TypeName.pp tname


let declare_global decls (global : Global.t) =
  VarName.Hashtbl.replace decls.globals global.name global


let is_global_declared decls (global : Global.t) = VarName.Hashtbl.mem decls.globals global.name

let is_proc_implemented decls procdecl =
  QualifiedNameHashtbl.find_opt decls.procnames procdecl.ProcDecl.qualified_name
  |> Option.value_map ~default:false ~f:snd


let declare_proc decls ~is_implemented (pname : ProcDecl.t) =
  let is_already_implemented = is_proc_implemented decls pname in
  QualifiedNameHashtbl.replace decls.procnames pname.qualified_name
    (pname, is_already_implemented || is_implemented)


let declare_struct decls (s : Struct.t) = TypeName.Hashtbl.replace decls.structs s.name s

let is_struct_declared decls (s : Struct.t) = TypeName.Hashtbl.mem decls.structs s.name

let is_field_declared decls ({enclosing_class; name} : qualified_fieldname) =
  match TypeName.Hashtbl.find_opt decls.structs enclosing_class with
  | None ->
      false
  | Some struct_ ->
      List.exists struct_.fields ~f:(fun {FieldDecl.qualified_name} ->
          FieldName.equal qualified_name.name name )


let get_global decls vname = VarName.Hashtbl.find_opt decls.globals vname

let get_fielddecl decls ({name; enclosing_class} : qualified_fieldname) =
  let open IOption.Let_syntax in
  let* struct_ = TypeName.Hashtbl.find_opt decls.structs enclosing_class in
  List.find struct_.Struct.fields ~f:(fun ({qualified_name} : FieldDecl.t) ->
      FieldName.equal qualified_name.name name )


let get_procname decls qualified_name =
  QualifiedNameHashtbl.find_opt decls.procnames qualified_name |> Option.map ~f:fst


let get_struct decls tname = TypeName.Hashtbl.find_opt decls.structs tname

let fold_globals decls ~init ~f =
  VarName.Hashtbl.fold (fun key data x -> f x key data) decls.globals init


let fold_procnames decls ~init ~f =
  QualifiedNameHashtbl.fold (fun _ (procname, _) x -> f x procname) decls.procnames init


let fold_structs decls ~init ~f =
  TypeName.Hashtbl.fold (fun key data x -> f x key data) decls.structs init


let source_file {sourcefile; _} = sourcefile

let check_fieldnames_not_declared_twice errors struct_ =
  List.fold struct_.Struct.fields ~init:(errors, FieldName.Set.empty)
    ~f:(fun (errors, seen) {FieldDecl.qualified_name} ->
      let name = qualified_name.name in
      let errors =
        if FieldName.Set.mem name seen then FieldDeclaredTwice qualified_name :: errors else errors
      in
      (errors, FieldName.Set.add name seen) )
  |> fst


let check_parameters_not_declared_twice errors procdesc =
  List.fold procdesc.ProcDesc.params ~init:(errors, VarName.Set.empty)
    ~f:(fun (errors, seen) vname ->
      let errors =
        if VarName.Set.mem vname seen then
          ParameterDeclatedTwice (procdesc.ProcDesc.procdecl.qualified_name, vname) :: errors
        else errors
      in
      (errors, VarName.Set.add vname seen) )
  |> fst


let check_global_not_declared_twice decls errors global =
  if is_global_declared decls global then GlobalDeclaredTwice global :: errors else errors


let check_struct_not_declared_twice decls errors struct_ =
  if is_struct_declared decls struct_ then StructDeclaredTwice struct_.Struct.name :: errors
  else errors


let check_nodes_not_implemented_twice errors procdesc =
  List.fold procdesc.ProcDesc.nodes ~init:(errors, NodeName.Set.empty)
    ~f:(fun (errors, seen) node ->
      let label = node.Node.label in
      let errors =
        if NodeName.Set.mem label seen then
          NodeImplementedTwice (procdesc.ProcDesc.procdecl.qualified_name, label) :: errors
        else errors
      in
      (errors, NodeName.Set.add label seen) )
  |> fst


let check_proc_not_implemented_twice decls errors procdecl =
  if is_proc_implemented decls procdecl then
    ProcImplementedTwice procdecl.ProcDecl.qualified_name :: errors
  else errors


let make_decls ({decls; sourcefile} : Module.t) : error list * t =
  let decls_env = init sourcefile in
  let register errors decl =
    match (decl : Module.decl) with
    | Global global ->
        let errors = check_global_not_declared_twice decls_env errors global in
        declare_global decls_env global ;
        errors
    | Struct struct_ ->
        let errors = check_struct_not_declared_twice decls_env errors struct_ in
        let errors = check_fieldnames_not_declared_twice errors struct_ in
        declare_struct decls_env struct_ ;
        errors
    | Procdecl procdecl ->
        declare_proc decls_env ~is_implemented:false procdecl ;
        errors
    | Proc pdesc ->
        let procdecl = pdesc.procdecl in
        let errors = check_proc_not_implemented_twice decls_env errors procdecl in
        let errors = check_parameters_not_declared_twice errors pdesc in
        let errors = check_nodes_not_implemented_twice errors pdesc in
        declare_proc decls_env ~is_implemented:true procdecl ;
        errors
  in
  let errors = List.fold decls ~init:[] ~f:register in
  (errors, decls_env)
