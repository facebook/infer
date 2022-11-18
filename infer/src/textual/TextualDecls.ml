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
  ; procnames: ProcDecl.t QualifiedNameHashtbl.t
  ; structs: Struct.t TypeName.Hashtbl.t
  ; sourcefile: SourceFile.t }

let init sourcefile =
  { globals= VarName.Hashtbl.create 17
  ; procnames= QualifiedNameHashtbl.create 17
  ; structs= TypeName.Hashtbl.create 17
  ; sourcefile }


let declare_global decls (global : Global.t) =
  VarName.Hashtbl.replace decls.globals global.name global |> ignore


let declare_proc decls (pname : ProcDecl.t) =
  QualifiedNameHashtbl.replace decls.procnames pname.qualified_name pname |> ignore


let declare_struct decls (s : Struct.t) = TypeName.Hashtbl.replace decls.structs s.name s |> ignore

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
  let* strct = TypeName.Hashtbl.find_opt decls.structs enclosing_class in
  List.find strct.Struct.fields ~f:(fun ({qualified_name} : FieldDecl.t) ->
      FieldName.equal qualified_name.name name )


let get_procname decls qualified_name = QualifiedNameHashtbl.find_opt decls.procnames qualified_name

let get_struct decls tname = TypeName.Hashtbl.find_opt decls.structs tname

let fold_globals decls ~init ~f =
  VarName.Hashtbl.fold (fun key data x -> f x key data) decls.globals init


let fold_procnames decls ~init ~f =
  QualifiedNameHashtbl.fold (fun _ procname x -> f x procname) decls.procnames init


let fold_structs decls ~init ~f =
  TypeName.Hashtbl.fold (fun key data x -> f x key data) decls.structs init


let source_file {sourcefile; _} = sourcefile

let make_decls ({decls; sourcefile} : Module.t) =
  let decls_env = init sourcefile in
  let register decl =
    match (decl : Module.decl) with
    | Global pvar ->
        declare_global decls_env pvar
    | Struct strct ->
        declare_struct decls_env strct
    | Procdecl procdecl ->
        declare_proc decls_env procdecl
    | Proc pdesc ->
        declare_proc decls_env pdesc.procdecl
  in
  List.iter decls ~f:register ;
  decls_env
