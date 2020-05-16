(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
(* Module that define preanalysis to derive nullability annotations *)

open! IStd
module F = Format

module FieldsAssignedInConstructors = AbstractDomain.FiniteSet (struct
  type t = Fieldname.t * Typ.t [@@deriving compare]

  let pp fmt (fieldname, typ) =
    F.fprintf fmt "(%a, %a)" Fieldname.pp fieldname (Typ.pp_full Pp.text) typ
end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = FieldsAssignedInConstructors

  type analysis_data = Exp.t Ident.Hash.t

  let exp_is_null ids_map exp =
    match exp with
    | Exp.Var id -> (
      try
        let exp = Ident.Hash.find ids_map id in
        Exp.is_null_literal exp
      with Caml.Not_found -> false )
    | _ ->
        Exp.is_null_literal exp


  let is_self ids_map id =
    try match Ident.Hash.find ids_map id with Exp.Lvar var -> Pvar.is_self var | _ -> false
    with Caml.Not_found -> false


  let exec_instr astate id_table _ instr =
    match instr with
    | Sil.Load {id; e= exp} ->
        Ident.Hash.add id_table id exp ;
        astate
    | Sil.Store {e1= Exp.Lfield (Exp.Var lhs_id, name, typ); typ= exp_typ; e2= rhs} -> (
      match exp_typ.Typ.desc with
      (* block field of a ObjC class *)
      | Typ.Tptr ({desc= Tfun}, _)
        when Typ.is_objc_class typ && is_self id_table lhs_id
             && (* lhs is self, rhs is not null *)
             not (exp_is_null id_table rhs) ->
          FieldsAssignedInConstructors.add (name, typ) astate
      | _ ->
          astate )
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "nullability preanalysis"
end

(* Tracks when block variables of ObjC classes have been assigned to in constructors *)
module FieldsAssignedInConstructorsChecker =
  AbstractInterpreter.MakeRPO (TransferFunctions (ProcCfg.Normal))

let add_annot annot annot_name = ({Annot.class_name= annot_name; parameters= []}, true) :: annot

let add_nonnull_to_selected_field given_field ((fieldname, typ, annot) as field) =
  if Fieldname.equal fieldname given_field && not (Annotations.ia_is_nullable annot) then
    let new_annot = add_annot annot Annotations.nonnull in
    (fieldname, typ, new_annot)
  else field


let add_nonnull_to_fields fields tenv =
  let add_nonnull_to_field (field, typ) =
    match Typ.name typ with
    | Some typ_name -> (
      match Tenv.lookup tenv typ_name with
      | Some ({fields} as struct_typ) ->
          let fields_with_annot = List.map ~f:(add_nonnull_to_selected_field field) fields in
          ignore (Tenv.mk_struct tenv ~default:struct_typ ~fields:fields_with_annot typ_name)
      | None ->
          () )
    | None ->
        ()
  in
  FieldsAssignedInConstructors.iter add_nonnull_to_field fields


let analysis cfg tenv =
  let initial = FieldsAssignedInConstructors.empty in
  let f proc_name pdesc domain =
    if Procdesc.is_defined pdesc && Procname.is_constructor proc_name then
      match
        FieldsAssignedInConstructorsChecker.compute_post ~initial (Ident.Hash.create 10) pdesc
      with
      | Some new_domain ->
          FieldsAssignedInConstructors.union new_domain domain
      | None ->
          domain
    else domain
  in
  let fields_assigned_in_constructor = Procname.Hash.fold f cfg initial in
  add_nonnull_to_fields fields_assigned_in_constructor tenv
