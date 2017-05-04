(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(* Module that define preanalysis to derive nullability annotations *)

open! IStd

module F = Format
module L = Logging

type t = Fieldname.t * Typ.t [@@deriving compare]

let pp fmt (fieldname, typ) =
  F.fprintf fmt "(%a, %a)" Fieldname.pp fieldname (Typ.pp_full Pp.text) typ

module DomainSet =
  PrettyPrintable.MakePPSet(struct
    type nonrec t = t
    let compare = compare
    let pp = pp
  end)

module FieldsAssignedInConstructors = AbstractDomain.FiniteSet(DomainSet)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = FieldsAssignedInConstructors
  type extras = Exp.t Ident.IdentHash.t

  let exp_is_null ids_map exp =
    match exp with
    | Exp.Var id ->
        (try
           let exp = Ident.IdentHash.find ids_map id in
           Exp.is_null_literal exp
         with Not_found -> false)
    | _ -> Exp.is_null_literal exp

  let is_self ids_map id =
    try
      match Ident.IdentHash.find ids_map id with
      | Exp.Lvar var -> Pvar.is_self var
      | _ -> false
    with Not_found -> false

  let exec_instr astate (proc_data: Exp.t Ident.IdentHash.t ProcData.t) _ instr =
    match instr with
    | Sil.Load (id, exp, _, _) ->
        Ident.IdentHash.add proc_data.extras id exp;
        astate
    | Sil.Store (Exp.Lfield (Exp.Var lhs_id, name, typ), exp_typ, rhs, _) ->
        (match exp_typ.Typ.desc with (* block field of a ObjC class *)
         | Typ.Tptr ({desc=Tfun _}, _)
           when Typ.is_objc_class typ &&
                is_self proc_data.extras lhs_id && (* lhs is self, rhs is not null *)
                not (exp_is_null proc_data.extras rhs) ->
             FieldsAssignedInConstructors.add (name, typ) astate
         | _ -> astate)
    | _ -> astate
end

(* Tracks when block variables of ObjC classes have been assigned to in constructors *)
module FieldsAssignedInConstructorsChecker =
  AbstractInterpreter.Make (ProcCfg.Normal) (TransferFunctions)

module AnalysisCfg = ProcCfg.Normal

let add_annot annot annot_name =
  ({ Annot.class_name = annot_name; parameters = []; }, true) :: annot

let add_nonnull_to_selected_field given_field ((fieldname, typ, annot) as field) =
  if Fieldname.equal fieldname given_field &&
     not (Annotations.ia_is_nullable annot) then
    let new_annot = add_annot annot Annotations.nonnull in
    (fieldname, typ, new_annot)
  else field

let add_nonnull_to_fields fields tenv =
  let add_nonnull_to_field (field, typ) =
    match Typ.name typ with
    | Some typ_name ->
        (match Tenv.lookup tenv typ_name with
         | Some { fields; statics; supers; methods; annots} ->
             let fields_with_annot =
               List.map ~f:(add_nonnull_to_selected_field field) fields in
             ignore(
               Tenv.mk_struct tenv
                 ~fields: fields_with_annot ~statics ~supers ~methods ~annots typ_name)
         | None -> ())
    | None -> () in
  DomainSet.iter add_nonnull_to_field fields

let analysis cfg tenv =
  let initial = FieldsAssignedInConstructors.empty in
  let f domain pdesc =
    let proc_name = Procdesc.get_proc_name pdesc in
    if Typ.Procname.is_constructor proc_name then
      match FieldsAssignedInConstructorsChecker.compute_post
              (ProcData.make pdesc tenv (Ident.IdentHash.create 10)) ~initial with
      | Some new_domain ->
          FieldsAssignedInConstructors.union new_domain domain
      | None -> domain
    else domain in
  let procs = Cfg.get_defined_procs cfg in
  let fields_assigned_in_constructor = List.fold ~f ~init:initial procs in
  add_nonnull_to_fields fields_assigned_in_constructor tenv
