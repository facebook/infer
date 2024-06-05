(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module BoField = BufferOverrunField
module SPath = Symb.SymbolPath
module FormalTyps = Caml.Map.Make (Pvar)

type t =
  { tenv: Tenv.t
  ; typ_of_param_path: SPath.partial -> Typ.t option
  ; may_last_field: SPath.partial -> bool
  ; entry_location: Location.t
  ; integer_type_widths: IntegerWidths.t
  ; class_name: Typ.name option }

let mk pdesc =
  let pname = Procdesc.get_proc_name pdesc in
  let formal_typs =
    List.fold (Procdesc.get_pvar_formals pdesc) ~init:FormalTyps.empty ~f:(fun acc (formal, typ) ->
        FormalTyps.add formal typ acc )
    |> fun init ->
    List.fold (Procdesc.get_captured pdesc) ~init ~f:(fun acc {CapturedVar.pvar; typ} ->
        FormalTyps.add pvar typ acc )
  in
  fun tenv integer_type_widths ->
    let rec typ_of_param_path = function
      | BoField.Prim (SPath.Pvar x) ->
          FormalTyps.find_opt x formal_typs
      | BoField.Prim (SPath.Deref (_, x)) -> (
        match typ_of_param_path x with
        | None ->
            None
        | Some typ -> (
          match typ.Typ.desc with
          | Tptr (typ, _) ->
              Some typ
          | Tarray {elt} ->
              Some elt
          | Tvoid ->
              Some StdTyp.void
          | Tstruct typename -> (
            match BufferOverrunTypModels.dispatch tenv typename with
            | Some (CArray {element_typ}) ->
                Some element_typ
            | Some CppStdVector ->
                Some (Typ.mk (Typ.Tptr (StdTyp.void, Typ.Pk_pointer)))
            | Some JavaCollection ->
                (* Current Java frontend does give element types of Java collection. *)
                None
            | Some JavaInteger ->
                L.internal_error "Deref of non-array modeled type `%a`" Typ.Name.pp typename ;
                None
            | None ->
                L.(die InternalError) "Deref of unmodeled type `%a`" Typ.Name.pp typename )
          | _ ->
              L.(die InternalError) "Untyped expression is given." ) )
      | BoField.Field {typ= Some _ as some_typ} ->
          some_typ
      | BoField.Field {fn; prefix= x} | BoField.StarField {last_field= fn; prefix= x} -> (
        match BoField.get_type fn with
        | None ->
            let lookup = Tenv.lookup tenv in
            Option.bind (typ_of_param_path x)
              ~f:
                ( if Config.bo_assume_void then fun t ->
                    Some (Struct.fld_typ ~lookup ~default:StdTyp.void fn t)
                  else Struct.fld_typ_opt ~lookup fn )
        | some_typ ->
            some_typ )
      | BoField.Prim (SPath.Callsite {ret_typ}) ->
          Some ret_typ
    in
    let is_last_field fn (fields : Struct.field list) =
      Option.exists (List.last fields) ~f:(fun {Struct.name= last_fn} -> Fieldname.equal fn last_fn)
    in
    let rec may_last_field = function
      | BoField.Prim (SPath.Pvar _ | SPath.Deref _ | SPath.Callsite _) ->
          true
      | BoField.(Field {fn; prefix= x} | StarField {last_field= fn; prefix= x}) ->
          may_last_field x
          && Option.value_map ~default:true (typ_of_param_path x) ~f:(fun parent_typ ->
                 match parent_typ.Typ.desc with
                 | Tstruct typename ->
                     let opt_struct = Tenv.lookup tenv typename in
                     Option.exists opt_struct ~f:(fun str -> is_last_field fn str.Struct.fields)
                 | _ ->
                     true )
    in
    let entry_location = Procdesc.Node.get_loc (Procdesc.get_start_node pdesc) in
    let class_name = Procname.get_class_type_name pname in
    {tenv; typ_of_param_path; may_last_field; entry_location; integer_type_widths; class_name}
