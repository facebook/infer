(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module SPath = Symb.SymbolPath
module FormalTyps = Caml.Map.Make (Pvar)

type t =
  { tenv: Tenv.t
  ; typ_of_param_path: SPath.partial -> Typ.t option
  ; may_last_field: SPath.partial -> bool
  ; entry_location: Location.t
  ; integer_type_widths: Typ.IntegerWidths.t }

let mk pdesc =
  let formal_typs =
    List.fold (Procdesc.get_pvar_formals pdesc) ~init:FormalTyps.empty ~f:(fun acc (formal, typ) ->
        FormalTyps.add formal typ acc )
  in
  fun tenv integer_type_widths ->
    let rec typ_of_param_path = function
      | SPath.Pvar x ->
          FormalTyps.find_opt x formal_typs
      | SPath.Deref (_, x) -> (
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
              Some Typ.void
          | Tstruct typename -> (
            match BufferOverrunTypModels.dispatch tenv typename with
            | Some (CArray {element_typ}) ->
                Some element_typ
            | Some CppStdVector ->
                Some (Typ.mk (Typ.Tptr (Typ.void, Typ.Pk_pointer)))
            | Some _ ->
                L.internal_error "Deref of non-array modeled type `%a`" Typ.Name.pp typename ;
                None
            | None ->
                L.(die InternalError) "Deref of unmodeled type `%a`" Typ.Name.pp typename )
          | _ ->
              L.(die InternalError) "Untyped expression is given." ) )
      | SPath.Field {typ= Some _ as some_typ} ->
          some_typ
      | SPath.Field {fn; prefix= x} -> (
        match BufferOverrunField.get_type fn with
        | None ->
            let lookup = Tenv.lookup tenv in
            Option.map (typ_of_param_path x) ~f:(Typ.Struct.fld_typ ~lookup ~default:Typ.void fn)
        | some_typ ->
            some_typ )
      | SPath.StarField {last_field} ->
          BufferOverrunField.get_type last_field
      | SPath.Callsite {ret_typ} ->
          Some ret_typ
    in
    let is_last_field fn (fields : Typ.Struct.field list) =
      Option.value_map (List.last fields) ~default:false ~f:(fun (last_fn, _, _) ->
          Typ.Fieldname.equal fn last_fn )
    in
    let rec may_last_field = function
      | SPath.Pvar _ | SPath.Deref _ | SPath.Callsite _ ->
          true
      | SPath.Field {fn; prefix= x} | SPath.StarField {last_field= fn; prefix= x} ->
          may_last_field x
          && Option.value_map ~default:true (typ_of_param_path x) ~f:(fun parent_typ ->
                 match parent_typ.Typ.desc with
                 | Tstruct typename ->
                     let opt_struct = Tenv.lookup tenv typename in
                     Option.value_map opt_struct ~default:false ~f:(fun str ->
                         is_last_field fn str.Typ.Struct.fields )
                 | _ ->
                     true )
    in
    let entry_location = Procdesc.Node.get_loc (Procdesc.get_start_node pdesc) in
    {tenv; typ_of_param_path; may_last_field; entry_location; integer_type_widths}
