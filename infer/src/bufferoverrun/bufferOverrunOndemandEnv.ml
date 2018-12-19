(*
 * Copyright (c) 2018-present, Facebook, Inc.
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
      | SPath.Deref (_, x) ->
          Option.map (typ_of_param_path x) ~f:(fun typ ->
              match typ.Typ.desc with
              | Tptr (typ, _) ->
                  typ
              | Tarray {elt} ->
                  elt
              | Tvoid ->
                  Typ.void
              | Tstruct typename -> (
                match BufferOverrunTypModels.dispatch tenv typename with
                | Some (CArray {element_typ}) ->
                    element_typ
                | Some _ ->
                    L.(die InternalError)
                      "Deref of non-array modeled type `%a`" Typ.Name.pp typename
                | None ->
                    L.(die InternalError) "Deref of unmodeled type `%a`" Typ.Name.pp typename )
              | _ ->
                  L.(die InternalError) "Untyped expression is given." )
      | SPath.Field (fn, x) -> (
        match BufferOverrunField.get_type fn with
        | None ->
            let lookup = Tenv.lookup tenv in
            Option.map (typ_of_param_path x) ~f:(Typ.Struct.fld_typ ~lookup ~default:Typ.void fn)
        | some_typ ->
            some_typ )
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
      | SPath.Field (fn, x) ->
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


let canonical_path typ_of_param_path path =
  let module K = struct
    type t =
      | Default of {struct_typ: Typ.t option; fn: Typ.Fieldname.t}
      | FldTypKnownPtr of {fld_typ: Typ.t}
    [@@deriving compare]
  end in
  let module KnownFields = Caml.Map.Make (K) in
  let rec helper path =
    match path with
    | SPath.Pvar _ | SPath.Callsite _ ->
        (None, KnownFields.empty)
    | SPath.Deref (deref_kind, ptr) ->
        let ptr_opt, known_fields = helper ptr in
        (Option.map ptr_opt ~f:(fun ptr -> SPath.deref ~deref_kind ptr), known_fields)
    | SPath.Field (fn, struct_path) -> (
        let struct_path_opt, known_fields = helper struct_path in
        let key =
          match typ_of_param_path path with
          | Some fld_typ when Typ.is_pointer fld_typ ->
              K.FldTypKnownPtr {fld_typ}
          | _ ->
              let struct_typ =
                typ_of_param_path (Option.value ~default:struct_path struct_path_opt)
              in
              K.Default {struct_typ; fn}
        in
        match KnownFields.find_opt key known_fields with
        | Some _ as canonicalized ->
            (canonicalized, known_fields)
        | None ->
            let field_path =
              Option.value_map struct_path_opt ~default:path ~f:(fun struct_path ->
                  SPath.field struct_path fn )
            in
            (None, KnownFields.add key field_path known_fields) )
  in
  Option.value (helper path |> fst) ~default:path
