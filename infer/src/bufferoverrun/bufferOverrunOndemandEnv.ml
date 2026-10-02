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
module FormalTyps = Stdlib.Map.Make (Pvar)

type t =
  { tenv: Tenv.t
  ; typ_of_param_path: SPath.partial -> Typ.t option
  ; typ_of_global_array: Pvar.t -> Typ.t option
  ; may_last_field: SPath.partial -> bool
  ; entry_location: Location.t
  ; integer_type_widths: IntegerWidths.t
  ; class_name: Typ.name option }

let rec strip_array (typ : Typ.t) = match typ.desc with Tarray {elt} -> strip_array elt | _ -> typ

let mk pdesc =
  let pname = Procdesc.get_proc_name pdesc in
  let formal_typs =
    List.fold (Procdesc.get_pvar_formals pdesc) ~init:FormalTyps.empty ~f:(fun acc (formal, typ) ->
        FormalTyps.add formal typ acc )
    |> fun init ->
    List.fold (Procdesc.get_captured pdesc) ~init ~f:(fun acc {CapturedVar.pvar; typ} ->
        FormalTyps.add pvar typ acc )
  in
  let global_typs =
    (* a variable recorded with several types, e.g. same-named static locals of different scopes,
       is left untyped *)
    List.fold (Procdesc.get_globals pdesc) ~init:Pvar.Map.empty ~f:(fun acc (pvar, typ) ->
        Pvar.Map.update pvar (function None -> Some (Some typ) | Some _ -> Some None) acc )
    |> Pvar.Map.filter_map (fun _ typ_opt -> typ_opt)
  in
  fun tenv integer_type_widths ->
    let c_array_model_element_typ (typ : Typ.t) =
      match typ.desc with
      | Tstruct typename -> (
        match BufferOverrunTypModels.dispatch tenv typename with
        | Some (CArray {element_typ}) ->
            Some element_typ
        | Some (CppStdVector | JavaCollection | JavaInteger) | None ->
            None )
      | _ ->
          None
    in
    let global_array_typs =
      Pvar.Map.filter
        (fun _ (typ : Typ.t) ->
          match typ.desc with
          | Tarray _ ->
              true
          | _ ->
              (* the [std::array] model checks the indices of inner arrays against the outer array *)
              Option.exists (c_array_model_element_typ typ) ~f:(fun (elt : Typ.t) ->
                  match elt.desc with
                  | Tarray _ ->
                      false
                  | _ ->
                      Option.is_none (c_array_model_element_typ elt) ) )
        global_typs
    in
    let typ_of_global_array pvar = Pvar.Map.find_opt pvar global_array_typs in
    (* The declared type of a global [g] does not type the path [g], which is also the path of the
       elements allocated by the initializer of [g] (see [BufferOverrunUtils.Exec.decl_local_array]),
       only its fields and, if [g] is an array, its elements. Other dereferences of [g], e.g. of a
       struct read through a pointer cast, come from the type of a load and are typed by it. *)
    let rec typ_of_deref_prefix_path deref_kind path =
      match (deref_kind, path) with
      | SPath.Deref_ArrayIndex, BoField.Prim (SPath.Pvar x) when Pvar.is_global x ->
          typ_of_global_array x
      | _ ->
          typ_of_param_path path
    and typ_of_field_prefix_path path =
      let typ =
        match path with
        | BoField.Prim (SPath.Pvar x) when Pvar.is_global x ->
            Pvar.Map.find_opt x global_typs
        | _ ->
            typ_of_param_path path
      in
      (* a field of an array is a field of its elements, which can have the path of the array *)
      Option.map typ ~f:strip_array
    and typ_of_param_path = function
      | BoField.Prim (SPath.Pvar x) ->
          FormalTyps.find_opt x formal_typs
      | BoField.Prim (SPath.Deref (deref_kind, x)) -> (
        match typ_of_deref_prefix_path deref_kind x with
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
                L.internal_error "Deref of non-array modeled type `%a`@\n" Typ.Name.pp typename ;
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
            Option.bind (typ_of_field_prefix_path x)
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
      Option.exists (List.last fields) ~f:(fun ({Struct.name= last_fn} : Struct.field) ->
          Fieldname.equal fn last_fn )
    in
    let rec may_last_field = function
      | BoField.Prim (SPath.Pvar _ | SPath.Deref _ | SPath.Callsite _) ->
          true
      | BoField.(Field {fn; prefix= x} | StarField {last_field= fn; prefix= x}) ->
          may_last_field x
          && Option.value_map ~default:true (typ_of_field_prefix_path x) ~f:(fun parent_typ ->
              match parent_typ.Typ.desc with
              | Tstruct typename ->
                  let opt_struct = Tenv.lookup tenv typename in
                  Option.exists opt_struct ~f:(fun str -> is_last_field fn str.Struct.fields)
              | _ ->
                  true )
    in
    let entry_location = Procdesc.Node.get_loc (Procdesc.get_start_node pdesc) in
    let class_name = Procname.get_class_type_name pname in
    { tenv
    ; typ_of_param_path
    ; typ_of_global_array
    ; may_last_field
    ; entry_location
    ; integer_type_widths
    ; class_name }
