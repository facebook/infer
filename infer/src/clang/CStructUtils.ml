(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let unrolled_array_length (typ : Typ.t) =
  let limit = IntLit.of_int Config.clang_compound_literal_init_limit in
  (* a zero-length dimension makes the total number of elements zero: check every dimension *)
  let rec num_elements (typ : Typ.t) =
    match typ.desc with
    | Tarray {elt; length= Some length} when IntLit.leq length limit ->
        Option.map (num_elements elt) ~f:(IntLit.mul length)
    | Tarray _ ->
        None
    | _ ->
        Some IntLit.one
  in
  match (typ.desc, num_elements typ) with
  | Tarray {length= Some length}, Some n when IntLit.leq n limit ->
      Some (IntLit.to_int_exn length)
  | _ ->
      None


(* nested structs and arrays of structs multiply the number of copies: bound the size of the
   unrolled copy *)
let max_scalar_copies = 16

(** the number of scalar copies of an unrolled copy of [typ], at most [max_scalar_copies + 1] *)
let rec count_scalar_copies tenv (typ : Typ.t) =
  let saturate n = Int.min n (max_scalar_copies + 1) in
  match (typ.desc, unrolled_array_length typ) with
  | Tstruct ((CStruct _ | CppClass _) as struct_name), _ -> (
    match Tenv.lookup tenv struct_name with
    | Some {Struct.fields} ->
        List.fold fields ~init:0 ~f:(fun n (field : Struct.field) ->
            saturate (n + count_scalar_copies tenv field.typ) )
    | None ->
        1 )
  | Tarray {elt}, Some length ->
      saturate (length * count_scalar_copies tenv elt)
  | _ ->
      1


let is_copy_unrolled tenv typ = count_scalar_copies tenv typ <= max_scalar_copies

(** the copies of the array members of a class add up in its copy constructors and assignment
    operators: they are unrolled only if they copy at most [max_scalar_copies] values together *)
let are_array_members_unrolled tenv class_name =
  match Tenv.lookup tenv class_name with
  | Some {Struct.fields} ->
      List.fold_until fields ~init:0
        ~finish:(fun _ -> true)
        ~f:(fun n ({typ} : Struct.field) ->
          let n =
            if Option.is_some (unrolled_array_length typ) then n + count_scalar_copies tenv typ
            else n
          in
          if n > max_scalar_copies then Stop false else Continue n )
  | None ->
      true


let rec member_class_name (exp : Exp.t) =
  match exp with
  | Lindex (exp, _) ->
      member_class_name exp
  | Lfield (_, field_name, _) ->
      Some (Fieldname.get_class_name field_name)
  | _ ->
      None


let unrolled_copy_length tenv ?dest typ =
  Option.filter (unrolled_array_length typ) ~f:(fun _ ->
      is_copy_unrolled tenv typ
      && Option.for_all (Option.bind dest ~f:member_class_name) ~f:(are_array_members_unrolled tenv) )


let rec copy tenv ~unroll loc e1 e2 (typ : Typ.t) rev_acc =
  let copy_whole rev_acc =
    let id = Ident.create_fresh Ident.knormal in
    Sil.Store {e1; typ; e2= Exp.Var id; loc} :: Sil.Load {id; e= e2; typ; loc} :: rev_acc
  in
  match (typ.desc, unrolled_array_length typ) with
  | Tstruct ((CStruct _ | CppClass _) as struct_name), _ ->
      copy_fields tenv ~unroll loc e1 e2 typ struct_name rev_acc
  | Tarray {elt}, Some length when unroll ->
      (* copying an array as a whole does not copy its elements, which are separate from it, but
         some analyses, such as bufferoverrun, model arrays as a whole *)
      List.fold (List.init length ~f:Fn.id) ~init:(copy_whole rev_acc) ~f:(fun rev_acc i ->
          let mk_index e = Exp.Lindex (e, Exp.int (IntLit.of_int i)) in
          copy tenv ~unroll loc (mk_index e1) (mk_index e2) elt rev_acc )
  | _ ->
      copy_whole rev_acc


and copy_fields tenv ~unroll loc e1 e2 typ struct_name rev_acc =
  let {Struct.fields} = Option.value_exn (Tenv.lookup tenv struct_name) in
  List.fold fields ~init:rev_acc ~f:(fun rev_acc {Struct.name= field_name; typ= field_typ} ->
      let mk_field e = Exp.Lfield ({exp= e; is_implicit= false}, field_name, typ) in
      copy tenv ~unroll loc (mk_field e1) (mk_field e2) field_typ rev_acc )


let struct_copy tenv loc e1 e2 ~typ ~struct_name =
  if Exp.equal e1 e2 then []
  else copy_fields tenv ~unroll:(is_copy_unrolled tenv typ) loc e1 e2 typ struct_name [] |> List.rev


let array_copy tenv loc e1 e2 ~typ =
  if Exp.equal e1 e2 then []
  else copy tenv ~unroll:(is_copy_unrolled tenv typ) loc e1 e2 typ [] |> List.rev
