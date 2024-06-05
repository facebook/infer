(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let struct_copy tenv loc e1 e2 ~typ ~struct_name =
  let rec struct_copy_helper e1 e2 typ struct_name rev_acc =
    let {Struct.fields} = Option.value_exn (Tenv.lookup tenv struct_name) in
    List.fold fields ~init:rev_acc ~f:(fun rev_acc {Struct.name= field_name; typ= field_typ} ->
        let mk_field e = Exp.Lfield (e, field_name, typ) in
        let e1 = mk_field e1 in
        let e2 = mk_field e2 in
        match field_typ.Typ.desc with
        | Tstruct ((CStruct _ | CppClass _) as struct_name) ->
            struct_copy_helper e1 e2 field_typ struct_name rev_acc
        | _ ->
            let id = Ident.create_fresh Ident.knormal in
            Sil.Store {e1; typ= field_typ; e2= Exp.Var id; loc}
            :: Sil.Load {id; e= e2; typ= field_typ; loc}
            :: rev_acc )
  in
  if Exp.equal e1 e2 then [] else struct_copy_helper e1 e2 typ struct_name [] |> List.rev
