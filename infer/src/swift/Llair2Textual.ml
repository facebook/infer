(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let to_textual_typ _llair_typ =
  (* TODO: translate types *)
  Textual.Typ.Void


let translate_llair_globals globals =
  let to_textual_global global =
    let global = global.GlobalDefn.name in
    let global_name = Global.name global in
    let name = Textual.VarName.of_string global_name in
    let typ = to_textual_typ (Global.typ global) in
    Textual.Global.{name; typ; attributes= []}
  in
  let globals = IArray.to_array globals |> Array.to_list in
  List.map ~f:to_textual_global globals


let translate sourcefile (llair_program : Llair.Program.t) : Textual.Module.t =
  let globals = translate_llair_globals llair_program.Llair.globals in
  let decls = List.map ~f:(fun global -> Textual.Module.Global global) globals in
  Textual.Module.{attrs= []; decls; sourcefile}
