(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let pp_variable_name f name = Format.fprintf f "%s" name

let pp_pattern f (pattern : ToplAst.label_pattern) =
  match pattern with
  | ArrayWritePattern ->
      Format.fprintf f "#ArrayWrite"
  | ProcedureNamePattern procedure_name ->
      Format.fprintf f "%s" procedure_name


let pp_raw_label f {ToplAst.pattern; arguments; condition= _; action= _} =
  (* TODO: Print condition and action. *)
  Format.fprintf f "%a %a when CONDITION => ACTION" pp_pattern pattern
    (Pp.option (Pp.comma_seq pp_variable_name))
    arguments


let pp_label = Pp.option pp_raw_label
