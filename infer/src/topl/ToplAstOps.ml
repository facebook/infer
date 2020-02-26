(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let pp_raw_label f {ToplAst.procedure_name} = Format.fprintf f "%s" procedure_name

let pp_label = Pp.option pp_raw_label
