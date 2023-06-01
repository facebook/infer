(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let proc_desc_ref = ref None

let () = AnalysisGlobalState.register_ref_with_proc_desc proc_desc_ref ~init:Option.some

let proc_desc () = Option.value_exn !proc_desc_ref
