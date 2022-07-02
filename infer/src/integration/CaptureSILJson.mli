(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Capture module for the json analysis in the capture phase *)

 open! IStd

 
 val capture :
      changed_files:SourceFile.Set.t option
   -> json_cfg:string
   -> json_tenv:string
   -> unit
 (** Run the capture of the files for which we have cfg in [json_cfg], type environment [json_tenv] in and
     [changed_files], if specified. *)