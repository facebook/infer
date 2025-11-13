(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Constants *)

open! IStd
module F = Format

type t =
  | Cint of IntLit.t  (** integer constants *)
  | Cfun of Procname.t  (** function names *)
  | Cstr of string  (** string constants *)
  | Cfloat of float  (** float constants *)
  | Cclass of Ident.name  (** class constant *)
[@@deriving compare, equal, hash, normalize, yojson_of]

let pp pe f = function
  | Cint i ->
      IntLit.pp f i
  | Cfun fn -> (
    match pe.Pp.kind with
    | HTML ->
        F.fprintf f "_fun_%s" (Escape.escape_xml (F.asprintf "%a" Procname.pp fn))
    | _ ->
        F.fprintf f "_fun_%a" Procname.pp fn )
  | Cstr s ->
      F.fprintf f "\"%s\"" (String.escaped s)
  | Cfloat v ->
      F.pp_print_float f v
  | Cclass c ->
      Ident.pp_name f c


let iszero_int_float = function Cint i -> IntLit.iszero i | Cfloat 0.0 -> true | _ -> false

let isone_int_float = function Cint i -> IntLit.isone i | Cfloat 1.0 -> true | _ -> false

let isminusone_int_float = function
  | Cint i ->
      IntLit.isminusone i
  | Cfloat -1.0 ->
      true
  | _ ->
      false
