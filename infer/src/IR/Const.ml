(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** The Smallfoot Intermediate Language: Constants *)

open! IStd
module F = Format

type t =
  | Cint of IntLit.t  (** integer constants *)
  | Cfun of Typ.Procname.t  (** function names *)
  | Cstr of string  (** string constants *)
  | Cfloat of float  (** float constants *)
  | Cclass of Ident.name  (** class constant *)
[@@deriving compare]

let equal = [%compare.equal : t]

let kind_equal c1 c2 =
  let const_kind_number = function
    | Cint _ ->
        1
    | Cfun _ ->
        2
    | Cstr _ ->
        3
    | Cfloat _ ->
        4
    | Cclass _ ->
        5
  in
  Int.equal (const_kind_number c1) (const_kind_number c2)


let pp pe f = function
  | Cint i ->
      IntLit.pp f i
  | Cfun fn -> (
    match pe.Pp.kind with
    | HTML ->
        F.fprintf f "_fun_%s" (Escape.escape_xml (Typ.Procname.to_string fn))
    | _ ->
        F.fprintf f "_fun_%s" (Typ.Procname.to_string fn) )
  | Cstr s ->
      F.fprintf f "\"%s\"" (String.escaped s)
  | Cfloat v ->
      F.fprintf f "%f" v
  | Cclass c ->
      F.fprintf f "%a" Ident.pp_name c


let to_string c = F.asprintf "%a" (pp Pp.text) c

let iszero_int_float = function Cint i -> IntLit.iszero i | Cfloat 0.0 -> true | _ -> false

let isone_int_float = function Cint i -> IntLit.isone i | Cfloat 1.0 -> true | _ -> false

let isminusone_int_float = function
  | Cint i ->
      IntLit.isminusone i
  | Cfloat -1.0 ->
      true
  | _ ->
      false
