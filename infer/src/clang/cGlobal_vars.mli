(*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

type t 

val add : string -> Sil.typ -> unit
  
val find : Mangled.t -> t

val reset_map : unit -> unit
  
val var_get_name : t -> Sil.pvar
  
val var_get_typ : t -> Sil.typ

val print_map : unit -> unit
