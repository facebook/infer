(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

type t 

val add : string -> Sil.typ -> unit
  
val find : Mangled.t -> t

val reset_map : unit -> unit
  
val var_get_name : t -> Sil.pvar
  
val var_get_typ : t -> Sil.typ

val print_map : unit -> unit
