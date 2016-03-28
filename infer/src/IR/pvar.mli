(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Program variables. *)

module F = Format

(** Type for program variables. There are 4 kinds of variables:
    1) local variables, used for local variables and formal parameters
    2) callee program variables, used to handle recursion ([x | callee] is distinguished from [x])
    3) global variables
    4) seed variables, used to store the initial value of formal parameters
*)
type t

(** Compare two pvar's *)
val compare : t -> t -> int

(** Dump a program variable. *)
val d : t -> unit

(** Dump a list of program variables. *)
val d_list : t list -> unit

(** Equality for pvar's *)
val equal : t -> t -> bool

(** Get the name component of a program variable. *)
val get_name : t -> Mangled.t

(** [get_ret_pvar proc_name] retuns the return pvar associated with the procedure name *)
val get_ret_pvar : Procname.t -> t

(** Get a simplified version of the name component of a program variable. *)
val get_simplified_name : t -> string

(** Check if the pvar is an abducted return var or param passed by ref *)
val is_abducted : t -> bool

(** Check if the pvar is a callee var *)
val is_callee : t -> bool

(** Check if the pvar is a global var *)
val is_global : t -> bool

(** Check if the pvar is a local var *)
val is_local : t -> bool

(** Check if the pvar is a seed var *)
val is_seed : t -> bool

(** Check if the pvar is a return var *)
val is_return : t -> bool

(** Check if a pvar is the special "this" var *)
val is_this : t -> bool

(** [mk name proc_name suffix] creates a program var with the given function name and suffix *)
val mk : Mangled.t -> Procname.t -> t

(** create an abducted variable for a parameter passed by reference *)
val mk_abducted_ref_param : Procname.t -> t -> Location.t -> t

(** create an abducted return variable for a call to [proc_name] at [loc] *)
val mk_abducted_ret : Procname.t -> Location.t -> t

(** [mk_callee name proc_name] creates a program var
    for a callee function with the given function name *)
val mk_callee : Mangled.t -> Procname.t -> t

(** create a global variable with the given name *)
val mk_global : Mangled.t -> t

(** Pretty print a program variable. *)
val pp : printenv -> F.formatter -> t -> unit

(** Pretty print a list of program variables. *)
val pp_list : printenv -> F.formatter -> t list -> unit

(** Pretty print a pvar which denotes a value, not an address *)
val pp_value : printenv -> F.formatter -> t -> unit

(** Turn an ordinary program variable into a callee program variable *)
val to_callee : Procname.t -> t -> t

(** Turn a pvar into a seed pvar (which stores the initial value of a stack var) *)
val to_seed : t -> t

(** Convert a pvar to string. *)
val to_string : t -> string
