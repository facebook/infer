(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Type of a Datalog fact. In all facts, "proc_name" is the procedure inside of which the fact is
    emitted. The allocation and call sites are uniquely identified by the string
    "class:line:assigned_variable" (note that in SIL there is always an assigned variable, even for
    procedures with a void return type). The assigned variable is used instead of the column because
    the Java frontend does not provide information about columns, and variables have unique names. *)
type t =
  | Reachable of {proc_name: Procname.t}
  | Extends of {typ: Typ.Name.t; typ_super: Typ.Name.t}
  | Cast of {proc_name: Procname.t; dest: Ident.t; src: Ident.t; dest_typ: Typ.t}
  (* return_var = new typ(). The allocation site is "class:line:return". *)
  | Alloc of {proc_name: Procname.t; return: Ident.t; allocation_site: string; typ: Typ.t}
  (* receiver.call_proc(). The call site is "class:line:assigned_variable".
     proc_signature is the method signature without the class. *)
  | VirtualCall of
      {proc_name: Procname.t; call_site: string; receiver: Ident.t; proc_signature: string}
  (* The call site is "class:line:assigned_variable". *)
  | StaticCall of {proc_name: Procname.t; call_site: string; call_proc: Procname.t}
  (* A call at call_site with actual argument arg. n_arg is the position of the argument. *)
  | ActualArg of {proc_name: Procname.t; call_site: string; n_arg: int; arg: Ident.t}
  (* A procedure with formal argument arg. n_arg is the position of the argument. *)
  | FormalArg of {proc_name: Procname.t; n_arg: int; arg: Ident.t}
  (* return_var = call(). If void, then no fact is generated. *)
  | ActualReturn of {proc_name: Procname.t; call_site: string; return: Ident.t}
  (* proc_name() {return return_var}. Emitted for every "return" statement. *)
  | FormalReturn of {proc_name: Procname.t; return: Ident.t}
  (* Class typ implements method proc_signature.
     proc_signature is the method signature without the class. *)
  | Implem of {typ: Typ.Name.t; proc_signature: string}

val to_string : t -> string

val iter_fact_types : (string -> unit) -> unit

val reachable : Procname.t -> t

val extends : Typ.Name.t -> Typ.Name.t -> t

val cast : Procname.t -> Ident.t -> Ident.t -> Typ.t -> t

val alloc : Procname.t -> Ident.t -> Location.t -> Typ.t -> t

val virtual_call : Procname.t -> Location.t -> Ident.t -> Procname.t -> Ident.t -> t

val static_call : Procname.t -> Location.t -> Ident.t -> Procname.t -> t

val actual_arg : Procname.t -> Location.t -> Ident.t -> int -> Ident.t -> t

val formal_arg : Procname.t -> int -> Ident.t -> t

val actual_return : Procname.t -> Location.t -> Ident.t -> t

val formal_return : Procname.t -> Ident.t -> t

val implem : Typ.Name.t -> Procname.t -> t
