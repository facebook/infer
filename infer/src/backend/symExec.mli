(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Symbolic Execution *)

(** print the builtin functions and exit *)
val print_builtins : unit -> unit

(** Check if the function is a builtin *)
val function_is_builtin : Procname.t -> bool

(** symbolic execution on the level of sets of propositions *)
val lifted_sym_exec : (exn -> unit) -> Cfg.cfg -> Sil.tenv -> Cfg.Procdesc.t ->
  Paths.PathSet.t -> Cfg.Node.t -> Sil.instr list -> Paths.PathSet.t

(** OO method resolution: given a class name and a method name, climb the class hierarchy to find
 * the procname that the method name will actually resolve to at runtime. For example, if we have
 * a procname like Foo.toString() and Foo does not override toString(), we must resolve the call to
 * toString(). We will end up with Super.toString() where Super is some superclass of Foo. *)
val resolve_method : Sil.tenv -> Typename.t -> Procname.t -> Procname.t
(** {2 Functions for handling builtins } *)

module ModelBuiltins : sig
  val __assert_fail : Procname.t
  val __delete : Procname.t
  val __delete_array : Procname.t
  val __exit : Procname.t
  val __get_array_size : Procname.t
  val __get_type_of : Procname.t
  val __infer_fail : Procname.t
  val __instanceof : Procname.t (** [__instanceof(val,typ)] implements java's [val instanceof typ] *)
  val __cast : Procname.t (** [__cast(val,typ)] implements java's [typ(val)] *)
  val __placement_delete : Procname.t
  val __placement_new : Procname.t
  val __new : Procname.t
  val __new_array : Procname.t
  val __objc_alloc : Procname.t
  val __objc_alloc_no_fail : Procname.t
  val __set_array_size : Procname.t
  val __unwrap_exception : Procname.t
  val __set_file_attribute : Procname.t
  val __set_mem_attribute : Procname.t
  val __infer_assume : Procname.t
  val __objc_retain : Procname.t
  val __objc_release : Procname.t
  val __objc_retain_cf : Procname.t
  val __objc_release_cf : Procname.t
  val __set_autorelease_attribute : Procname.t
  val __objc_release_autorelease_pool : Procname.t
  val __objc_cast : Procname.t
  val __objc_dictionary_literal : Procname.t
  val malloc_no_fail : Procname.t
end
