(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t

(** [mk qs prog args] finds the type of command depending on its arguments [args]. The quoting style
    of the arguments have to be provided, so that the command may be run later on. Beware that this
    doesn't look inside argument files. This can be used to create a "clang -### ..." command on
    which to call [command_to_run], but other functions from the module will not work as expected
    unless the command has been normalized by "clang -### ...". *)

val mk : ClangQuotes.style -> prog:string -> args:string list -> t

(** Make a command into a string ready to be passed to a shell to be executed. Fine to call with
    clang driver commands. *)

val command_to_run : t -> string

(** Whether the command has this flag set in its arguments. Must be called on normalized commands. *)

val has_flag : t -> string -> bool

(** The value passed to an option in the arguments of a command. Must be called on normalized commands. *)

val value_of_option : t -> string -> string option

(** Whether the command is suitable for attaching the AST exporter. Must be called on normalized commands. *)

val can_attach_ast_exporter : t -> bool

(** Add the arguments needed to attach the facebook-clang-plugins plugin. Must be called on normalized commands. *)

val with_plugin_args : t -> t

val prepend_arg : string -> t -> t

val prepend_args : string list -> t -> t

val append_args : string list -> t -> t

val get_orig_argv : t -> string array

(** update the executable to be run *)

val with_exec : string -> t -> t
