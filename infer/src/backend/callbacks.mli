(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module to register and invoke callbacks *)

type proc_callback_args =
  { get_procs_in_file: Typ.Procname.t -> Typ.Procname.t list
  ; tenv: Tenv.t
  ; integer_type_widths: Typ.IntegerWidths.t
  ; summary: Summary.t
  ; proc_desc: Procdesc.t
  ; exe_env: Exe_env.t }

(** Type of a procedure callback:
    - List of all the procedures the callback will be called on.
    - get_proc_desc to get a proc desc from a proc name
    - Type environment.
    - Procedure for the callback to act on. *)
type proc_callback_t = proc_callback_args -> Summary.t

type cluster_callback_args =
  {procedures: (Tenv.t * Procdesc.t) list; source_file: SourceFile.t; exe_env: Exe_env.t}

type cluster_callback_t = cluster_callback_args -> unit

val register_procedure_callback :
  name:string -> ?dynamic_dispatch:bool -> Language.t -> proc_callback_t -> unit
(** register a procedure callback *)

val register_cluster_callback : name:string -> Language.t -> cluster_callback_t -> unit
(** register a cluster callback *)

val analyze_file : Exe_env.t -> SourceFile.t -> unit
(** Invoke all the registered callbacks on the given file. *)
