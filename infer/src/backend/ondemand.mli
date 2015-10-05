(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for on-demand analysis. *)

type analyze_proc = Procname.t -> unit

type get_proc_desc = Procname.t -> Cfg.Procdesc.t option

(** do_analysis get_proc_desc curr_descproc_name
    performs an on-demand analysis of proc_name
    triggered during the analysis of curr_pdesc. *)
val do_analysis : get_proc_desc -> Cfg.Procdesc.t -> Procname.t -> unit

(** Check if the procedure called by the current pdesc needs to be analyzed. *)
val procedure_should_be_analyzed : Cfg.Procdesc.t -> Procname.t -> bool

(** Mark the return type @Nullable by modifying the spec. *)
val proc_add_return_nullable : Procname.t -> unit

(** Set the function to be used to perform on-demand analysis. *)
val set_analyze_proc : analyze_proc -> unit

(** Unset the function to be used to perform on-demand analysis. *)
val unset_analyze_prop : unit -> unit
