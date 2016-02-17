(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for on-demand analysis. *)

(** Optional set of source dirs to analyze in on-demand mode. *)
val read_dirs_to_analyze : unit -> StringSet.t option

type analyze_ondemand = Procname.t -> unit

type get_proc_desc = Procname.t -> Cfg.Procdesc.t option

type callbacks =
  {
    analyze_ondemand : analyze_ondemand;
    get_proc_desc : get_proc_desc;
  }

(** do_analysis curr_pdesc proc_name
    performs an on-demand analysis of proc_name
    triggered during the analysis of curr_pname. *)
val do_analysis : Cfg.Procdesc.t -> Procname.t -> unit

val one_cluster_per_procedure : unit -> bool

(** Check if the procedure called by the current pdesc needs to be analyzed. *)
val procedure_should_be_analyzed : Cfg.Procdesc.t -> Procname.t -> bool

(** Set the callbacks used to perform on-demand analysis. *)
val set_callbacks : callbacks -> unit

(** Unset the callbacks used to perform on-demand analysis. *)
val unset_callbacks : unit -> unit
