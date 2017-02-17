(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module for on-demand analysis. *)

(** Optional set of source dirs to analyze in on-demand mode. If None then all source dirs
    will be analyzed *)
val dirs_to_analyze : String.Set.t option

type analyze_ondemand = SourceFile.t -> Procdesc.t -> unit

type get_proc_desc = Procname.t -> Procdesc.t option

type callbacks =
  {
    analyze_ondemand : analyze_ondemand;
    get_proc_desc : get_proc_desc;
  }

(** Find a proc desc for the procedure, perhaps loading it from disk. *)
val get_proc_desc : get_proc_desc

(** analyze_proc_desc curr_pdesc callee_pdesc
    performs an on-demand analysis of callee_pdesc
    triggered during the analysis of curr_pdesc. *)
val analyze_proc_desc :
  propagate_exceptions:bool -> Procdesc.t -> Procdesc.t -> Specs.summary option

(** analyze_proc_name curr_pdesc proc_name
    performs an on-demand analysis of proc_name
    triggered during the analysis of curr_pdesc. *)
val analyze_proc_name :
  propagate_exceptions:bool -> Procdesc.t -> Procname.t -> Specs.summary option

(** Check if the procedure called needs to be analyzed. *)
val procedure_should_be_analyzed : Procname.t -> bool

(** Set the callbacks used to perform on-demand analysis. *)
val set_callbacks : callbacks -> unit

(** Unset the callbacks used to perform on-demand analysis. *)
val unset_callbacks : unit -> unit
