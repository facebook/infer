(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type complete =
  { summary_loads: Procname.t list
  ; recursion_edges: Procname.Set.t
  ; other_proc_names: Procname.t list
  ; used_tenv_sources: SourceFile.t list }

type t =
  | Partial
  | Complete of complete
      (** Dependencies are [partial] and mutable while the summary to which they belong is being
          computed, then made [complete] and immutable once the summary is fully analyzed. *)

val pp : F.formatter -> t -> unit

(** Mutable state keeping track during on-demand interprocedural analysis of (1) which procedure is
    currently being analyzed and (2) which procedures type environments were used to compute
    summaries.

    Located here in the IR module to avoid adding parameters threading the currently-under-analysis
    procedure throughout various analysis engine and checker code. These dependencies are then used
    in the Backend module to conservatively invalidate procedure summaries that were computed using
    out-of-date type environment information. *)

val currently_under_analysis : Procname.t option ref

val reset : Procname.t -> t

val freeze : Procname.t -> t -> complete

val complete_exn : t -> complete

type kind =
  | SummaryLoad  (** the summary of a procedure was queried *)
  | RecursionEdge
      (** ondemand returned an empty summary because a mutual recursion cycle was detected *)
  | Other  (** anything else, eg the attributes of the function were loaded *)

val record_pname_dep : ?caller:Procname.t -> kind -> Procname.t -> unit

val record_srcfile_dep : SourceFile.t -> unit

val clear : unit -> unit
(** drop all currently-recorded dependency edges to reclaim memory *)

val merge : complete -> complete -> complete
