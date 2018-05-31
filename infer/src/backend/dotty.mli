(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Pretty printing functions in dot format. *)

(** {2 Contol-Flow Graph} *)

val print_icfg_dotty : SourceFile.t -> Cfg.t -> unit
(** Print the cfg *)

(** {2 Specs} *)

val pp_speclist_dotty_file : DB.filename -> Prop.normal BiabductionSummary.spec list -> unit
(** Dotty printing for specs *)
