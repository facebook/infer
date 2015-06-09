(*
* Copyright (c) 2015 - Facebook.
* All rights reserved.
*)

(** Filter type for a source file *)
type path_filter = DB.source_file -> bool

(** Filter type for an error name. *)
type error_filter = Localise.t -> bool

type filters =
  {
    path_filter : path_filter;
    error_filter: error_filter;
  }

(** Filters that accept everything. *)
val do_not_filter : filters

(** Create filters based on the config file *)
val create_filters : Utils.analyzer -> filters

module NeverReturnNull : sig
  type matcher = DB.source_file -> Procname.t -> bool
  val load_matcher : Sil.language -> matcher
end

(** Load the config file and list the files to report on *)
val test: unit -> unit
