(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module for user-defined checkers. *)

(** State that persists in the .specs files. *)
module ST : sig
  val report_error :
    Tenv.t -> Typ.Procname.t -> Procdesc.t -> Localise.t -> Location.t -> ?advice:string option
    -> ?field_name:Typ.Fieldname.t option -> ?origin_loc:Location.t option
    -> ?exception_kind:(string -> Localise.error_desc -> exn) -> ?always_report:bool -> string
    -> unit
  (** Report an error. *)
end

(* ST *)

module PP : sig
  val pp_loc_range : Printer.LineReader.t -> int -> int -> Format.formatter -> Location.t -> unit
  (** Print a range of lines of the source file in [loc], including [nbefore] lines before loc
      and [nafter] lines after [loc] *)
end
(* PP *)
