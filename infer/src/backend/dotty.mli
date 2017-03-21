(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Pretty printing functions in dot format. *)

(** {2 Propositions} *)

type kind_of_dotty_prop =
  | Generic_proposition
  | Spec_precondition
  | Spec_postcondition of Prop.normal Prop.t (** the precondition associated with the post *)
  | Lambda_pred of int * int * bool

val reset_proposition_counter : unit -> unit

val pp_dotty : Format.formatter -> kind_of_dotty_prop -> Prop.normal Prop.t ->
  ((Sil.strexp * Typ.t) * Fieldname.t * Sil.strexp) list option -> unit

(** {2 Sets and lists of propositions} *)

val pp_dotty_prop_list_in_path: Format.formatter -> Prop.normal Prop.t list -> int -> int -> unit

val pp_proplist_parsed2dotty_file : string -> Prop.normal Prop.t list -> unit

(** {2 Contol-Flow Graph} *)

(** Print the cfg *)
val print_icfg_dotty : SourceFile.t -> Cfg.cfg -> unit

(** {2 Specs} *)
val reset_dotty_spec_counter : unit -> unit

(** Dotty printing for specs *)
val pp_speclist_dotty_file : DB.filename -> Prop.normal Specs.spec list -> unit

(* create a dotty file with a single proposition *)
val dotty_prop_to_dotty_file : string -> Prop.normal Prop.t ->
  ((Sil.strexp * Typ.t) * Fieldname.t * Sil.strexp) list -> unit

val dotty_prop_to_str : Prop.normal Prop.t ->
  ((Sil.strexp * Typ.t) * Fieldname.t * Sil.strexp) list -> string option

(** reset the counter used for node and heap identifiers *)
val reset_node_counter : unit -> unit

(** convert a proposition to xml with the given tag and id *)
val prop_to_xml : Prop.normal Prop.t -> string -> int -> Io_infer.Xml.node

(** Print a list of specs in XML format *)
val print_specs_xml :
  string -> Prop.normal Specs.spec list -> Location.t -> Format.formatter -> unit
