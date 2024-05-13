(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Pretty Printing *)

(** Colors supported in printing *)
type color = Black | Blue | Green | Orange | Red [@@deriving compare]

val equal_color : color -> color -> bool

(** map subexpressions (as Obj.t element compared by physical equality) to colors *)
type colormap = Obj.t -> color

(** Kind of simple printing: default or with full types *)
type simple_kind = SIM_DEFAULT | SIM_WITH_TYP

(** Kind of printing *)
type print_kind = TEXT | HTML [@@deriving compare]

val equal_print_kind : print_kind -> print_kind -> bool

(** Print environment threaded through all the printing functions *)
type env =
  { opt: simple_kind  (** Current option for simple printing *)
  ; kind: print_kind  (** Current kind of printing *)
  ; break_lines: bool
        (** whether to let Format add its own line breaks or not (false by default) *)
  ; cmap_norm: colormap  (** Current colormap for the normal part *)
  ; cmap_foot: colormap  (** Current colormap for the footprint part *)
  ; color: color  (** Current color *)
  ; obj_sub: (Obj.t -> Obj.t) option  (** generic object substitution *) }

val reset_obj_sub : env -> env
(** Reset the object substitution, so that no substitution takes place *)

val set_obj_sub : env -> ('a -> 'a) -> env
(** Set the object substitution, which is supposed to preserve the type. Currently only used for a
    map from (identifier) expressions to the program var containing them *)

val extend_colormap : env -> Obj.t -> color -> env
(** Extend the normal colormap for the given object with the given color *)

val color_wrapper : env -> F.formatter -> 'a -> f:(env -> F.formatter -> 'a -> unit) -> unit

val text : env
(** Default text print environment *)

val text_break : env
(** text print environment that allows line breaks *)

val html : color -> env
(** Default html print environment *)

val color_string : color -> string
(** string representation of colors *)

val escape_xml : (F.formatter -> 'a -> unit) -> print_kind -> F.formatter -> 'a -> unit
(** escapes the output of the pretty printer parameter using HTML codes *)

val with_color : print_kind -> color -> (F.formatter -> 'a -> unit) -> F.formatter -> 'a -> unit

val html_with_color : color -> (F.formatter -> 'a -> unit) -> F.formatter -> 'a -> unit

val html_collapsible_block :
  name:string -> print_kind -> (F.formatter -> 'a -> unit) -> F.formatter -> 'a -> unit
(** Output the value in a named summary-details block. Both the name and the result of inner
    pretty-printer will be escaped. *)

val option : (F.formatter -> 'a -> unit) -> F.formatter -> 'a option -> unit

val cli_args : F.formatter -> string list -> unit
(** pretty print command line arguments, expanding argument files to print their contents *)

val cli_args_with_verbosity : verbose:bool -> F.formatter -> string list -> unit
(** pretty print command line arguments, and expand argument files if [verbose] is true *)

val seq :
     ?print_env:env
  -> ?sep:string
  -> ?sep_html:string
  -> (F.formatter -> 'a -> unit)
  -> F.formatter
  -> 'a list
  -> unit
(** Pretty print a sequence with [sep] followed by a space between each element. By default,
    [print_env] is [text], [sep] is "", and [sep_html] set to [sep]. *)

val comma_seq : ?print_env:env -> (F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit
(** Pretty print a comma-separated sequence. *)

val comma_seq_diff : (F.formatter -> 'a -> unit) -> env -> F.formatter -> 'a list -> unit

val semicolon_seq : ?print_env:env -> (F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit
(** Pretty print a ;-separated sequence *)

val of_string : f:('a -> string) -> F.formatter -> 'a -> unit
(** If all you have is to_string, but you need pp_foo. *)

val string_of_pp : (F.formatter -> 'a -> unit) -> 'a -> string
(** If all you have is pp_foo, but you need to_string. *)

val pair :
     fst:(F.formatter -> 'a -> unit)
  -> snd:(F.formatter -> 'b -> unit)
  -> F.formatter
  -> 'a * 'b
  -> unit

val in_backticks : (F.formatter -> 'a -> unit) -> F.formatter -> 'a -> unit
[@@warning "-unused-value-declaration"]

val collection :
     fold:('t, 'item, bool) Container.fold
  -> sep:(unit, F.formatter, unit) format
  -> ?filter:('item -> bool)
  -> (F.formatter -> 'item -> unit)
  -> F.formatter
  -> 't
  -> unit
