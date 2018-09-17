(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
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
(** Set the object substitution, which is supposed to preserve the type.
    Currently only used for a map from (identifier) expressions to the program var containing them *)

val colormap_red : colormap
(** red colormap *)

val extend_colormap : env -> Obj.t -> color -> env
(** Extend the normal colormap for the given object with the given color *)

val text : env
(** Default text print environment *)

val text_break : env
(** text print environment that allows line breaks *)

val html : color -> env
(** Default html print environment *)

val color_string : color -> string
(** string representation of colors *)

val option : (F.formatter -> 'a -> unit) -> F.formatter -> 'a option -> unit

val cli_args : F.formatter -> string list -> unit
(** pretty print command line arguments, expanding argument files to print their contents *)

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

val semicolon_seq : ?print_env:env -> (F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit
(** Pretty print a ;-separated sequence *)

val to_string : f:('a -> string) -> F.formatter -> 'a -> unit
(** turn a "to_string" function into a "pp_foo" *)

val current_time : F.formatter -> unit -> unit
(** Print the current time and date in a format similar to the "date" command *)

val elapsed_time : F.formatter -> unit -> unit
(** Print the time in seconds elapsed since the beginning of the execution of the current command. *)

val pair :
     fst:(F.formatter -> 'a -> unit)
  -> snd:(F.formatter -> 'b -> unit)
  -> F.formatter
  -> 'a * 'b
  -> unit
