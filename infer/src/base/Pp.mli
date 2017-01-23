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

(** Pretty Printing} *)

(** Colors supported in printing *)
type color = Black | Blue | Green | Orange | Red [@@deriving compare]

val equal_color : color -> color -> bool

(** map subexpressions (as Obj.t element compared by physical equality) to colors *)
type colormap = Obj.t -> color

(** Kind of simple printing: default or with full types *)
type simple_kind = SIM_DEFAULT | SIM_WITH_TYP

(** Kind of printing *)
type print_kind = TEXT | LATEX | HTML [@@deriving compare]

val equal_print_kind : print_kind -> print_kind -> bool

(** Print environment threaded through all the printing functions *)
type env = {
  opt : simple_kind; (** Current option for simple printing *)
  kind : print_kind; (** Current kind of printing *)
  cmap_norm : colormap; (** Current colormap for the normal part *)
  cmap_foot : colormap; (** Current colormap for the footprint part *)
  color : color; (** Current color *)
  obj_sub : (Obj.t -> Obj.t) option (** generic object substitution *)
}

(** Reset the object substitution, so that no substitution takes place *)
val reset_obj_sub : env -> env

(** Set the object substitution, which is supposed to preserve the type.
    Currently only used for a map from (identifier) expressions to the program var containing them *)
val set_obj_sub : env -> ('a -> 'a) -> env

(** standard colormap: black *)
val colormap_black : colormap

(** red colormap *)
val colormap_red : colormap

(** Extend the normal colormap for the given object with the given color *)
val extend_colormap : env -> Obj.t -> color -> env

(** Default text print environment *)
val text : env

(** Default html print environment *)
val html : color -> env

(** Default latex print environment *)
val latex : color -> env

(** string representation of colors *)
val color_string : color -> string

(** Pretty print a space-separated sequence *)
val seq : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(** Pretty print a comma-separated sequence *)
val comma_seq : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(** Pretty print a ;-separated sequence *)
val semicolon_seq : env -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(** Pretty print a ;-separated sequence on one line *)
val semicolon_seq_oneline : env -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(** Pretty print a or-separated sequence *)
val or_seq : env -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(** Print the current time and date in a format similar to the "date" command *)
val current_time : Format.formatter -> unit -> unit

(** Print the time in seconds elapsed since the beginning of the execution of the current command. *)
val elapsed_time : Format.formatter -> unit -> unit
