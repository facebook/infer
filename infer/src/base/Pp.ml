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
module F = Format

(** Pretty Printing} *)

(** Kind of simple printing: default or with full types *)
type simple_kind = SIM_DEFAULT | SIM_WITH_TYP

(** Kind of printing *)
type print_kind = TEXT | LATEX | HTML [@@deriving compare]

let equal_print_kind = [%compare.equal : print_kind]

(** Colors supported in printing *)
type color = Black | Blue | Green | Orange | Red [@@deriving compare]

let equal_color = [%compare.equal : color]

(** map subexpressions (as Obj.t element compared by physical equality) to colors *)
type colormap = Obj.t -> color

(** Print environment threaded through all the printing functions *)
type env =
  { opt: simple_kind  (** Current option for simple printing *)
  ; kind: print_kind  (** Current kind of printing *)
  ; cmap_norm: colormap  (** Current colormap for the normal part *)
  ; cmap_foot: colormap  (** Current colormap for the footprint part *)
  ; color: color  (** Current color *)
  ; obj_sub: (Obj.t -> Obj.t) option  (** generic object substitution *) }

(** Create a colormap of a given color *)
let colormap_from_color color (_: Obj.t) = color

(** standard colormap: black *)
let colormap_black (_: Obj.t) = Black

(** red colormap *)
let colormap_red (_: Obj.t) = Red

(** Default text print environment *)
let text =
  { opt= SIM_DEFAULT
  ; kind= TEXT
  ; cmap_norm= colormap_black
  ; cmap_foot= colormap_black
  ; color= Black
  ; obj_sub= None }

(** Default html print environment *)
let html color =
  { text with
    kind= HTML; cmap_norm= colormap_from_color color; cmap_foot= colormap_from_color color; color }

(** Default latex print environment *)
let latex color =
  { opt= SIM_DEFAULT
  ; kind= LATEX
  ; cmap_norm= colormap_from_color color
  ; cmap_foot= colormap_from_color color
  ; color
  ; obj_sub= None }

(** Extend the normal colormap for the given object with the given color *)
let extend_colormap pe (x: Obj.t) (c: color) =
  let colormap (y: Obj.t) = if phys_equal x y then c else pe.cmap_norm y in
  {pe with cmap_norm= colormap}

(** Set the object substitution, which is supposed to preserve the type.
    Currently only used for a map from (identifier) expressions to the program var containing them *)
let set_obj_sub pe (sub: 'a -> 'a) =
  let new_obj_sub x =
    let x' = Obj.repr (sub (Obj.obj x)) in
    match pe.obj_sub with None -> x' | Some sub' -> sub' x'
  in
  {pe with obj_sub= Some new_obj_sub}

(** Reset the object substitution, so that no substitution takes place *)
let reset_obj_sub pe = {pe with obj_sub= None}

(** string representation of colors *)
let color_string = function
  | Black
   -> "color_black"
  | Blue
   -> "color_blue"
  | Green
   -> "color_green"
  | Orange
   -> "color_orange"
  | Red
   -> "color_red"

(** Pretty print a space-separated sequence *)
let rec seq pp f = function
  | []
   -> ()
  | [x]
   -> F.fprintf f "%a" pp x
  | x :: l
   -> F.fprintf f "%a %a" pp x (seq pp) l

(** Print a comma-separated sequence *)
let rec comma_seq pp f = function
  | []
   -> ()
  | [x]
   -> F.fprintf f "%a" pp x
  | x :: l
   -> F.fprintf f "%a,%a" pp x (comma_seq pp) l

(** Print a ;-separated sequence. *)
let rec _semicolon_seq oneline pe pp f =
  let sep fmt () = if oneline then F.fprintf fmt " " else F.fprintf fmt "@\n" in
  function
    | []
     -> ()
    | [x]
     -> F.fprintf f "%a" pp x
    | x :: l ->
      match pe.kind with
      | TEXT | HTML
       -> F.fprintf f "%a ; %a%a" pp x sep () (_semicolon_seq oneline pe pp) l
      | LATEX
       -> F.fprintf f "%a ;\\\\%a %a" pp x sep () (_semicolon_seq oneline pe pp) l

(** Print a ;-separated sequence with newlines. *)
let semicolon_seq pe = _semicolon_seq false pe

(** Print a ;-separated sequence on one line. *)
let semicolon_seq_oneline pe = _semicolon_seq true pe

(** Print an or-separated sequence. *)
let or_seq pe pp f = function
  | []
   -> ()
  | [x]
   -> F.fprintf f "%a" pp x
  | x :: l ->
    match pe.kind with
    | TEXT
     -> F.fprintf f "%a || %a" pp x (semicolon_seq pe pp) l
    | HTML
     -> F.fprintf f "%a &or; %a" pp x (semicolon_seq pe pp) l
    | LATEX
     -> F.fprintf f "%a \\vee %a" pp x (semicolon_seq pe pp) l

(** Print the current time and date in a format similar to the "date" command *)
let current_time f () =
  let tm = Unix.localtime (Unix.time ()) in
  F.fprintf f "%02d/%02d/%4d %02d:%02d" tm.Unix.tm_mday tm.Unix.tm_mon (tm.Unix.tm_year + 1900)
    tm.Unix.tm_hour tm.Unix.tm_min

(** Print the time in seconds elapsed since the beginning of the execution of the current command. *)
let elapsed_time fmt () =
  let elapsed = Unix.gettimeofday () -. Utils.initial_timeofday in
  Format.fprintf fmt "%f" elapsed
