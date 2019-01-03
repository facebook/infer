(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

(** Pretty Printing} *)

(** Kind of simple printing: default or with full types *)
type simple_kind = SIM_DEFAULT | SIM_WITH_TYP

(** Kind of printing *)
type print_kind = TEXT | HTML [@@deriving compare]

let equal_print_kind = [%compare.equal: print_kind]

(** Colors supported in printing *)
type color = Black | Blue | Green | Orange | Red [@@deriving compare]

let equal_color = [%compare.equal: color]

(** map subexpressions (as Obj.t element compared by physical equality) to colors *)
type colormap = Obj.t -> color

(** Print environment threaded through all the printing functions *)
type env =
  { opt: simple_kind  (** Current option for simple printing *)
  ; kind: print_kind  (** Current kind of printing *)
  ; break_lines: bool  (** whether to let Format add its own line breaks or not *)
  ; cmap_norm: colormap  (** Current colormap for the normal part *)
  ; cmap_foot: colormap  (** Current colormap for the footprint part *)
  ; color: color  (** Current color *)
  ; obj_sub: (Obj.t -> Obj.t) option  (** generic object substitution *) }

(** Create a colormap of a given color *)
let colormap_from_color color (_ : Obj.t) = color

(** standard colormap: black *)
let colormap_black (_ : Obj.t) = Black

(** red colormap *)
let colormap_red (_ : Obj.t) = Red

(** Default text print environment *)
let text =
  { opt= SIM_DEFAULT
  ; kind= TEXT
  ; break_lines= false
  ; cmap_norm= colormap_black
  ; cmap_foot= colormap_black
  ; color= Black
  ; obj_sub= None }


let text_break = {text with break_lines= true}

(** Default html print environment *)
let html color =
  { text with
    kind= HTML; cmap_norm= colormap_from_color color; cmap_foot= colormap_from_color color; color
  }


(** Extend the normal colormap for the given object with the given color *)
let extend_colormap pe (x : Obj.t) (c : color) =
  let colormap (y : Obj.t) = if phys_equal x y then c else pe.cmap_norm y in
  {pe with cmap_norm= colormap}


(** Set the object substitution, which is supposed to preserve the type.
    Currently only used for a map from (identifier) expressions to the program var containing them *)
let set_obj_sub pe (sub : 'a -> 'a) =
  let new_obj_sub x =
    let x' = Obj.repr (sub (Obj.obj x)) in
    match pe.obj_sub with None -> x' | Some sub' -> sub' x'
  in
  {pe with obj_sub= Some new_obj_sub}


(** Reset the object substitution, so that no substitution takes place *)
let reset_obj_sub pe = {pe with obj_sub= None}

(** string representation of colors *)
let color_string = function
  | Black ->
      "color_black"
  | Blue ->
      "color_blue"
  | Green ->
      "color_green"
  | Orange ->
      "color_orange"
  | Red ->
      "color_red"


let seq ?(print_env = text) ?sep:(sep_text = " ") ?(sep_html = sep_text) pp =
  let rec aux f = function
    | [] ->
        ()
    | [x] ->
        pp f x
    | x :: l ->
        let sep = match print_env.kind with TEXT -> sep_text | HTML -> sep_html in
        if print_env.break_lines then F.fprintf f "%a%s@ %a" pp x sep aux l
        else F.fprintf f "%a%s%a" pp x sep aux l
  in
  aux


let comma_seq ?print_env pp f l = seq ?print_env ~sep:"," pp f l

let semicolon_seq ?print_env pp f l = seq ?print_env ~sep:"; " pp f l

(** Print the current time and date in a format similar to the "date" command *)
let current_time f () =
  let tm = Unix.localtime (Unix.time ()) in
  F.fprintf f "%02d/%02d/%4d %02d:%02d" tm.Unix.tm_mday tm.Unix.tm_mon (tm.Unix.tm_year + 1900)
    tm.Unix.tm_hour tm.Unix.tm_min


(** Print the time in seconds elapsed since the beginning of the execution of the current command. *)
let elapsed_time fmt () = Mtime.Span.pp fmt (Mtime_clock.elapsed ())

let option pp fmt = function
  | None ->
      F.pp_print_string fmt "<None>"
  | Some x ->
      F.fprintf fmt "<Some %a>" pp x


let to_string ~f fmt x = F.pp_print_string fmt (f x)

let cli_args fmt args =
  let pp_args fmt args =
    F.fprintf fmt "@[<hov2>  " ;
    seq ~sep:"" ~print_env:text_break F.pp_print_string fmt args ;
    F.fprintf fmt "@]"
  in
  let rec pp_argfile_args in_argfiles fmt args =
    let at_least_one = ref false in
    List.iter args ~f:(fun arg ->
        String.chop_prefix ~prefix:"@" arg
        |> Option.iter ~f:(fun argfile ->
               if not !at_least_one then (
                 F.fprintf fmt "@[<hov2>  " ;
                 at_least_one := true ) ;
               pp_argfile in_argfiles fmt argfile ) ) ;
    if !at_least_one then F.fprintf fmt "@]@\n"
  and pp_argfile in_argfiles fmt fname =
    if not (String.Set.mem in_argfiles fname) then
      let in_argfiles' = String.Set.add in_argfiles fname in
      match In_channel.read_lines fname with
      | args ->
          F.fprintf fmt "++Contents of %s:@\n%a@\n"
            (Escape.escape_in_single_quotes fname)
            pp_args args ;
          pp_argfile_args in_argfiles' fmt args ;
          ()
      | exception exn ->
          F.fprintf fmt "@\n++Error reading file %s:@\n  %a@\n"
            (Escape.escape_in_single_quotes fname)
            Exn.pp exn
  in
  pp_args fmt args ;
  pp_argfile_args String.Set.empty fmt args


let pair ~fst ~snd fmt (a, b) = F.fprintf fmt "(%a,@,%a)" fst a snd b
