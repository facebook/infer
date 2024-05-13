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

(** Kind of simple printing: default or with full types *)
type simple_kind = SIM_DEFAULT | SIM_WITH_TYP

(** Kind of printing *)
type print_kind = TEXT | HTML [@@deriving compare, equal]

(** Colors supported in printing *)
type color = Black | Blue | Green | Orange | Red [@@deriving compare, equal]

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
    kind= HTML
  ; cmap_norm= colormap_from_color color
  ; cmap_foot= colormap_from_color color
  ; color }


(** Extend the normal colormap for the given object with the given color *)
let extend_colormap pe (x : Obj.t) (c : color) =
  let colormap (y : Obj.t) = if phys_equal x y then c else pe.cmap_norm y in
  {pe with cmap_norm= colormap}


(** Set the object substitution, which is supposed to preserve the type. Currently only used for a
    map from (identifier) expressions to the program var containing them *)
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


let html_with_color color pp f x =
  let before = Printf.sprintf "<span class='%s'>" (color_string color) in
  let after = "</span>" in
  F.fprintf f "@<0>%s%a@<0>%s" before pp x after


let with_color kind color pp fmt x =
  match kind with HTML -> html_with_color color pp fmt x | TEXT -> pp fmt x


let escape_xml pp pp_kind fmt x =
  match pp_kind with
  | TEXT ->
      pp fmt x
  | HTML ->
      let original = F.asprintf "%a" pp x in
      let escaped = Escape.escape_xml original in
      (* use [pp_print_as] so as not to mess up formatting: the display length of [escaped] is the
         length of [original] *)
      F.pp_print_as fmt (String.length original) escaped


let html_collapsible_block ~name pp_kind pp f x =
  match pp_kind with
  | TEXT ->
      F.fprintf f "@[<hv2>%s: %a@]" name pp x
  | HTML ->
      let before =
        Printf.sprintf "<details class='state'><summary>%s</summary><p>" (Escape.escape_xml name)
      in
      let after = "</p></details>" in
      F.fprintf f "@<0>%s%a@<0>%s" before pp x after


let color_wrapper pe ppf x ~f =
  match pe.kind with
  | HTML when not (equal_color (pe.cmap_norm (Obj.repr x)) pe.color) ->
      let color = pe.cmap_norm (Obj.repr x) in
      let pe' =
        if equal_color color Red then
          (* All subexpressions red *)
          {pe with cmap_norm= colormap_red; color= Red}
        else {pe with color}
      in
      html_with_color color (f pe') ppf x
  | _ ->
      f pe ppf x


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

let comma_seq_diff pp pe0 f =
  let rec doit = function
    | [] ->
        ()
    | [x] ->
        color_wrapper pe0 f x ~f:(fun _pe -> pp)
    | x :: l ->
        color_wrapper pe0 f x ~f:(fun _pe -> pp) ;
        F.pp_print_string f ", " ;
        doit l
  in
  doit


let option pp fmt = function
  | None ->
      F.pp_print_string fmt "[None]"
  | Some x ->
      F.fprintf fmt "[Some %a]" pp x


let of_string ~f fmt x = Fmt.of_to_string f fmt x

let string_of_pp pp x = Fmt.to_to_string pp x

let cli_args_with_verbosity ~verbose fmt args =
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
  if verbose then pp_argfile_args String.Set.empty fmt args


let cli_args fmt args = cli_args_with_verbosity ~verbose:true fmt args

let pair ~fst ~snd fmt (a, b) = F.fprintf fmt "(%a,@,%a)" fst a snd b

let in_backticks pp fmt x = F.fprintf fmt "`%a`" pp x

let collection :
       fold:('t, 'item, _) Container.fold
    -> sep:(unit, F.formatter, unit) format
    -> ?filter:('item -> bool)
    -> (F.formatter -> 'item -> unit)
    -> F.formatter
    -> 't
    -> unit =
 fun ~fold ~sep ?(filter = fun _ -> true) pp_item fmt coll ->
  let pp_coll_aux print_sep item =
    if filter item then (
      F.fprintf fmt "@[<h>" ;
      if print_sep then F.fprintf fmt sep ;
      F.fprintf fmt "%a@]" pp_item item ;
      true )
    else print_sep
  in
  F.fprintf fmt "@[<hv>%t@]" (fun _fmt -> fold coll ~init:false ~f:pp_coll_aux |> ignore)
