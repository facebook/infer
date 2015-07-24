(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module to handle IO. Includes html and xml modules. *)

module F = Format
open Utils

(* =============== START of module Html =============== *)
module Html : sig
  val close : Unix.file_descr * Format.formatter -> unit (** Close an Html file *)
  val create : DB.Results_dir.path_kind -> DB.Results_dir.path -> Unix.file_descr * Format.formatter (** Create a new html file *)
  val modified_during_analysis : DB.Results_dir.path -> bool (** Return true if the html file was modified since the beginning of the analysis *)
  val open_out : DB.Results_dir.path -> Unix.file_descr * Format.formatter (** Open an Html file to append data *)
  val pp_line_link : ?with_name: bool -> ?text: (string option) -> DB.Results_dir.path -> Format.formatter -> int -> unit (** Print an html link to the given line number of the current source file *)
  val pp_hline : Format.formatter -> unit -> unit (** Print a horizontal line *)
  val pp_end_color : Format.formatter -> unit -> unit (** Print end color *)

  (** [pp_node_link path_to_root description isvisited isproof fmt id] prints an html link to the given node.
      [path_to_root] is the path to the dir for the procedure in the spec db.
      [description] is a string description.
      [is_visited] indicates whether the node should be active or greyed out.
      [is_proof] indicates whether the node is part of a proof and should be green.
      [id] is the node identifier. *)
  val pp_node_link : DB.Results_dir.path -> string -> int list -> int list -> int list -> bool -> bool -> Format.formatter -> int -> unit
  val pp_proc_link : DB.Results_dir.path -> Procname.t -> Format.formatter -> string -> unit (** Print an html link to the given proc *)
  val pp_session_link : ?with_name: bool -> string list -> Format.formatter -> int * int * int -> unit (** Print an html link given node id and session *)
  val pp_start_color : Format.formatter -> color -> unit (** Print start color *)
end = struct
  let create pk path =
    let fname, dir_path = match list_rev path with
      | fname:: dir_path -> fname, dir_path
      | [] -> raise (Failure "Html.create") in
    let fd = DB.Results_dir.create_file pk (list_rev ((fname ^ ".html") :: dir_path)) in
    let outc = Unix.out_channel_of_descr fd in
    let fmt = F.formatter_of_out_channel outc in
    let (++) x y = x ^ "\n" ^ y in
    let s =
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">" ++
      "<html>\n<head>\n<title>" ^ fname ^ "</title>" ++
                                          "<style type=\"text/css\">" ++
                                          "body { color:#000000; background-color:#ffffff }" ++
                                          "body { font-family:Helvetica, sans-serif; font-size:10pt }" ++
                                          "h1 { font-size:14pt }" ++
                                          ".code { border-collapse:collapse; width:100%; }" ++
                                          ".code { font-family: \"Andale Mono\", monospace; font-size:10pt }" ++
                                          ".code { line-height: 1.2em }" ++
                                          ".comment { color: green; font-style: oblique }" ++
                                          ".keyword { color: blue }" ++
                                          ".string_literal { color: red }" ++
                                          ".color_black { color: black }" ++
                                          ".color_blue { color: blue }" ++
                                          ".color_green { color: green }" ++
                                          ".color_red { color: red }" ++
                                          ".color_orange { color: orange }" ++
                                          ".directive { color: darkmagenta }" ++
                                          ".expansion { display: none; }" ++
                                          ".visited:hover .expansion { display: block; border: 2px solid #FF0000; padding: 2px; background-color:#FFF0F0; font-weight: normal;   -webkit-border-radius:5px;  -webkit-box-shadow:1px 1px 7px #000; position: absolute; top: -1em; left:10em; z-index: 1 }" ++
                                          ".visited { color: darkmagenta; background-color:LemonChiffon; position: relative }" ++
                                          ".visitedproof:hover .expansion { display: block; border: 2px solid #FF0000; padding: 2px; background-color:#FFF0F0; font-weight: normal;   -webkit-border-radius:5px;  -webkit-box-shadow:1px 1px 7px #000; position: absolute; top: -1em; left:10em; z-index: 1 }" ++
                                          ".visitedproof { color: darkmagenta; background-color:lightgreen; position: relative }" ++
                                          ".dangling:hover .expansion { display: block; border: 2px solid #FF0000; padding: 2px; background-color:#FFF0F0; font-weight: normal;   -webkit-border-radius:5px;  -webkit-box-shadow:1px 1px 7px #000; position: absolute; top: -1em; left:10em; z-index: 1 }" ++
                                          ".dangling { color: gray; background-color:white; position: relative }" ++
                                          ".num { width:2.5em; padding-right:2ex; background-color:#eeeeee }" ++
                                          ".num { text-align:right; font-size: smaller }" ++
                                          ".num { color:#444444 }" ++
                                          ".line { padding-left: 1ex; border-left: 3px solid #ccc }" ++
                                          ".line { white-space: pre }" ++
                                          ".msg { background-color:#fff8b4; color:#000000 }" ++
                                          ".msg { -webkit-box-shadow:1px 1px 7px #000 }" ++
                                          ".msg { -webkit-border-radius:5px }" ++
                                          ".msg { font-family:Helvetica, sans-serif; font-size: smaller }" ++
                                          ".msg { font-weight: bold }" ++
                                          ".msg { float:left }" ++
                                          ".msg { padding:0.5em 1ex 0.5em 1ex }" ++
                                          ".msg { margin-top:10px; margin-bottom:10px }" ++
                                          ".msg { max-width:60em; word-wrap: break-word; white-space: pre-wrap;}" ++
                                          ".mrange { background-color:#dfddf3 }" ++
                                          ".mrange { border-bottom:1px solid #6F9DBE }" ++
                                          ".PathIndex { font-weight: bold }" ++
                                          "table.simpletable {" ++
                                          "padding: 5px;" ++
                                          "font-size:12pt;" ++
                                          "margin:20px;" ++
                                          "border-collapse: collapse; border-spacing: 0px;" ++
                                          "}" ++
                                          "td.rowname {" ++
                                          "text-align:right; font-weight:bold; color:#444444;" ++
                                          "padding-right:2ex; }" ++
                                          "</style>" ++
                                          "</head>" ++
                                          "<body" ^ ">" ++
                                                    "" in
    F.fprintf fmt "%s" s;
    (fd, fmt)

  (** get the full html filename from a path *)
  let get_full_fname path =
    let fname, dir_path = match list_rev path with
      | fname:: dir_path -> fname, dir_path
      | [] -> raise (Failure "Html.open_out") in
    DB.Results_dir.path_to_filename DB.Results_dir.Abs_source_dir (list_rev ((fname ^ ".html") :: dir_path))

  let open_out path =
    let full_fname = get_full_fname path in
    let fd = Unix.openfile (DB.filename_to_string full_fname) [Unix.O_WRONLY; Unix.O_APPEND] 0o777 in
    let outc = Unix.out_channel_of_descr fd in
    let fmt = F.formatter_of_out_channel outc in
    (fd, fmt)

  let modified_during_analysis path =
    let fname = get_full_fname path in
    if DB.file_exists fname then
      DB.file_modified_time fname >= initial_analysis_time
    else false

  let close (fd, fmt) =
    F.fprintf fmt "</body>@\n</html>@.";
    Unix.close fd

  (** Print a horizontal line *)
  let pp_hline fmt () =
    F.fprintf fmt "<hr width=\"100%%\">@\n"

  (** Print start color *)
  let pp_start_color fmt color =
    F.fprintf fmt "%s" ("<span class='" ^ (color_string color) ^ "'>")

  (** Print end color *)
  let pp_end_color fmt () =
    F.fprintf fmt "%s" "</span>"

  let pp_link ?(name = None) ?(pos = None) path fmt text =
    let pos_str = match pos with
      | None -> ""
      | Some s -> "#" ^ s in
    let link_str = (DB.filename_to_string (DB.Results_dir.path_to_filename DB.Results_dir.Rel path)) ^ ".html" ^ pos_str in
    let name_str = match name with
      | None -> ""
      | Some n -> "name=\"" ^ n ^ "\"" in
    let pr_str = "<a " ^ name_str ^ "href=\"" ^ link_str ^ "\">" ^ text ^ "</a>" in
    F.fprintf fmt " %s" pr_str

  (** [pp_node_link path_to_root description isvisited isproof fmt id] prints an html link to the given node. *)
  let pp_node_link path_to_root description preds succs exn isvisited isproof fmt id =
    let display_name =
      (if description = "" then "N" else String.sub description 0 1) ^ "_" ^ (string_of_int id) in
    let node_name = "node" ^ string_of_int id in
    let style_class = if not isvisited then "dangling" else if isproof then "visitedproof" else "visited" in
    let node_text =
      let pp fmt () =
        Format.fprintf fmt "<span class='%s'>%s<span class='expansion'>node%d preds:%a succs:%a exn:%a %s%s</span></span>"
          style_class display_name id
          (pp_seq Format.pp_print_int) preds (pp_seq Format.pp_print_int) succs (pp_seq Format.pp_print_int) exn
          description (if not isvisited then "\nNOT VISITED" else "") in
      pp_to_string pp () in
    if not isvisited then F.fprintf fmt " %s" node_text
    else pp_link (path_to_root @ ["nodes"; node_name]) fmt node_text

  (** Print an html link to the given proc *)
  let pp_proc_link path_to_root proc_name fmt text =
    pp_link (path_to_root @ [Procname.to_filename proc_name]) fmt text

  (** Print an html link to the given line number of the current source file *)
  let pp_line_link ?(with_name = false) ?(text = None) path_to_root fmt linenum =
    let fname = DB.source_file_encoding !DB.current_source in
    let linenum_str = string_of_int linenum in
    let name = "LINE" ^ linenum_str in
    pp_link ~name: (if with_name then Some name else None) (path_to_root @ [".."; fname]) ~pos: (Some name)
      fmt (match text with Some s -> s | None -> linenum_str)

  (** Print an html link given node id and session *)
  let pp_session_link ?(with_name = false) path_to_root fmt (node_id, session, linenum) =
    let node_name = "node" ^ (string_of_int node_id) in
    let path_to_node = path_to_root @ ["nodes"; node_name] in
    let pos = "session" ^ (string_of_int session) in
    pp_link ~name: (if with_name then Some pos else None) ~pos: (Some pos) path_to_node fmt (node_name ^ "#" ^ pos);
    F.fprintf fmt "(%a)" (pp_line_link path_to_root) linenum
end
(* =============== END of module Html =============== *)

(* =============== START of module Xml =============== *)
(** Create and print xml trees *)
module Xml = struct
  let tag_branch = "branch"
  let tag_call_trace = "call_trace"
  let tag_callee = "callee"
  let tag_callee_id = "callee_id"
  let tag_caller = "caller"
  let tag_caller_id = "caller_id"
  let tag_cyclomatic = "cyclomatic"
  let tag_class = "class"
  let tag_code = "code"
  let tag_description = "description"
  let tag_err = "err"
  let tag_flags = "flags"
  let tag_file = "file"
  let tag_hash = "hash"
  let tag_in_calls = "in_calls"
  let tag_key = "key"
  let tag_kind = "kind"
  let tag_level = "level"
  let tag_line = "line"
  let tag_loc = "loc"
  let tag_name = "name"
  let tag_name_id = "name_id"
  let tag_node = "node"
  let tag_out_calls = "out_calls"
  let tag_precondition = "precondition"
  let tag_procedure = "procedure"
  let tag_procedure_id = "procedure_id"
  let tag_proof_coverage = "proof_coverage"
  let tag_proof_trace = "proof_trace"
  let tag_qualifier = "qualifier"
  let tag_qualifier_tags = "qualifier_tags"
  let tag_rank = "rank"
  let tag_severity = "severity"
  let tag_signature = "signature"
  let tag_specs = "specs"
  let tag_symop = "symop"
  let tag_time = "time"
  let tag_to = "to"
  let tag_top = "top"
  let tag_trace = "trace"
  let tag_type = "type"
  let tag_weight = "weight"

  type tree = { name: string; attributes: (string * string) list; forest: node list }
  and node =
    | Tree of tree
    | String of string

  let pp = F.fprintf

  let create_tree name attributes forest =
    Tree { name = name; attributes = attributes; forest = forest }

  let pp_attribute fmt (name, value) =
    pp fmt "%s=\"%s\"" name value

  let pp_attributes fmt l =
    pp_seq pp_attribute fmt l

  (** print an xml node *)
  let rec pp_node newline indent fmt = function
    | Tree { name = name; attributes = attributes; forest = forest } ->
        let indent' = if newline = "" then "" else indent ^ "  " in
        let space = if attributes = [] then "" else " " in
        let pp_inside fmt () = match forest with
          | [] -> ()
          | [String s] -> pp fmt "%s" s
          | _ -> pp fmt "%s%a%s" newline (pp_forest newline indent') forest indent in
        pp fmt "%s<%s%s%a>%a</%s>%s" indent name space pp_attributes attributes pp_inside () name newline
    | String s ->
        F.fprintf fmt "%s%s%s" indent s newline
  and pp_forest newline indent fmt forest =
    list_iter (pp_node newline indent fmt) forest

  let pp_prelude fmt = pp fmt "%s" "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

  let pp_open fmt name =
    pp_prelude fmt;
    pp fmt "<%s>@\n" name

  let pp_close fmt name =
    pp fmt "</%s>@." name

  let pp_inner_node fmt node =
    pp_node "\n" "" fmt node

  (** print an xml document, if the first parameter is false on a single line without preamble *)
  let pp_document on_several_lines fmt node =
    let newline = if on_several_lines then "\n" else "" in
    if on_several_lines then pp_prelude fmt;
    pp_node newline "" fmt node;
    if on_several_lines then pp fmt "@."
end
(* =============== END of module Xml =============== *)
