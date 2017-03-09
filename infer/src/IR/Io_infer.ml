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

(** Module to handle IO. Includes html and xml modules. *)

module F = Format

(* =============== START of module Html =============== *)
module Html =
struct
  (** Create a new html file *)
  let create pk path =
    let fname, dir_path = match List.rev path with
      | fname :: path_rev ->
          fname, List.rev ((fname ^ ".html") :: path_rev)
      | [] ->
          raise (Failure "Html.create") in
    let fd = DB.Results_dir.create_file pk dir_path in
    let outc = Unix.out_channel_of_descr fd in
    let fmt = F.formatter_of_out_channel outc in
    let s =
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n\
       <html>\n\
       <head>\n\
       <title>" ^
      fname ^
      "</title>\n\
       <style type=\"text/css\">\n\
       body { color:#000000; background-color:#ffffff }\n\
       body { font-family:Helvetica, sans-serif; font-size:10pt }\n\
       h1 { font-size:14pt }\n\
       .code { border-collapse:collapse; width:100%; }\n\
       .code { font-family: \"Andale Mono\", monospace; font-size:10pt }\n\
       .code { line-height: 1.2em }\n\
       .comment { color: green; font-style: oblique }\n\
       .keyword { color: blue }\n\
       .string_literal { color: red }\n\
       .color_black { color: black }\n\
       .color_blue { color: blue }\n\
       .color_green { color: green }\n\
       .color_red { color: red }\n\
       .color_orange { color: orange }\n\
       .directive { color: darkmagenta }\n\
       .expansion { display: none; }\n\
       .visited:hover .expansion {\
       display: block;\
       border: 2px\
       solid #FF0000;\
       padding: 2px;\
       background-color:#FFF0F0;\
       font-weight: normal;\
       -webkit-border-radius:5px;\
       -webkit-box-shadow:1px 1px 7px #000;\
       position: absolute;\
       top: -1em;\
       left:10em;\
       z-index: 1 }\n\
       .visited {\
       color: darkmagenta;\
       background-color:LemonChiffon;\
       position: relative }\n\
       .visitedproof:hover .expansion {\
       display: block;\
       border: 2px solid #FF0000;\
       padding: 2px;\
       background-color:#FFF0F0;\
       font-weight: normal;\
       -webkit-border-radius:5px;\
       -webkit-box-shadow:1px 1px 7px #000;\
       position: absolute;\
       top: -1em;\
       left:10em;\
       z-index: 1 }\n\
       .visitedproof {\
       color: darkmagenta;\
       background-color:lightgreen;\
       position: relative }\n\
       .dangling:hover .expansion {\
       display: block;\
       border: 2px solid #FF0000;\
       padding: 2px;\
       background-color:#FFF0F0;\
       font-weight: normal;\
       -webkit-border-radius:5px;\
       -webkit-box-shadow:1px 1px 7px #000;\
       position: absolute;\
       top: -1em;\
       left:10em;\
       z-index: 1 }\n\
       .dangling { color: gray; background-color:white; position: relative }\n\
       .num { width:2.5em; padding-right:2ex; background-color:#eeeeee }\n\
       .num { text-align:right; font-size: smaller }\n\
       .num { color:#444444 }\n\
       .line { padding-left: 1ex; border-left: 3px solid #ccc }\n\
       .line { white-space: pre }\n\
       .msg { background-color:#fff8b4; color:#000000 }\n\
       .msg { -webkit-box-shadow:1px 1px 7px #000 }\n\
       .msg { -webkit-border-radius:5px }\n\
       .msg { font-family:Helvetica, sans-serif; font-size: smaller }\n\
       .msg { font-weight: bold }\n\
       .msg { float:left }\n\
       .msg { padding:0.5em 1ex 0.5em 1ex }\n\
       .msg { margin-top:10px; margin-bottom:10px }\n\
       .msg { max-width:60em; word-wrap: break-word; white-space: pre-wrap;}\n\
       .mrange { background-color:#dfddf3 }\n\
       .mrange { border-bottom:1px solid #6F9DBE }\n\
       .PathIndex { font-weight: bold }\n\
       table.simpletable { padding: 5px; font-size:12pt; margin:20px; border-collapse: collapse;\
       border-spacing: 0px; }\n\
       td.rowname { text-align:right; font-weight:bold; color:#444444; padding-right:2ex; }\n\
       </style>\n\
       </head>\
       \n\
       <body>\
       \n" in
    F.fprintf fmt "%s" s;
    (fd, fmt)

  (** Get the full html filename from a path *)
  let get_full_fname source path =
    let dir_path = match List.rev path with
      | fname :: path_rev ->
          List.rev ((fname ^ ".html") :: path_rev)
      | [] ->
          raise (Failure "Html.open_out") in
    DB.Results_dir.path_to_filename (DB.Results_dir.Abs_source_dir source) dir_path

  (** Open an Html file to append data *)
  let open_out source path =
    let full_fname = get_full_fname source path in
    let fd =
      Unix.openfile
        (DB.filename_to_string full_fname)
        ~mode:Unix.[O_WRONLY; O_APPEND]
        ~perm:0o777 in
    let outc = Unix.out_channel_of_descr fd in
    let fmt = F.formatter_of_out_channel outc in
    (fd, fmt)

  (** Return true if the html file was modified since the beginning of the analysis *)
  let modified_during_analysis source path =
    let fname = get_full_fname source path in
    if DB.file_exists fname then
      DB.file_modified_time fname >= Config.initial_analysis_time
    else false

  (** Close an Html file *)
  let close (fd, fmt) =
    F.fprintf fmt "</body>@\n</html>@.";
    Unix.close fd

  (** Print a horizontal line *)
  let pp_hline fmt () =
    F.fprintf fmt "<hr width=\"100%%\">@\n"

  (** Print start color *)
  let pp_start_color fmt color =
    F.fprintf fmt "%s" ("<span class='" ^ (Pp.color_string color) ^ "'>")

  (** Print end color *)
  let pp_end_color fmt () =
    F.fprintf fmt "%s" "</span>"

  let pp_link ?(name = None) ?(pos = None) ~path fmt text =
    let pos_str = match pos with
      | None -> ""
      | Some s -> "#" ^ s in
    let link_str =
      (DB.filename_to_string (DB.Results_dir.path_to_filename DB.Results_dir.Rel path))
      ^ ".html"
      ^ pos_str in
    let name_str = match name with
      | None -> ""
      | Some n -> "name=\"" ^ n ^ "\"" in
    let pr_str = "<a " ^ name_str ^ "href=\"" ^ link_str ^ "\">" ^ text ^ "</a>" in
    F.fprintf fmt " %s" pr_str

  (** File name for the node, given the procedure name and node id *)
  let node_filename pname id = (Typ.Procname.to_filename pname) ^ "_node" ^ string_of_int id

  (** Print an html link to the given node. *)
  let pp_node_link path_to_root pname ~description ~preds ~succs ~exn ~isvisited ~isproof fmt id =
    let display_name =
      (if String.equal description "" then "N" else String.sub description ~pos:0 ~len:1)
      ^ "_"
      ^ (string_of_int id) in
    let node_fname = node_filename pname id in
    let style_class =
      if not isvisited
      then "dangling"
      else if isproof then "visitedproof" else "visited" in
    let node_text =
      let pp fmt =
        Format.fprintf fmt
          "<span class='%s'>%s\
           <span class='expansion'>\
           node%d preds:%a succs:%a exn:%a %s%s\
           </span>\
           </span>"
          style_class display_name id
          (Pp.seq Format.pp_print_int) preds
          (Pp.seq Format.pp_print_int) succs
          (Pp.seq Format.pp_print_int) exn
          description
          (if not isvisited then "\nNOT VISITED" else "") in
      F.asprintf "%t" pp in
    if not isvisited
    then F.fprintf fmt " %s" node_text
    else pp_link ~path: (path_to_root @ ["nodes"; node_fname]) fmt node_text

  (** Print an html link to the given proc *)
  let pp_proc_link path_to_root proc_name fmt text =
    pp_link ~path: (path_to_root @ [Typ.Procname.to_filename proc_name]) fmt text

  (** Print an html link to the given line number of the current source file *)
  let pp_line_link ?(with_name = false) ?(text = None) source path_to_root fmt linenum =
    let fname = SourceFile.encoding source in
    let linenum_str = string_of_int linenum in
    let name = "LINE" ^ linenum_str in
    pp_link
      ~name: (if with_name then Some name else None)
      ~pos: (Some name)
      ~path: (path_to_root @ [".."; fname])
      fmt
      (match text with Some s -> s | None -> linenum_str)

  (** Print an html link given node id and session *)
  let pp_session_link ?(with_name = false) source path_to_root fmt (node_id, session, linenum) =
    let node_name = "node" ^ (string_of_int node_id) in
    let path_to_node = path_to_root @ ["nodes"; node_name] in
    let pos = "session" ^ (string_of_int session) in
    pp_link
      ~name: (if with_name then Some pos else None)
      ~pos: (Some pos)
      ~path: path_to_node
      fmt
      (node_name ^ "#" ^ pos);
    F.fprintf fmt "(%a)" (pp_line_link source path_to_root) linenum
end
(* =============== END of module Html =============== *)

(* =============== START of module Xml =============== *)
(** Create and print xml trees *)
module Xml =
struct
  let tag_branch = "branch"
  let tag_call_trace = "call_trace"
  let tag_callee = "callee"
  let tag_callee_id = "callee_id"
  let tag_caller = "caller"
  let tag_caller_id = "caller_id"
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
    Pp.seq pp_attribute fmt l

  (** print an xml node *)
  let rec pp_node newline indent fmt = function
    | Tree { name = name; attributes = attributes; forest = forest } ->
        let indent' = if String.equal newline "" then "" else indent ^ "  " in
        let space = if List.is_empty attributes then "" else " " in
        let pp_inside fmt () = match forest with
          | [] ->
              ()
          | [String s] ->
              pp fmt "%s" s
          | _ ->
              pp fmt "%s%a%s" newline (pp_forest newline indent') forest indent in
        pp fmt "%s<%s%s%a>%a</%s>%s"
          indent
          name
          space
          pp_attributes attributes
          pp_inside ()
          name
          newline
    | String s ->
        F.fprintf fmt "%s%s%s" indent s newline
  and pp_forest newline indent fmt forest =
    List.iter ~f:(pp_node newline indent fmt) forest

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
