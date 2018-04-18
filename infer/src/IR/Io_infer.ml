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
module Html = struct
  (** Create a new html file *)
  let create pk path =
    let fname, dir_path =
      match List.rev path with
      | fname :: path_rev ->
          (fname, List.rev ((fname ^ ".html") :: path_rev))
      | [] ->
          raise (Failure "Html.create")
    in
    let fd = DB.Results_dir.create_file pk dir_path in
    let outc = Unix.out_channel_of_descr fd in
    let fmt = F.formatter_of_out_channel outc in
    let s =
      {|<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
<title>|}
      ^ fname
      ^ {|</title>
<style type="text/css">
body { color:#000000; background-color:#ffffff }
body { font-family:Helvetica, sans-serif; font-size:10pt }
h1 { font-size:14pt }
.code { border-collapse:collapse; width:100%; }
.code { font-family: "Andale Mono", monospace; font-size:10pt }
.code { line-height: 1.2em }
.comment { color: green; font-style: oblique }
.keyword { color: blue }
.string_literal { color: red }
.color_black { color: black }
.color_blue { color: blue }
.color_green { color: green }
.color_red { color: red }
.color_orange { color: orange }
.directive { color: darkmagenta }
.expansion { display: none; }
.visited:hover .expansion { display: block; border: 2px solid #FF0000; padding: 2px; background-color:#FFF0F0; font-weight: normal; -webkit-border-radius:5px; -webkit-box-shadow:1px 1px 7px #000; position: absolute; top: -1em; left:10em; z-index: 1 }
.visited { color: darkmagenta; background-color:LemonChiffon; position: relative }
.visitedproof:hover .expansion { display: block; border: 2px solid #FF0000; padding: 2px; background-color:#FFF0F0; font-weight: normal; -webkit-border-radius:5px; -webkit-box-shadow:1px 1px 7px #000; position: absolute; top: -1em; left:10em; z-index: 1 }
.visitedproof { color: darkmagenta; background-color:lightgreen; position: relative }
.dangling:hover .expansion { display: block; border: 2px solid #FF0000; padding: 2px; background-color:#FFF0F0; font-weight: normal; -webkit-border-radius:5px; -webkit-box-shadow:1px 1px 7px #000; position: absolute; top: -1em; left:10em; z-index: 1 }
.dangling { color: gray; background-color:white; position: relative }
.num { width:2.5em; padding-right:2ex; background-color:#eeeeee }
.num { text-align:right; font-size: smaller }
.num { color:#444444 }
.line { padding-left: 1ex; border-left: 3px solid #ccc }
.line { white-space: pre }
.msg { background-color:#fff8b4; color:#000000 }
.msg { -webkit-box-shadow:1px 1px 7px #000 }
.msg { -webkit-border-radius:5px }
.msg { font-family:Helvetica, sans-serif; font-size: smaller }
.msg { font-weight: bold }
.msg { float:left }
.msg { padding:0.5em 1ex 0.5em 1ex }
.msg { margin-top:10px; margin-bottom:10px }
.msg { max-width:60em; word-wrap: break-word; white-space: pre-wrap;}
.mrange { background-color:#dfddf3 }
.mrange { border-bottom:1px solid #6F9DBE }
.PathIndex { font-weight: bold }
table.simpletable { padding: 5px; font-size:12pt; margin:20px; border-collapse: collapse; border-spacing: 0px; }
td.rowname { text-align:right; font-weight:bold; color:#444444; padding-right:2ex; }
</style>
</head>
<body>
|}
    in
    F.pp_print_string fmt s ; (fd, fmt)


  (** Get the full html filename from a path *)
  let get_full_fname source path =
    let dir_path =
      match List.rev path with
      | fname :: path_rev ->
          List.rev ((fname ^ ".html") :: path_rev)
      | [] ->
          raise (Failure "Html.open_out")
    in
    DB.Results_dir.path_to_filename (DB.Results_dir.Abs_source_dir source) dir_path


  (** Open an Html file to append data *)
  let open_out source path =
    let full_fname = get_full_fname source path in
    let fd =
      Unix.openfile (DB.filename_to_string full_fname) ~mode:Unix.[O_WRONLY; O_APPEND] ~perm:0o777
    in
    let outc = Unix.out_channel_of_descr fd in
    let fmt = F.formatter_of_out_channel outc in
    (fd, fmt)


  (** Return true if the html file was modified since the beginning of the analysis *)
  let modified_during_analysis source path =
    let fname = get_full_fname source path in
    if DB.file_exists fname then DB.file_modified_time fname >= Config.initial_analysis_time
    else false


  (** Close an Html file *)
  let close (fd, fmt) =
    F.fprintf fmt "</body>@\n</html>@." ;
    Unix.close fd


  (** Print a horizontal line *)
  let pp_hline fmt () = F.pp_print_string fmt "<hr width=\"100%\">\n"

  (** Print start color *)
  let pp_start_color fmt color = F.fprintf fmt "<span class='%s'>" (Pp.color_string color)

  (** Print end color *)
  let pp_end_color fmt () = F.pp_print_string fmt "</span>"

  let pp_link ?(name= None) ?(pos= None) ~path fmt text =
    let link_str =
      let escaped_path = List.map ~f:Escape.escape_url path in
      DB.filename_to_string (DB.Results_dir.path_to_filename DB.Results_dir.Rel escaped_path)
    in
    let pp_name fmt = Option.iter ~f:(F.fprintf fmt "name=\"%s\" ") in
    let pp_pos fmt = Option.iter ~f:(F.fprintf fmt "#%s") in
    F.fprintf fmt "<a %ahref=\"%s.html%a\">%s</a>" pp_name name link_str pp_pos pos text


  (** File name for the node, given the procedure name and node id *)
  let node_filename pname id = F.sprintf "%s_node%d" (Typ.Procname.to_filename pname) id

  (** Print an html link to the given node. *)
  let pp_node_link path_to_root pname ~description ~preds ~succs ~exn ~isvisited ~isproof fmt id =
    let node_fname = node_filename pname id in
    let node_text =
      let descr = if String.equal description "" then "N" else String.prefix description 1 in
      let style_class =
        if not isvisited then "dangling" else if isproof then "visitedproof" else "visited"
      in
      F.asprintf
        "<span class='%s'>%s_%d<span class='expansion'>node%d preds:%a succs:%a exn:%a \
         %s%s</span></span>"
        style_class descr id id (Pp.seq F.pp_print_int) preds (Pp.seq F.pp_print_int) succs
        (Pp.seq F.pp_print_int) exn description
        (if not isvisited then "\nNOT VISITED" else "")
    in
    pp_link ~path:(path_to_root @ ["nodes"; node_fname]) fmt node_text


  (** Print an html link to the given proc *)
  let pp_proc_link path_to_root proc_name fmt text =
    pp_link ~path:(path_to_root @ [Typ.Procname.to_filename proc_name]) fmt text


  (** Print an html link to the given line number of the current source file *)
  let pp_line_link ?(with_name= false) ?(text= None) source path_to_root fmt linenum =
    let fname = DB.source_file_encoding source in
    let linenum_str = string_of_int linenum in
    let name = "LINE" ^ linenum_str in
    pp_link
      ~name:(if with_name then Some name else None)
      ~pos:(Some name)
      ~path:(path_to_root @ [".."; fname])
      fmt
      (match text with Some s -> s | None -> linenum_str)


  (** Print an html link given node id and session *)
  let pp_session_link ?(with_name= false) ?proc_name source path_to_root fmt
      (node_id, session, linenum) =
    let node_name = "node" ^ string_of_int node_id in
    let pos = "session" ^ string_of_int session in
    let text = F.sprintf "%s#%s" node_name pos in
    let path_to_node =
      let node_fname =
        match proc_name with Some pname -> node_filename pname node_id | None -> node_name
      in
      path_to_root @ ["nodes"; node_fname]
    in
    pp_link
      ~name:(if with_name then Some pos else None)
      ~pos:(Some pos) ~path:path_to_node fmt text ;
    F.fprintf fmt "(%a)" (pp_line_link source path_to_root) linenum
end

(* =============== END of module Html =============== *)
(* =============== START of module Xml =============== *)

(** Create and print xml trees *)
module Xml = struct
  let tag_err = "err"

  let tag_file = "file"

  let tag_in_calls = "in_calls"

  let tag_line = "line"

  let tag_loc = "loc"

  let tag_name = "name"

  let tag_name_id = "name_id"

  let tag_out_calls = "out_calls"

  let tag_proof_coverage = "proof_coverage"

  let tag_proof_trace = "proof_trace"

  let tag_rank = "rank"

  let tag_signature = "signature"

  let tag_specs = "specs"

  let tag_symop = "symop"

  let tag_time = "time"

  let tag_to = "to"

  let tag_top = "top"

  let tag_weight = "weight"
end

(* =============== END of module Xml =============== *)
