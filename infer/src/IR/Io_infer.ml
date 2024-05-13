(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module to handle IO. Includes html and xml modules. *)

module F = Format
module L = Logging

(* =============== START of module Html =============== *)
module Html = struct
  (** Create a new html file *)
  let create source path =
    if SourceFile.is_invalid source then
      L.debug Capture Verbose "Invalid source. (Did you forget to create a start/exit node?)" ;
    let fname, dir_path =
      match List.rev path with
      | fname :: path_rev ->
          (fname, List.rev ((fname ^ ".html") :: path_rev))
      | [] ->
          raise (Failure "Html.create")
    in
    let fd = DB.Results_dir.(create_file (Abs_source_dir source)) dir_path in
    let outc = Unix.out_channel_of_descr fd in
    let fmt = F.formatter_of_out_channel outc in
    let script =
      {|
<script>
var detailsOpen = false;
function toggleDetailsBlock() {
  detailsOpen = !detailsOpen;
  var details = document.querySelectorAll("details");
  details.forEach(d => {
    d.open = detailsOpen;
  });
  return detailsOpen;
}

function toggleListingOnTop() {
  var sticky_class = "sticky_header";
  var listing = document.getElementById("node_listing");
  if (listing.classList.contains(sticky_class)) {
    listing.classList.remove(sticky_class);
  } else {
    listing.classList.add(sticky_class);
  }
}
       
function toggleListingVisibility() {
  var listing = document.querySelector("#node_listing > listing");
  if (listing.style.display == "none") {
  listing.style.display = "";
  } else {
    listing.style.display = "none";
  }
}
</script>
|}
    in
    let style =
      {|
<style type="text/css">
body { background-color:#fff; color:#000; font-family:Helvetica, sans-serif; font-size:10pt }
h1 { font-size:14pt }
.code { border-collapse:collapse; width:100%; font-family: "Andale Mono", monospace; font-size:10pt; line-height: 1.2em }
.color_black { color: black }
.color_blue { color: blue }
.color_green { color: green }
.color_orange { color: orange }
.color_red { color: red }
.line { border-left: 3px solid #ccc; padding-left: 1ex; white-space: pre; }
.msg { background-color:#fff8b4; color:#000; float:left; font-family:Helvetica, sans-serif; font-size: smaller; font-weight: bold; margin-bottom:10px; margin-top:10px; max-width:60em; padding:0.5em 1ex 0.5em 1ex; -webkit-border-radius:5px; -webkit-box-shadow:1px 1px 7px #000; white-space: pre-wrap; word-wrap: break-word; }
.num { background-color:#eee; color:#444; font-size: smaller; padding-right:2ex; text-align:right; width:2.5em; }
.dangling { background-color:white; color: gray; }
.visited { background-color:LemonChiffon; color: darkmagenta; }
.tooltip { display: none; background-color:#FFF0F0; border: 2px solid #F00; font-weight: normal; left:3em; padding: 2px; position: absolute; top: -1em; -webkit-border-radius:5px; -webkit-box-shadow:1px 1px 7px #000; z-index: 1}
.with_tooltip { position: relative; }
.with_tooltip:hover .tooltip, .visited:hover .tooltip { display: block; }
#node_listing { margin-top: 5pt; margin-bottom: 5pt; }
.sticky_header { position: fixed; top: 0; width: 100%; background-color: #eeeee4; }         
details { padding-left: 20pt; }
summary { margin-left: -20pt; }
.d_with_indent { padding-left: 20pt; }
.d_with_indent_name { margin-left: -20pt; }
.has_hover:hover { background-color:LemonChiffon; }
.pulse_invalid { color:red; }
</style>
|}
    in
    let page =
      Printf.sprintf
        {|
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
<title>%s</title>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
%s
%s
</head>
<body>
|}
        fname script style
    in
    F.pp_print_string fmt page ;
    (fd, fmt)


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
      Unix.openfile (DB.filename_to_string full_fname) ~mode:[O_WRONLY; O_APPEND] ~perm:0o777
    in
    let outc = Unix.out_channel_of_descr fd in
    let fmt = F.formatter_of_out_channel outc in
    (fd, fmt)


  (** Return true if the html file was modified since the beginning of the analysis *)
  let modified_during_analysis source path =
    let fname = get_full_fname source path in
    if DB.file_exists fname then Float.(DB.file_modified_time fname >= Config.initial_analysis_time)
    else false


  (** Close an Html file *)
  let close (fd, fmt) =
    F.fprintf fmt "</body>@\n</html>@." ;
    Unix.close fd


  (** Print a horizontal line *)
  let pp_hline fmt () = F.pp_print_string fmt "\n<hr width=\"100%\">\n"

  let pp_link ?(name = None) ?(pos = None) ~path fmt text =
    let link_str =
      let escaped_path = List.map ~f:Escape.escape_url path in
      DB.filename_to_string (DB.Results_dir.path_to_filename DB.Results_dir.Rel escaped_path)
    in
    let pp_name fmt = Option.iter ~f:(F.fprintf fmt "name=\"%s\" ") in
    let pp_pos fmt = Option.iter ~f:(F.fprintf fmt "#%s") in
    F.fprintf fmt "<a %ahref=\"%s.html%a\">%s</a>" pp_name name link_str pp_pos pos text


  (** File name for the node, given the procedure name and node id *)
  let node_filename pname id = F.sprintf "%s_node%d" (Procname.to_filename pname) id

  (** Print an html link to the given node. *)
  let pp_node_link path_to_root pname ~description ~preds ~succs ~exn ~isvisited fmt id =
    let node_fname = node_filename pname id in
    let node_text =
      let descr = if String.equal description "" then "N" else String.prefix description 1 in
      let style_class = if not isvisited then "dangling" else "visited" in
      F.asprintf
        "<span class='%s with_tooltip'>%s_%d<span class='tooltip'>node%d preds:%a succs:%a exn:%a \
         %s%s</span></span>"
        style_class descr id id (Pp.seq F.pp_print_int) preds (Pp.seq F.pp_print_int) succs
        (Pp.seq F.pp_print_int) exn description
        (if not isvisited then "\nNOT VISITED" else "")
    in
    pp_link ~path:(path_to_root @ ["nodes"; node_fname]) fmt node_text


  (** Print an html link to the given proc *)
  let pp_proc_link path_to_root proc_name fmt text =
    pp_link ~path:(path_to_root @ [Procname.to_filename proc_name]) fmt text


  (** Print an html link to the given line number of the current source file *)
  let pp_line_link ?(with_name = false) ?(text = None) source path_to_root fmt linenum =
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
  let pp_session_link ?(with_name = false) ?proc_name source path_to_root fmt
      (node_id, session, linenum) =
    let node_name = "node" ^ string_of_int node_id in
    let text, pos =
      if session > 0 then
        let pos = "session" ^ string_of_int session in
        let text = F.sprintf "%s#%s" node_name pos in
        (text, Some pos)
      else (node_name, None)
    in
    let path_to_node =
      let node_fname =
        match proc_name with Some pname -> node_filename pname node_id | None -> node_name
      in
      path_to_root @ ["nodes"; node_fname]
    in
    pp_link ~name:(if with_name then pos else None) ~pos ~path:path_to_node fmt text ;
    F.fprintf fmt "(%a)" (pp_line_link source path_to_root) linenum
end

(* =============== END of module Html =============== *)
