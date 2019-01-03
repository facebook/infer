(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl

(** Printers for the analysis results *)

module L = Logging
module F = Format

(** Module to read specific lines from files.
    The data from any file will stay in memory until the handle is collected by the gc. *)
module LineReader = struct
  (** Map a file name to an array of string, one for each line in the file. *)
  type t = (SourceFile.t, string array) Hashtbl.t

  let create () = Hashtbl.create 1

  let read_file fname =
    let cin = In_channel.create fname in
    let lines = ref [] in
    try
      while true do
        let line_raw = In_channel.input_line_exn cin in
        let line =
          let len = String.length line_raw in
          if len > 0 && Char.equal line_raw.[len - 1] '\r' then
            String.sub line_raw ~pos:0 ~len:(len - 1)
          else line_raw
        in
        lines := line :: !lines
      done ;
      assert false (* execution never reaches here *)
    with End_of_file ->
      In_channel.close cin ;
      Array.of_list (List.rev !lines)


  let file_data (hash : t) fname =
    try Some (Hashtbl.find hash fname) with Caml.Not_found -> (
      try
        let lines_arr = read_file (SourceFile.to_abs_path fname) in
        Hashtbl.add hash fname lines_arr ; Some lines_arr
      with exn when SymOp.exn_not_failure exn -> None )


  let from_file_linenum_original hash fname linenum =
    match file_data hash fname with
    | None ->
        None
    | Some lines_arr ->
        if linenum > 0 && linenum <= Array.length lines_arr then Some lines_arr.(linenum - 1)
        else None


  let from_file_linenum hash fname linenum = from_file_linenum_original hash fname linenum

  let from_loc hash loc = from_file_linenum hash loc.Location.file loc.Location.line
end

(** Current formatter for the html output *)
let curr_html_formatter = ref F.std_formatter

(** Return true if the node was visited during analysis *)
let is_visited node =
  match Summary.get (Procdesc.Node.get_proc_name node) with
  | None ->
      false
  | Some summary ->
      let stats = summary.Summary.stats in
      let node_id = (Procdesc.Node.get_id node :> int) in
      Summary.Stats.is_visited stats node_id


let compare_node =
  let key node = (Procdesc.Node.get_wto_index node, Procdesc.Node.get_id node) in
  fun node1 node2 -> [%compare: int * Procdesc.Node.id] (key node1) (key node2)


let pp_node_link_seq path_to_root ~description fmt nodes =
  let nodes = List.sort nodes ~compare:compare_node in
  let pp_one fmt node =
    let description =
      if description then Procdesc.Node.get_description (Pp.html Black) node else ""
    in
    let pname = Procdesc.Node.get_proc_name node in
    Io_infer.Html.pp_node_link path_to_root pname ~description
      ~preds:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_preds node) :> int list)
      ~succs:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_succs node) :> int list)
      ~exn:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_exn node) :> int list)
      ~isvisited:(is_visited node) fmt
      (Procdesc.Node.get_id node :> int)
  in
  Pp.seq pp_one fmt nodes


(* =============== START of module NodesHtml =============== *)

(** Print information into html files for nodes
    when starting and finishing the processing of a node *)
module NodesHtml : sig
  val start_node :
       int
    -> Location.t
    -> Typ.Procname.t
    -> Procdesc.Node.t list
    -> Procdesc.Node.t list
    -> Procdesc.Node.t list
    -> Instrs.not_reversed_t
    -> SourceFile.t
    -> unit

  val finish_node : Typ.Procname.t -> int -> SourceFile.t -> unit
end = struct
  let log_files = Hashtbl.create 11

  let pp_node_link_seq fmt node = pp_node_link_seq [".."] ~description:false fmt node

  let start_node nodeid loc proc_name preds succs exns instrs source =
    let node_fname = Io_infer.Html.node_filename proc_name nodeid in
    let node_path = ["nodes"; node_fname] in
    let modified = Io_infer.Html.modified_during_analysis source node_path in
    let needs_initialization, (fd, fmt) =
      if modified then (false, Io_infer.Html.open_out source node_path)
      else (true, Io_infer.Html.create source node_path)
    in
    curr_html_formatter := fmt ;
    Hashtbl.replace log_files (node_fname, source) fd ;
    if needs_initialization then (
      F.fprintf fmt "<center><h1>Cfg Node %a</h1></center>"
        (Io_infer.Html.pp_line_link source ~text:(Some (string_of_int nodeid)) [".."])
        loc.Location.line ;
      F.fprintf fmt "PROC: %a LINE: %a@\n"
        (Io_infer.Html.pp_proc_link [".."] proc_name)
        (Escape.escape_xml (Typ.Procname.to_string proc_name))
        (Io_infer.Html.pp_line_link source [".."])
        loc.Location.line ;
      F.fprintf fmt "<br>PREDS:@\n" ;
      pp_node_link_seq fmt preds ;
      F.fprintf fmt "<br>SUCCS:@\n" ;
      pp_node_link_seq fmt succs ;
      F.fprintf fmt "<br>EXN:@\n" ;
      pp_node_link_seq fmt exns ;
      F.fprintf fmt "<br>@\n" ;
      F.fprintf fmt "<LISTING class='%s'>%a</LISTING>" (Pp.color_string Green)
        (Instrs.pp (Pp.html Green))
        instrs )


  let finish_node proc_name nodeid source =
    let node_fname = Io_infer.Html.node_filename proc_name nodeid in
    let fd = Hashtbl.find log_files (node_fname, source) in
    Unix.close fd ;
    curr_html_formatter := F.std_formatter
end

(* =============== END of module NodesHtml =============== *)
(* =============== Printing functions =============== *)

(** Execute the delayed print actions *)
let force_delayed_prints () =
  F.pp_print_flush !curr_html_formatter () ;
  (* flush html stream *)
  L.force_and_reset_delayed_prints !curr_html_formatter ;
  F.pp_print_flush !curr_html_formatter ()


(** Start a session, and create a new html file for the node if it does not exist yet *)
let start_session ~pp_name node (loc : Location.t) proc_name session source =
  let node_id = Procdesc.Node.get_id node in
  NodesHtml.start_node
    (node_id :> int)
    loc proc_name (Procdesc.Node.get_preds node) (Procdesc.Node.get_succs node)
    (Procdesc.Node.get_exn node) (Procdesc.Node.get_instrs node) source ;
  F.fprintf !curr_html_formatter "%a%a %t" Io_infer.Html.pp_hline ()
    (Io_infer.Html.pp_session_link source ~with_name:true [".."] ~proc_name)
    ((node_id :> int), session, loc.Location.line)
    pp_name ;
  F.fprintf !curr_html_formatter "@\n<LISTING class='%s'>" (Pp.color_string Black)


let node_start_session ~pp_name node session =
  if Config.write_html then
    let loc = Procdesc.Node.get_loc node in
    let source = loc.Location.file in
    let pname = Procdesc.Node.get_proc_name node in
    start_session ~pp_name node loc pname session source


(** Finish a session, and perform delayed print actions if required *)
let node_finish_session node =
  if not Config.only_cheap_debug then force_delayed_prints () else L.reset_delayed_prints () ;
  if Config.write_html then (
    F.fprintf !curr_html_formatter "</LISTING>@?" ;
    let source = (Procdesc.Node.get_loc node).file in
    NodesHtml.finish_node
      (Procdesc.Node.get_proc_name node)
      (Procdesc.Node.get_id node :> int)
      source )


(** Write html file for the procedure. *)
let write_proc_html pdesc =
  if Config.write_html then (
    let pname = Procdesc.get_proc_name pdesc in
    let source = (Procdesc.get_loc pdesc).file in
    let nodes = List.sort ~compare:Procdesc.Node.compare (Procdesc.get_nodes pdesc) in
    let linenum = (Procdesc.Node.get_loc (List.hd_exn nodes)).Location.line in
    let fd, fmt = Io_infer.Html.create source [Typ.Procname.to_filename pname] in
    F.fprintf fmt "<center><h1>Procedure %a</h1></center>@\n"
      (Io_infer.Html.pp_line_link source
         ~text:(Some (Escape.escape_xml (Typ.Procname.to_string pname)))
         [])
      linenum ;
    pp_node_link_seq [] ~description:true fmt nodes ;
    ( match Summary.get pname with
    | None ->
        ()
    | Some summary ->
        F.pp_print_string fmt "<br />@\n" ;
        Summary.pp_html source fmt summary ) ;
    F.fprintf fmt "<hr />@\n<pre>@\n%a</pre>@\n" ProcAttributes.pp (Procdesc.get_attributes pdesc) ;
    Io_infer.Html.close (fd, fmt) )


(** Creare a hash table mapping line numbers to the set of errors occurring on that line *)
let create_table_err_per_line err_log =
  let err_per_line = Hashtbl.create 17 in
  let add_err (key : Errlog.err_key) (err_data : Errlog.err_data) =
    let err_str =
      F.asprintf "%s %a" key.err_name.IssueType.unique_id Localise.pp_error_desc key.err_desc
    in
    try
      let set = Hashtbl.find err_per_line err_data.loc.Location.line in
      Hashtbl.replace err_per_line err_data.loc.Location.line (String.Set.add set err_str)
    with Caml.Not_found ->
      Hashtbl.add err_per_line err_data.loc.Location.line (String.Set.singleton err_str)
  in
  Errlog.iter add_err err_log ; err_per_line


(** Create error message for html file *)
let pp_err_message fmt err_string =
  F.fprintf fmt "\n<div class=\"msg\" style=\"margin-left:9ex\">%s</div>" err_string


let write_html_proc source table_nodes_at_linenum global_err_log proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let _ = (* Initializes wto_indexes *) Procdesc.get_wto proc_desc in
  let process_node n =
    let lnum = (Procdesc.Node.get_loc n).Location.line in
    let curr_nodes = try Hashtbl.find table_nodes_at_linenum lnum with Caml.Not_found -> [] in
    Hashtbl.replace table_nodes_at_linenum lnum (n :: curr_nodes)
  in
  let proc_loc = Procdesc.get_loc proc_desc in
  let process_proc =
    Procdesc.is_defined proc_desc
    && SourceFile.equal proc_loc.Location.file source
    &&
    match Attributes.find_file_capturing_procedure proc_name with
    | None ->
        true
    | Some (source_captured, _) ->
        SourceFile.equal source_captured (Procdesc.get_loc proc_desc).file
  in
  if process_proc then (
    List.iter ~f:process_node (Procdesc.get_nodes proc_desc) ;
    match Summary.get proc_name with
    | None ->
        ()
    | Some summary ->
        Errlog.update global_err_log (Summary.get_err_log summary) )


(** Create filename.ext.html. *)
let write_html_file linereader filename procs =
  let fname_encoding = DB.source_file_encoding filename in
  let fd, fmt = Io_infer.Html.create filename [".."; fname_encoding] in
  let pp_prelude () =
    F.fprintf fmt "<center><h1>File %a </h1></center>@\n<table class=\"code\">@\n" SourceFile.pp
      filename
  in
  let print_one_line table_nodes_at_linenum table_err_per_line line_number =
    let line_html =
      match LineReader.from_file_linenum linereader filename line_number with
      | Some line_raw ->
          Escape.escape_xml line_raw
      | None ->
          raise End_of_file
    in
    let errors_at_linenum =
      try
        let errset = Hashtbl.find table_err_per_line line_number in
        String.Set.elements errset
      with Caml.Not_found -> []
    in
    F.fprintf fmt "<tr><td class=\"num\" id=\"LINE%d\">%d</td><td class=\"line\">%s " line_number
      line_number line_html ;
    ( match Hashtbl.find table_nodes_at_linenum line_number with
    | nodes_at_linenum ->
        pp_node_link_seq [fname_encoding] ~description:true fmt nodes_at_linenum ;
        List.iter
          ~f:(fun n ->
            match Procdesc.Node.get_kind n with
            | Procdesc.Node.Start_node ->
                let proc_name = Procdesc.Node.get_proc_name n in
                let num_specs =
                  match Summary.get proc_name with
                  | None ->
                      0
                  | Some summary ->
                      List.length (Tabulation.get_specs_from_payload summary)
                in
                let label =
                  F.sprintf "%s: %d specs"
                    (Escape.escape_xml (Typ.Procname.to_string proc_name))
                    num_specs
                in
                F.pp_print_char fmt ' ' ;
                Io_infer.Html.pp_proc_link [fname_encoding] proc_name fmt label
            | _ ->
                () )
          nodes_at_linenum
    | exception Caml.Not_found ->
        () ) ;
    List.iter ~f:(pp_err_message fmt) errors_at_linenum ;
    F.fprintf fmt "</td></tr>@\n"
  in
  pp_prelude () ;
  let global_err_log = Errlog.empty () in
  let table_nodes_at_linenum = Hashtbl.create 11 in
  List.iter ~f:(write_html_proc filename table_nodes_at_linenum global_err_log) procs ;
  let table_err_per_line = create_table_err_per_line global_err_log in
  let linenum = ref 0 in
  try
    while true do
      incr linenum ;
      print_one_line table_nodes_at_linenum table_err_per_line !linenum
    done
  with End_of_file ->
    F.fprintf fmt "</table>@\n" ;
    Errlog.pp_html filename [fname_encoding] fmt global_err_log ;
    Io_infer.Html.close (fd, fmt)


(** Create the HTML debug file for the source file. *)
let write_all_html_files source_file =
  let opt_whitelist_regex =
    match Config.write_html_whitelist_regex with
    | [] ->
        None
    | _ as reg_list ->
        Some (Str.regexp (String.concat ~sep:"\\|" reg_list))
  in
  let is_whitelisted file =
    Option.value_map opt_whitelist_regex ~default:true ~f:(fun regex ->
        let fname = SourceFile.to_rel_path file in
        Str.string_match regex fname 0 )
  in
  let linereader = LineReader.create () in
  let procs_in_source = SourceFiles.proc_names_of_source source_file in
  let source_files_in_cfg, pdescs_in_cfg =
    List.fold procs_in_source ~init:(SourceFile.Set.empty, [])
      ~f:(fun ((files, pdescs) as acc) proc_name ->
        match Procdesc.load proc_name with
        | Some proc_desc ->
            let updated_files =
              if Procdesc.is_defined proc_desc then
                let file = (Procdesc.get_loc proc_desc).Location.file in
                if is_whitelisted file then SourceFile.Set.add file files else files
              else files
            in
            (updated_files, proc_desc :: pdescs)
        | None ->
            acc )
  in
  SourceFile.Set.iter
    (fun file -> write_html_file linereader file pdescs_in_cfg)
    source_files_in_cfg
