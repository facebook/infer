(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl

(** Printers for the analysis results *)

module L = Logging
module F = Format

(** Current formatter for the html output *)
let curr_html_formatter = ref F.std_formatter

let () = AnalysisGlobalState.register_ref ~init:(fun () -> F.std_formatter) curr_html_formatter

(** Return true if the node was visited during analysis *)
let is_visited node =
  match
    Summary.OnDisk.get ~lazy_payloads:true AnalysisRequest.all (Procdesc.Node.get_proc_name node)
  with
  | None ->
      false
  | Some summary ->
      let stats = summary.Summary.stats in
      let node_id = (Procdesc.Node.get_id node :> int) in
      Summary.Stats.is_visited stats node_id


let pp_node_link_seq =
  let compare_node =
    let key node = (Procdesc.Node.get_wto_index node, Procdesc.Node.get_id node) in
    fun node1 node2 -> [%compare: int * Procdesc.Node.id] (key node1) (key node2)
  in
  fun path_to_root ~description fmt nodes ->
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


(** Print information into html files for nodes when starting and finishing the processing of a node *)
module NodesHtml : sig
  val start_session : pp_name:(Format.formatter -> unit) -> Procdesc.Node.t -> int -> unit

  val finish_session : Procdesc.Node.t -> unit
end = struct
  let log_files = Hashtbl.create 11

  let pp_node_link_seq fmt node = pp_node_link_seq [".."] ~description:false fmt node

  let start_session ~pp_name node session =
    let loc = Procdesc.Node.get_loc node in
    let source = loc.Location.file in
    let line = loc.Location.line in
    let proc_name = Procdesc.Node.get_proc_name node in
    let nodeid = (Procdesc.Node.get_id node :> int) in
    let node_fname = Io_infer.Html.node_filename proc_name nodeid in
    let needs_initialization, (fd, fmt) =
      let node_path = ["nodes"; node_fname] in
      let modified = Io_infer.Html.modified_during_analysis source node_path in
      if modified then (false, Io_infer.Html.open_out source node_path)
      else (true, Io_infer.Html.create source node_path)
    in
    curr_html_formatter := fmt ;
    Hashtbl.replace log_files (node_fname, source) fd ;
    if needs_initialization then (
      F.fprintf fmt "<center><h1>Cfg Node %a</h1></center>"
        (Io_infer.Html.pp_line_link source ~text:(Some (string_of_int nodeid)) [".."])
        line ;
      F.fprintf fmt "PROC: %a LINE: %a@\n"
        (Io_infer.Html.pp_proc_link [".."] proc_name)
        (Escape.escape_xml (Procname.to_string ~verbosity:Verbose proc_name))
        (Io_infer.Html.pp_line_link source [".."])
        line ;
      F.fprintf fmt "<br>PREDS:@\n" ;
      pp_node_link_seq fmt (Procdesc.Node.get_preds node) ;
      F.fprintf fmt "<br>SUCCS:@\n" ;
      pp_node_link_seq fmt (Procdesc.Node.get_succs node) ;
      F.fprintf fmt "<br>EXN:@\n" ;
      pp_node_link_seq fmt (Procdesc.Node.get_exn node) ;
      F.fprintf fmt "<br>@\n" ;
      (* Instruction listing + buttons to control stickiness/visibility *)
      F.fprintf fmt "<DIV id='node_listing'>@\n" ;
      F.fprintf fmt
        "<BUTTON type='button' onclick='toggleListingOnTop()'>Listing on top</BUTTON>@\n" ;
      F.fprintf fmt
        "<BUTTON type='button' onclick='toggleListingVisibility()'>Listing visibility</BUTTON>@\n" ;
      F.fprintf fmt "<LISTING class='%s'>%a</LISTING>@\n" (Pp.color_string Green)
        (Instrs.pp ~indent:false (Pp.html Green))
        (Procdesc.Node.get_instrs node) ;
      F.fprintf fmt "</DIV>@\n" ;
      (* Listing end *)
      F.fprintf fmt "<BUTTON type='button' onclick='toggleDetailsBlock()'>Toggle details</BUTTON>" ) ;
    F.fprintf fmt "%a%a %t" Io_infer.Html.pp_hline ()
      (Io_infer.Html.pp_session_link source ~with_name:true [".."] ~proc_name)
      (nodeid, session, line) pp_name ;
    F.fprintf fmt "@\n<LISTING class='%s'>" (Pp.color_string Black)


  let finish_session node =
    F.fprintf !curr_html_formatter "</LISTING>@?" ;
    let fd =
      let source = (Procdesc.Node.get_loc node).file in
      let node_fname =
        let proc_name = Procdesc.Node.get_proc_name node in
        let nodeid = (Procdesc.Node.get_id node :> int) in
        Io_infer.Html.node_filename proc_name nodeid
      in
      Hashtbl.find log_files (node_fname, source)
    in
    Unix.close fd ;
    curr_html_formatter := F.std_formatter
end

module ProcsHtml : sig
  val write : Procdesc.t -> unit
end = struct
  let write pdesc =
    let pname = Procdesc.get_proc_name pdesc in
    let loc = Procdesc.get_loc pdesc in
    let source = loc.file in
    let nodes = List.sort ~compare:Procdesc.Node.compare (Procdesc.get_nodes pdesc) in
    let linenum = loc.Location.line in
    let fd, fmt = Io_infer.Html.create source [Procname.to_filename pname] in
    F.fprintf fmt "<center><h1>Procedure %a</h1></center>@\n"
      (Io_infer.Html.pp_line_link source
         ~text:(Some (Escape.escape_xml (Procname.to_string ~verbosity:Verbose pname)))
         [] )
      linenum ;
    pp_node_link_seq [] ~description:true fmt nodes ;
    (* load payloads eagerly as we need them to print them all *)
    ( match Summary.OnDisk.get ~lazy_payloads:false AnalysisRequest.all pname with
    | None ->
        ()
    | Some summary ->
        F.pp_print_string fmt "<br />@\n" ;
        Summary.pp_html source fmt summary ) ;
    F.fprintf fmt "<hr />@\n<pre>@\n%s</pre>@\n"
      (Escape.escape_xml (F.asprintf "%a" ProcAttributes.pp (Procdesc.get_attributes pdesc))) ;
    Io_infer.Html.close (fd, fmt)
end

module FilesHtml : sig
  val write_all_html_files : SourceFile.t -> unit

  val ensure_file_is_written : Procdesc.Node.t -> unit
end = struct
  (* Only used in debug html mode *)
  let linereader = LineReader.create ()

  (** Create a hash table mapping line numbers to the set of errors occurring on that line *)
  let create_table_err_per_line err_log =
    let err_per_line = Hashtbl.create 17 in
    let add_err (key : Errlog.err_key) (err_data : Errlog.err_data) =
      let err_str =
        F.asprintf "%s %a" key.issue_type.unique_id Localise.pp_error_desc key.err_desc
      in
      try
        let set = Hashtbl.find err_per_line err_data.loc.Location.line in
        Hashtbl.replace err_per_line err_data.loc.Location.line (String.Set.add set err_str)
      with Caml.Not_found ->
        Hashtbl.add err_per_line err_data.loc.Location.line (String.Set.singleton err_str)
    in
    Errlog.iter add_err err_log ;
    err_per_line


  (** Create error message for html file *)
  let pp_err_message fmt err_string =
    F.fprintf fmt "\n<div class=\"msg\" style=\"margin-left:9ex\">%s</div>" err_string


  let process_proc table_nodes_at_linenum global_err_log proc_desc =
    let proc_name = Procdesc.get_proc_name proc_desc in
    Procdesc.init_wto proc_desc ;
    let process_node n =
      let lnum = (Procdesc.Node.get_loc n).Location.line in
      let curr_nodes = try Hashtbl.find table_nodes_at_linenum lnum with Caml.Not_found -> [] in
      Hashtbl.replace table_nodes_at_linenum lnum (n :: curr_nodes)
    in
    List.iter ~f:process_node (Procdesc.get_nodes proc_desc) ;
    match Summary.OnDisk.get ~lazy_payloads:true AnalysisRequest.all proc_name with
    | None ->
        ()
    | Some {err_log} ->
        Errlog.update global_err_log err_log


  let write_html_file filename procs =
    let fname_encoding = DB.source_file_encoding filename in
    let fd, fmt = Io_infer.Html.create filename [".."; fname_encoding] in
    F.fprintf fmt "<center><h1>File %a </h1></center>@\n<table class=\"code\">@\n" SourceFile.pp
      filename ;
    let global_err_log = Errlog.empty () in
    let table_nodes_at_linenum = Hashtbl.create 11 in
    List.iter ~f:(process_proc table_nodes_at_linenum global_err_log) procs ;
    let table_err_per_line = create_table_err_per_line global_err_log in
    let print_one_line line_number line_raw =
      let line_html = Escape.escape_xml line_raw in
      F.fprintf fmt "<tr><td class=\"num\" id=\"LINE%d\">%d</td><td class=\"line\">%s " line_number
        line_number line_html ;
      ( match Hashtbl.find table_nodes_at_linenum line_number with
      | nodes_at_linenum ->
          pp_node_link_seq [fname_encoding] ~description:true fmt nodes_at_linenum ;
          List.iter nodes_at_linenum ~f:(fun n ->
              match Procdesc.Node.get_kind n with
              | Procdesc.Node.Start_node ->
                  let proc_name = Procdesc.Node.get_proc_name n in
                  let proc_name_escaped =
                    Escape.escape_xml (Procname.to_string ~verbosity:Verbose proc_name)
                  in
                  if
                    Summary.OnDisk.get ~lazy_payloads:true AnalysisRequest.all proc_name
                    |> Option.is_some
                  then (
                    F.pp_print_char fmt ' ' ;
                    let label = F.asprintf "summary for %s" proc_name_escaped in
                    Io_infer.Html.pp_proc_link [fname_encoding] proc_name fmt label )
                  else F.fprintf fmt "no summary for %s" proc_name_escaped
              | _ ->
                  () )
      | exception Caml.Not_found ->
          () ) ;
      ( match Hashtbl.find table_err_per_line line_number with
      | errset ->
          String.Set.iter errset ~f:(pp_err_message fmt)
      | exception Caml.Not_found ->
          () ) ;
      F.fprintf fmt "</td></tr>@\n"
    in
    LineReader.iteri linereader filename ~f:print_one_line ;
    F.fprintf fmt "</table>@\n" ;
    Errlog.pp_html filename [fname_encoding] fmt global_err_log ;
    Io_infer.Html.close (fd, fmt)


  let is_allow_listed =
    match Config.write_html_allow_list_regex with
    | [] ->
        fun _ -> true
    | _ as reg_list ->
        let regex = Str.regexp (String.concat ~sep:"\\|" reg_list) in
        fun file ->
          let fname = SourceFile.to_rel_path file in
          Str.string_match regex fname 0


  (*
    Stores all the proc_descs in source files.
    We need to keep collecting them because some may be captured by other files, happens especially
    with templates in header files.
  *)
  let pdescs_in_source = Hashtbl.create 1

  let write_all_html_files source_file =
    let procs_in_source = SourceFiles.proc_names_of_source source_file in
    let source_files_in_cfg =
      List.fold procs_in_source ~init:SourceFile.Set.empty ~f:(fun files proc_name ->
          match Procdesc.load proc_name with
          | Some proc_desc ->
              if Procdesc.is_defined proc_desc then
                let file = (Procdesc.get_loc proc_desc).Location.file in
                if is_allow_listed file then (
                  let pdescs_in_file =
                    try Hashtbl.find pdescs_in_source file
                    with Caml.Not_found -> Procname.Map.empty
                  in
                  let pdescs_in_file = Procname.Map.add proc_name proc_desc pdescs_in_file in
                  Hashtbl.replace pdescs_in_source file pdescs_in_file ;
                  SourceFile.Set.add file files )
                else files
              else files
          | None ->
              files )
    in
    SourceFile.Set.iter
      (fun file ->
        let pdescs_in_file =
          match Hashtbl.find pdescs_in_source file with
          | pdescs_map ->
              Procname.Map.bindings pdescs_map |> List.map ~f:snd
          | exception Caml.Not_found ->
              []
        in
        DB.Results_dir.init file ;
        write_html_file file pdescs_in_file )
      source_files_in_cfg


  let ensure_file_is_written =
    let written_files = Hashtbl.create 1 in
    fun node ->
      let file = (Procdesc.Node.get_loc node).Location.file in
      if not (Hashtbl.mem written_files file) then (
        write_all_html_files file ;
        Hashtbl.add written_files file () )
end

(* =============== Printing functions =============== *)

(** Execute the delayed print actions *)
let force_delayed_prints () =
  F.pp_print_flush !curr_html_formatter () ;
  (* flush html stream *)
  L.force_and_reset_delayed_prints !curr_html_formatter ;
  F.pp_print_flush !curr_html_formatter ()


(** Start a session, and create a new html file for the node if it does not exist yet *)
let node_start_session ~pp_name node session =
  if Config.write_html then (
    FilesHtml.ensure_file_is_written node ;
    NodesHtml.start_session ~pp_name node session )


(** Finish a session, and perform delayed print actions if required *)
let node_finish_session node =
  if not Config.only_cheap_debug then force_delayed_prints () else L.reset_delayed_prints () ;
  if Config.write_html then NodesHtml.finish_session node


(** Write html file for the procedure. *)
let write_proc_html pdesc = if Config.write_html then ProcsHtml.write pdesc

(** Create the HTML debug file for the source file. *)
let write_all_html_files = FilesHtml.write_all_html_files
