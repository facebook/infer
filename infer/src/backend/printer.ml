(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Printers for the analysis results *)

module L = Logging
module F = Format

(** Module to read specific lines from files.
    The data from any file will stay in memory until the handle is collected by the gc. *)
module LineReader =
struct
  (** Map a file name to an array of string, one for each line in the file. *)
  type t = (DB.source_file, string array) Hashtbl.t

  let create () =
    Hashtbl.create 1

  let read_file fname =
    let cin = open_in fname in
    let lines = ref [] in
    try
      while true do
        let line_raw = input_line cin in
        let line =
          let len = String.length line_raw in
          if len > 0 && String.get line_raw (len -1) = '\013' then
            String.sub line_raw 0 (len -1)
          else line_raw in
        lines := line :: !lines
      done;
      assert false (* execution never reaches here *)
    with End_of_file ->
      (close_in cin;
       Array.of_list (IList.rev !lines))

  let file_data (hash: t) fname =
    try
      Some (Hashtbl.find hash fname)
    with Not_found ->
    try
      let lines_arr = read_file (DB.source_file_to_string fname) in
      Hashtbl.add hash fname lines_arr;
      Some lines_arr
    with exn when SymOp.exn_not_failure exn -> None

  let from_file_linenum_original hash fname linenum =
    match file_data hash fname with
    | None -> None
    | Some lines_arr ->
        if linenum > 0 && linenum <= Array.length lines_arr
        then Some lines_arr.(linenum -1)
        else None

  let from_file_linenum hash fname linenum =
    let fname_in_resdir =
      DB.source_file_in_resdir fname in
    let sourcefile_in_resdir =
      DB.abs_source_file_from_path (DB.filename_to_string fname_in_resdir) in
    from_file_linenum_original hash sourcefile_in_resdir linenum

  let from_loc hash loc =
    from_file_linenum hash loc.Location.file loc.Location.line
end


(** Current formatter for the html output *)
let curr_html_formatter = ref F.std_formatter

(** Return true if the node was visited during footprint and during re-execution*)
let node_is_visited node =
  let proc_desc = Cfg.Node.get_proc_desc node in
  let proc_name = Cfg.Procdesc.get_proc_name proc_desc in
  match Specs.get_summary proc_name with
  | None ->
      false, false
  | Some summary ->
      let stats = summary.Specs.stats in
      let is_visited_fp =
        IntSet.mem (Cfg.Node.get_id node :> int) stats.Specs.nodes_visited_fp in
      let is_visited_re =
        IntSet.mem (Cfg.Node.get_id node :> int) stats.Specs.nodes_visited_re in
      is_visited_fp, is_visited_re

(** Return true if the node was visited during analysis *)
let is_visited node =
  let visited_fp, visited_re = node_is_visited node in
  visited_fp || visited_re

(* =============== START of module NodesHtml =============== *)

(** Print information into html files for nodes
    when starting and finishing the processing of a node *)
module NodesHtml : sig
  val start_node :
    int -> Location.t -> Procname.t -> Cfg.node list -> Cfg.node list -> Cfg.node list -> bool
  val finish_node : int -> unit
end = struct
  let log_files = Hashtbl.create 11

  let id_to_fname id = "node" ^ string_of_int id

  let start_node nodeid loc proc_name preds succs exn =
    let node_fname = id_to_fname nodeid in
    let modified = Io_infer.Html.modified_during_analysis ["nodes"; node_fname] in
    let needs_initialization, (fd, fmt) =
      if modified then
        (false, Io_infer.Html.open_out ["nodes"; node_fname])
      else
        (true, Io_infer.Html.create DB.Results_dir.Abs_source_dir ["nodes"; node_fname]) in
    curr_html_formatter := fmt;
    Hashtbl.replace log_files (node_fname, !DB.current_source) fd;
    if needs_initialization then
      (F.fprintf fmt "<center><h1>Cfg Node %a</h1></center>"
         (Io_infer.Html.pp_line_link ~text: (Some (string_of_int nodeid)) [".."])
         loc.Location.line;
       F.fprintf fmt "PROC: %a LINE:%a\n"
         (Io_infer.Html.pp_proc_link [".."] proc_name)
         (Escape.escape_xml (Procname.to_string proc_name))
         (Io_infer.Html.pp_line_link [".."]) loc.Location.line;
       F.fprintf fmt "<br>PREDS:@\n";
       IList.iter (fun node ->
           Io_infer.Html.pp_node_link [".."] ""
             (IList.map Cfg.Node.get_id (Cfg.Node.get_preds node) :> int list)
             (IList.map Cfg.Node.get_id (Cfg.Node.get_succs node) :> int list)
             (IList.map Cfg.Node.get_id (Cfg.Node.get_exn node) :> int list)
             (is_visited node) false fmt (Cfg.Node.get_id node :> int)) preds;
       F.fprintf fmt "<br>SUCCS: @\n";
       IList.iter (fun node ->
           Io_infer.Html.pp_node_link [".."] ""
             (IList.map Cfg.Node.get_id (Cfg.Node.get_preds node) :> int list)
             (IList.map Cfg.Node.get_id (Cfg.Node.get_succs node) :> int list)
             (IList.map Cfg.Node.get_id (Cfg.Node.get_exn node) :> int list)
             (is_visited node) false fmt (Cfg.Node.get_id node :> int)) succs;
       F.fprintf fmt "<br>EXN: @\n";
       IList.iter (fun node ->
           Io_infer.Html.pp_node_link [".."] ""
             (IList.map Cfg.Node.get_id (Cfg.Node.get_preds node) :> int list)
             (IList.map Cfg.Node.get_id (Cfg.Node.get_succs node) :> int list)
             (IList.map Cfg.Node.get_id (Cfg.Node.get_exn node) :> int list)
             (is_visited node) false fmt (Cfg.Node.get_id node :> int)) exn;
       F.fprintf fmt "<br>@\n";
       F.pp_print_flush fmt ();
       true
      )
    else false

  let finish_node nodeid =
    let fname = id_to_fname nodeid in
    let fd = Hashtbl.find log_files (fname, !DB.current_source) in
    Unix.close fd;
    curr_html_formatter := F.std_formatter
end
(* =============== END of module NodesHtml =============== *)

(* =============== Printing functions =============== *)

(** Execute the delayed print actions *)
let force_delayed_print fmt =
  let pe_default =
    if !Config.write_html then pe_html Black else pe_text in
  function
  | (L.PTatom, a) ->
      let (a: Sil.atom) = Obj.obj a in
      Sil.pp_atom pe_default fmt a
  | (L.PTdecrease_indent, n) ->
      let (n: int) = Obj.obj n in
      for _ = 1 to n do F.fprintf fmt "@]" done
  | (L.PTexp, e) ->
      let (e: Sil.exp) = Obj.obj e in
      Sil.pp_exp pe_default fmt e
  | (L.PTexp_list, el) ->
      let (el: Sil.exp list) = Obj.obj el in
      Sil.pp_exp_list pe_default fmt el
  | (L.PThpred, hpred) ->
      let (hpred: Sil.hpred) = Obj.obj hpred in
      Sil.pp_hpred pe_default fmt hpred
  | (L.PTincrease_indent, n) ->
      let (n: int) = Obj.obj n in
      let s = ref "" in
      for _ = 1 to n do s := "  " ^ !s done;
      F.fprintf fmt "%s@[" !s
  | (L.PTinstr, i) ->
      let (i: Sil.instr) = Obj.obj i in
      if !Config.write_html
      then
        F.fprintf fmt "%a%a%a"
          Io_infer.Html.pp_start_color Green
          (Sil.pp_instr (pe_html Green)) i
          Io_infer.Html.pp_end_color ()
      else
        Sil.pp_instr pe_text fmt i
  | (L.PTinstr_list, il) ->
      let (il: Sil.instr list) = Obj.obj il in
      if !Config.write_html
      then
        F.fprintf fmt "%a%a%a"
          Io_infer.Html.pp_start_color Green
          (Sil.pp_instr_list (pe_html Green)) il
          Io_infer.Html.pp_end_color ()
      else
        Sil.pp_instr_list pe_text fmt il
  | (L.PTjprop_list, shallow_jpl) ->
      let ((shallow: bool), (jpl: Prop.normal Specs.Jprop.t list)) = Obj.obj shallow_jpl in
      Specs.Jprop.pp_list pe_default shallow fmt jpl
  | (L.PTjprop_short, jp) ->
      let (jp: Prop.normal Specs.Jprop.t) = Obj.obj jp in
      Specs.Jprop.pp_short pe_default fmt jp
  | (L.PTloc, loc) ->
      let (loc: Location.t) = Obj.obj loc in
      Location.pp fmt loc
  | (L.PTnode_instrs, b_n) ->
      let (b: bool), (io: Sil.instr option), (n: Cfg.node) = Obj.obj b_n in
      if !Config.write_html
      then
        F.fprintf fmt "%a%a%a"
          Io_infer.Html.pp_start_color Green
          (Cfg.Node.pp_instrs (pe_html Green) io ~sub_instrs: b) n
          Io_infer.Html.pp_end_color ()
      else
        F.fprintf fmt "%a"
          (Cfg.Node.pp_instrs pe_text io ~sub_instrs: b) n
  | (L.PToff, off) ->
      let (off: Sil.offset) = Obj.obj off in
      Sil.pp_offset pe_default fmt off
  | (L.PToff_list, offl) ->
      let (offl: Sil.offset list) = Obj.obj offl in
      Sil.pp_offset_list pe_default fmt offl
  | (L.PTpathset, ps) ->
      let (ps: Paths.PathSet.t) = Obj.obj ps in
      F.fprintf fmt "%a@\n" (Paths.PathSet.pp pe_default) ps
  | (L.PTpi, pi) ->
      let (pi: Sil.atom list) = Obj.obj pi in
      Prop.pp_pi pe_default fmt pi
  | (L.PTpath, path) ->
      let (path: Paths.Path.t) = Obj.obj path in
      Paths.Path.pp fmt path
  | (L.PTprop, p) ->
      let (p: Prop.normal Prop.t) = Obj.obj p in
      Prop.pp_prop pe_default fmt p
  | (L.PTproplist, x) ->
      let (p : Prop.normal Prop.t), (pl: Prop.normal Prop.t list) = Obj.obj x in
      Propgraph.pp_proplist pe_default "PROP" (p, false) fmt pl
  | (L.PTprop_list_with_typ, plist) ->
      let (pl: Prop.normal Prop.t list) = Obj.obj plist in
      F.fprintf fmt "%a" (Prop.pp_proplist_with_typ pe_default) pl
  | (L.PTprop_with_typ, p) ->
      let (p: Prop.normal Prop.t) = Obj.obj p in
      Prop.pp_prop_with_typ pe_default fmt p
  | (L.PTpvar, pvar) ->
      let (pvar: Pvar.t) = Obj.obj pvar in
      Pvar.pp pe_default fmt pvar
  | (L.PTsexp, se) ->
      let (se: Sil.strexp) = Obj.obj se in
      Sil.pp_sexp pe_default fmt se
  | (L.PTsexp_list, sel) ->
      let (sel: Sil.strexp list) = Obj.obj sel in
      Sil.pp_sexp_list pe_default fmt sel
  | (L.PTsigma, sigma) ->
      let (sigma: Sil.hpred list) = Obj.obj sigma in
      Prop.pp_sigma pe_default fmt sigma
  | (L.PTspec, spec) ->
      let (spec: Prop.normal Specs.spec) = Obj.obj spec in
      Specs.pp_spec
        (if !Config.write_html then pe_html Blue else pe_text)
        None fmt spec
  | (L.PTstr, s) ->
      let (s: string) = Obj.obj s in
      F.fprintf fmt "%s" s
  | (L.PTstr_color, s) ->
      let (s: string), (c: color) = Obj.obj s in
      if !Config.write_html
      then
        F.fprintf fmt "%a%s%a"
          Io_infer.Html.pp_start_color c
          s
          Io_infer.Html.pp_end_color ()
      else
        F.fprintf fmt "%s" s
  | (L.PTstrln, s) ->
      let (s: string) = Obj.obj s in
      F.fprintf fmt "%s@\n" s
  | (L.PTstrln_color, s) ->
      let (s: string), (c: color) = Obj.obj s in
      if !Config.write_html
      then
        F.fprintf fmt "%a%s%a@\n"
          Io_infer.Html.pp_start_color c
          s
          Io_infer.Html.pp_end_color ()
      else
        F.fprintf fmt "%s@\n" s
  | (L.PTsub, sub) ->
      let (sub: Sil.subst) = Obj.obj sub in
      Prop.pp_sub pe_default fmt sub
  | (L.PTtexp_full, te) ->
      let (te: Sil.exp) = Obj.obj te in
      Sil.pp_texp_full pe_default fmt te
  | (L.PTtyp_full, t) ->
      let (t: Sil.typ) = Obj.obj t in
      Sil.pp_typ_full pe_default fmt t
  | (L.PTtyp_list, tl) ->
      let (tl: Sil.typ list) = Obj.obj tl in
      (pp_seq (Sil.pp_typ pe_default)) fmt tl
  | (L.PTerror, s) ->
      let (s: string) = Obj.obj s in
      if !Config.write_html
      then
        F.fprintf fmt "%aERROR: %s%a"
          Io_infer.Html.pp_start_color Red
          s
          Io_infer.Html.pp_end_color ()
      else
        F.fprintf fmt "ERROR: %s" s
  | (L.PTwarning, s) ->
      let (s: string) = Obj.obj s in
      if !Config.write_html
      then
        F.fprintf fmt "%aWARNING: %s%a"
          Io_infer.Html.pp_start_color Orange
          s
          Io_infer.Html.pp_end_color ()
      else
        F.fprintf fmt "WARNING: %s" s
  | (L.PTinfo, s) ->
      let (s: string) = Obj.obj s in
      if !Config.write_html
      then
        F.fprintf fmt "%aINFO: %s%a"
          Io_infer.Html.pp_start_color Blue
          s
          Io_infer.Html.pp_end_color ()
      else
        F.fprintf fmt "INFO: %s" s

(** Set printer hook as soon as this module is loaded *)
let () = L.printer_hook := force_delayed_print

(** Execute the delayed print actions *)
let force_delayed_prints () =
  Config.forcing_delayed_prints := true;
  F.fprintf !curr_html_formatter "@?"; (* flush html stream *)
  IList.iter
    (force_delayed_print !curr_html_formatter)
    (IList.rev (L.get_delayed_prints ()));
  F.fprintf !curr_html_formatter "@?";
  L.reset_delayed_prints ();
  Config.forcing_delayed_prints := false

(** Start a session, and create a new html fine for the node if it does not exist yet *)
let start_session node (loc: Location.t) proc_name session =
  let node_id = Cfg.Node.get_id node in
  (if NodesHtml.start_node
      (node_id :> int) loc proc_name
      (Cfg.Node.get_preds node)
      (Cfg.Node.get_succs node)
      (Cfg.Node.get_exn node)
   then
     F.fprintf !curr_html_formatter "%a<LISTING>%a</LISTING>%a"
       Io_infer.Html.pp_start_color Green
       (Cfg.Node.pp_instrs (pe_html Green) None ~sub_instrs: true) node
       Io_infer.Html.pp_end_color ());
  F.fprintf !curr_html_formatter "%a%a"
    Io_infer.Html.pp_hline ()
    (Io_infer.Html.pp_session_link ~with_name: true [".."])
    ((node_id :> int), session, loc.Location.line);
  F.fprintf !curr_html_formatter "<LISTING>%a"
    Io_infer.Html.pp_start_color Black

let node_start_session node loc proc_name session =
  if !Config.write_html then
    start_session node loc proc_name session

(** Finish a session, and perform delayed print actions if required *)
let node_finish_session node =
  if !Config.test == false then force_delayed_prints ()
  else L.reset_delayed_prints ();
  if !Config.write_html then begin
    F.fprintf !curr_html_formatter "</LISTING>%a"
      Io_infer.Html.pp_end_color ();
    NodesHtml.finish_node (Cfg.Node.get_id node :> int)
  end

(** Write html file for the procedure.
    The boolean indicates whether to print whole seconds only *)
let write_proc_html whole_seconds pdesc =
  if !Config.write_html then
    begin
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let nodes = IList.sort Cfg.Node.compare (Cfg.Procdesc.get_nodes pdesc) in
      let linenum = (Cfg.Node.get_loc (IList.hd nodes)).Location.line in
      let fd, fmt =
        Io_infer.Html.create DB.Results_dir.Abs_source_dir [Procname.to_filename pname] in
      F.fprintf fmt "<center><h1>Procedure %a</h1></center>@\n"
        (Io_infer.Html.pp_line_link
           ~text: (Some (Escape.escape_xml (Procname.to_string pname)))
           [])
        linenum;
      IList.iter
        (fun n ->
           Io_infer.Html.pp_node_link []
             (Cfg.Node.get_description (pe_html Black) n)
             (IList.map Cfg.Node.get_id (Cfg.Node.get_preds n) :> int list)
             (IList.map Cfg.Node.get_id (Cfg.Node.get_succs n) :> int list)
             (IList.map Cfg.Node.get_id (Cfg.Node.get_exn n) :> int list)
             (is_visited n) false fmt (Cfg.Node.get_id n :> int))
        nodes;
      (match Specs.get_summary pname with
       | None ->
           ()
       | Some summary ->
           Specs.pp_summary (pe_html Black) whole_seconds fmt summary;
           Io_infer.Html.close (fd, fmt))
    end

(** Creare a hash table mapping line numbers to the set of errors occurring on that line *)
let create_table_err_per_line err_log =
  let err_per_line = Hashtbl.create 17 in
  let add_err _ loc _ _ _ err_name desc _ _ _ _ =
    let err_str =
      Localise.to_string err_name ^
      " " ^
      (pp_to_string Localise.pp_error_desc desc) in
    try
      let set = Hashtbl.find err_per_line loc.Location.line in
      Hashtbl.replace err_per_line loc.Location.line (StringSet.add err_str set)
    with Not_found ->
      Hashtbl.add err_per_line loc.Location.line (StringSet.singleton err_str) in
  Errlog.iter add_err err_log;
  err_per_line

(** Create error message for html file *)
let create_err_message err_string =
  "\n<div class=\"msg\" style=\"margin-left:9ex\">" ^ err_string ^ "</div>"

(** Create filename.ext.html. *)
let write_html_file linereader filename cfg =
  let process_node nodes_tbl n =
    let lnum = (Cfg.Node.get_loc n).Location.line in
    let curr_nodes =
      try Hashtbl.find nodes_tbl lnum
      with Not_found -> [] in
    Hashtbl.replace nodes_tbl lnum (n:: curr_nodes) in
  let fname_encoding =
    DB.source_file_encoding filename in
  let (fd, fmt) =
    Io_infer.Html.create DB.Results_dir.Abs_source_dir [".."; fname_encoding] in
  let do_proc proof_cover table_nodes_at_linenum global_err_log proc_name proc_desc =
    (* add the err_log of this proc to [global_err_log] *)
    let proc_loc = (Cfg.Procdesc.get_loc proc_desc) in
    if Cfg.Procdesc.is_defined proc_desc &&
       (DB.source_file_equal proc_loc.Location.file !DB.current_source) then
      begin
        IList.iter (process_node table_nodes_at_linenum) (Cfg.Procdesc.get_nodes proc_desc);
        match Specs.get_summary proc_name with
        | None ->
            ()
        | Some summary ->
            IList.iter
              (fun sp ->
                 proof_cover := Specs.Visitedset.union sp.Specs.visited !proof_cover)
              (Specs.get_specs_from_payload summary);
            Errlog.update global_err_log summary.Specs.attributes.ProcAttributes.err_log
      end in

  let pp_prelude () =
    let s =
      "<center><h1>File " ^
      (DB.source_file_to_string !DB.current_source) ^
      "</h1></center>\n" ^
      "<table class=\"code\">\n" in
    F.fprintf fmt "%s" s in

  let print_one_line proof_cover table_nodes_at_linenum table_err_per_line line_number =
    let line_html =
      match LineReader.from_file_linenum linereader !DB.current_source line_number with
      | Some line_raw ->
          Escape.escape_xml line_raw
      | None ->
          raise End_of_file in
    let nodes_at_linenum =
      try Hashtbl.find table_nodes_at_linenum line_number
      with Not_found -> [] in
    let errors_at_linenum =
      try
        let errset = Hashtbl.find table_err_per_line line_number in
        StringSet.elements errset
      with Not_found -> [] in
    let linenum_str = string_of_int line_number in
    let line_str = "LINE" ^ linenum_str in
    let str =
      "<tr><td class=\"num\" id=\"" ^
      line_str ^
      "\">" ^
      linenum_str ^
      "</td><td class=\"line\">" ^
      line_html in
    F.fprintf fmt "%s" str;
    IList.iter
      (fun n ->
         let isproof =
           Specs.Visitedset.mem (Cfg.Node.get_id n, []) !proof_cover in
         Io_infer.Html.pp_node_link
           [fname_encoding]
           (Cfg.Node.get_description (pe_html Black) n)
           (IList.map Cfg.Node.get_id (Cfg.Node.get_preds n) :> int list)
           (IList.map Cfg.Node.get_id (Cfg.Node.get_succs n) :> int list)
           (IList.map Cfg.Node.get_id (Cfg.Node.get_exn n) :> int list)
           (is_visited n)
           isproof
           fmt
           (Cfg.Node.get_id n :> int))
      nodes_at_linenum;
    IList.iter
      (fun n ->
         match Cfg.Node.get_kind n with
         | Cfg.Node.Start_node proc_desc ->
             let proc_name = Cfg.Procdesc.get_proc_name proc_desc in
             let num_specs = IList.length (Specs.get_specs proc_name) in
             let label =
               (Escape.escape_xml (Procname.to_string proc_name)) ^
               ": " ^
               (string_of_int num_specs) ^
               " specs" in
             Io_infer.Html.pp_proc_link [fname_encoding] proc_name fmt label
         | _ ->
             ())
      nodes_at_linenum;
    IList.iter
      (fun err_string ->
         F.fprintf fmt "%s" (create_err_message err_string))
      errors_at_linenum;
    F.fprintf fmt "%s" "</td></tr>\n" in

  pp_prelude ();
  let global_err_log = Errlog.empty () in
  let table_nodes_at_linenum = Hashtbl.create 11 in
  let proof_cover = ref Specs.Visitedset.empty in
  Cfg.iter_proc_desc cfg (do_proc proof_cover table_nodes_at_linenum global_err_log);
  let table_err_per_line = create_table_err_per_line global_err_log in
  let linenum = ref 0 in

  try
    while true do
      incr linenum;
      print_one_line proof_cover table_nodes_at_linenum table_err_per_line !linenum
    done
  with End_of_file ->
    (F.fprintf fmt "%s" "</table>\n";
     Errlog.pp_html [fname_encoding] fmt global_err_log;
     Io_infer.Html.close (fd, fmt))

(** Create filename.ext.html for each file in the exe_env. *)
let write_all_html_files linereader exe_env =
  if !Config.write_html then
    Exe_env.iter_files
      (fun _ cfg ->
         let source_files_in_cfg =
           let files = ref DB.SourceFileSet.empty in
           Cfg.iter_proc_desc cfg
             (fun _ proc_desc ->
                if Cfg.Procdesc.is_defined proc_desc
                then
                  let file = (Cfg.Procdesc.get_loc proc_desc).Location.file in
                  files := DB.SourceFileSet.add file !files);
           !files in
         DB.SourceFileSet.iter
           (fun file ->
              DB.current_source := file;
              write_html_file linereader file cfg)
           source_files_in_cfg)
      exe_env
