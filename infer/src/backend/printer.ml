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
module Hashtbl = Caml.Hashtbl

(** Printers for the analysis results *)

module L = Logging
module F = Format

(** Module to read specific lines from files.
    The data from any file will stay in memory until the handle is collected by the gc. *)
module LineReader =
struct
  (** Map a file name to an array of string, one for each line in the file. *)
  type t = (SourceFile.t, string array) Hashtbl.t

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
          if len > 0 && Char.equal (String.get line_raw (len -1)) '\013' then
            String.sub line_raw ~pos:0 ~len:(len -1)
          else line_raw in
        lines := line :: !lines
      done;
      assert false (* execution never reaches here *)
    with End_of_file ->
      (In_channel.close cin;
       Array.of_list (List.rev !lines))

  let file_data (hash: t) fname =
    try
      Some (Hashtbl.find hash fname)
    with Not_found ->
    try
      let lines_arr = read_file (SourceFile.to_abs_path fname) in
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
    from_file_linenum_original hash fname linenum

  let from_loc hash loc =
    from_file_linenum hash loc.Location.file loc.Location.line
end


(** Current formatter for the html output *)
let curr_html_formatter = ref F.std_formatter

(** Return true if the node was visited during footprint and during re-execution*)
let node_is_visited node =
  match Specs.get_summary (Procdesc.Node.get_proc_name node) with
  | None ->
      false, false
  | Some summary ->
      let stats = summary.Specs.stats in
      let is_visited_fp =
        IntSet.mem (Procdesc.Node.get_id node :> int) stats.Specs.nodes_visited_fp in
      let is_visited_re =
        IntSet.mem (Procdesc.Node.get_id node :> int) stats.Specs.nodes_visited_re in
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
    int -> Location.t -> Typ.Procname.t -> Procdesc.Node.t list ->
    Procdesc.Node.t list -> Procdesc.Node.t list ->
    SourceFile.t -> bool
  val finish_node : Typ.Procname.t -> int -> SourceFile.t -> unit
end = struct
  let log_files = Hashtbl.create 11

  let start_node nodeid loc proc_name preds succs exns source =
    let node_fname = Io_infer.Html.node_filename proc_name nodeid in
    let modified = Io_infer.Html.modified_during_analysis source ["nodes"; node_fname] in
    let needs_initialization, (fd, fmt) =
      if modified then
        (false, Io_infer.Html.open_out source ["nodes"; node_fname])
      else
        (true,
         Io_infer.Html.create
           (DB.Results_dir.Abs_source_dir source)
           ["nodes"; node_fname]) in
    curr_html_formatter := fmt;
    Hashtbl.replace log_files (node_fname, source) fd;
    if needs_initialization then
      (F.fprintf fmt "<center><h1>Cfg Node %a</h1></center>"
         (Io_infer.Html.pp_line_link source ~text: (Some (string_of_int nodeid)) [".."])
         loc.Location.line;
       F.fprintf fmt "PROC: %a LINE:%a\n"
         (Io_infer.Html.pp_proc_link [".."] proc_name)
         (Escape.escape_xml (Typ.Procname.to_string proc_name))
         (Io_infer.Html.pp_line_link source [".."]) loc.Location.line;
       F.fprintf fmt "<br>PREDS:@\n";
       List.iter ~f:(fun node ->
           Io_infer.Html.pp_node_link
             [".."]
             (Procdesc.Node.get_proc_name node)
             ~description:""
             ~preds:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_preds node) :> int list)
             ~succs:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_succs node) :> int list)
             ~exn:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_exn node) :> int list)
             ~isvisited:(is_visited node)
             ~isproof:false
             fmt (Procdesc.Node.get_id node :> int)) preds;
       F.fprintf fmt "<br>SUCCS: @\n";
       List.iter ~f:(fun node ->
           Io_infer.Html.pp_node_link
             [".."]
             (Procdesc.Node.get_proc_name node)
             ~description:""
             ~preds:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_preds node) :> int list)
             ~succs:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_succs node) :> int list)
             ~exn:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_exn node) :> int list)
             ~isvisited:(is_visited node)
             ~isproof:false
             fmt (Procdesc.Node.get_id node :> int)) succs;
       F.fprintf fmt "<br>EXN: @\n";
       List.iter ~f:(fun node ->
           Io_infer.Html.pp_node_link
             [".."]
             (Procdesc.Node.get_proc_name node)
             ~description:""
             ~preds:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_preds node) :> int list)
             ~succs:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_succs node) :> int list)
             ~exn:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_exn node) :> int list)
             ~isvisited:(is_visited node)
             ~isproof:false
             fmt (Procdesc.Node.get_id node :> int)) exns;
       F.fprintf fmt "<br>@\n";
       F.pp_print_flush fmt ();
       true
      )
    else false

  let finish_node proc_name nodeid source =
    let node_fname = Io_infer.Html.node_filename proc_name nodeid in
    let fd = Hashtbl.find log_files (node_fname, source) in
    Unix.close fd;
    curr_html_formatter := F.std_formatter
end
(* =============== END of module NodesHtml =============== *)

(* =============== Printing functions =============== *)

(** Execute the delayed print actions *)
let force_delayed_print fmt =
  let pe_default =
    if Config.write_html then Pp.html Black else Pp.text in
  function
  | (L.PTatom, a) ->
      let (a: Sil.atom) = Obj.obj a in
      Sil.pp_atom pe_default fmt a
  | (L.PTattribute, a) ->
      let (a: PredSymb.t) = Obj.obj a in
      F.pp_print_string fmt (PredSymb.to_string pe_default a)
  | (L.PTdecrease_indent, n) ->
      let (n: int) = Obj.obj n in
      for _ = 1 to n do F.fprintf fmt "@]" done
  | (L.PTexp, e) ->
      let (e: Exp.t) = Obj.obj e in
      Sil.pp_exp_printenv pe_default fmt e
  | (L.PTexp_list, el) ->
      let (el: Exp.t list) = Obj.obj el in
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
      if Config.write_html
      then
        F.fprintf fmt "%a%a%a"
          Io_infer.Html.pp_start_color Pp.Green
          (Sil.pp_instr (Pp.html Green)) i
          Io_infer.Html.pp_end_color ()
      else
        Sil.pp_instr Pp.text fmt i
  | (L.PTinstr_list, il) ->
      let (il: Sil.instr list) = Obj.obj il in
      if Config.write_html
      then
        F.fprintf fmt "%a%a%a"
          Io_infer.Html.pp_start_color Pp.Green
          (Sil.pp_instr_list (Pp.html Green)) il
          Io_infer.Html.pp_end_color ()
      else
        Sil.pp_instr_list Pp.text fmt il
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
      let (b: bool), (io: Sil.instr option), (n: Procdesc.Node.t) = Obj.obj b_n in
      if Config.write_html
      then
        F.fprintf fmt "%a%a%a"
          Io_infer.Html.pp_start_color Pp.Green
          (Procdesc.Node.pp_instrs (Pp.html Green) io ~sub_instrs: b) n
          Io_infer.Html.pp_end_color ()
      else
        F.fprintf fmt "%a"
          (Procdesc.Node.pp_instrs Pp.text io ~sub_instrs: b) n
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
        (if Config.write_html then Pp.html Blue else Pp.text)
        None fmt spec
  | (L.PTstr, s) ->
      let (s: string) = Obj.obj s in
      F.fprintf fmt "%s" s
  | (L.PTstr_color, s) ->
      let (s: string), (c: Pp.color) = Obj.obj s in
      if Config.write_html
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
      let (s: string), (c: Pp.color) = Obj.obj s in
      if Config.write_html
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
      let (te: Exp.t) = Obj.obj te in
      Sil.pp_texp_full pe_default fmt te
  | (L.PTtyp_full, t) ->
      let (t: Typ.t) = Obj.obj t in
      Typ.pp_full pe_default fmt t
  | (L.PTtyp_list, tl) ->
      let (tl: Typ.t list) = Obj.obj tl in
      (Pp.seq (Typ.pp pe_default)) fmt tl
  | (L.PTerror, s) ->
      let (s: string) = Obj.obj s in
      if Config.write_html
      then
        F.fprintf fmt "%aERROR: %s%a"
          Io_infer.Html.pp_start_color Pp.Red
          s
          Io_infer.Html.pp_end_color ()
      else
        F.fprintf fmt "ERROR: %s" s
  | (L.PTwarning, s) ->
      let (s: string) = Obj.obj s in
      if Config.write_html
      then
        F.fprintf fmt "%aWARNING: %s%a"
          Io_infer.Html.pp_start_color Pp.Orange
          s
          Io_infer.Html.pp_end_color ()
      else
        F.fprintf fmt "WARNING: %s" s
  | (L.PTinfo, s) ->
      let (s: string) = Obj.obj s in
      if Config.write_html
      then
        F.fprintf fmt "%aINFO: %s%a"
          Io_infer.Html.pp_start_color Pp.Blue
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
  List.iter
    ~f:(force_delayed_print !curr_html_formatter)
    (List.rev (L.get_delayed_prints ()));
  F.fprintf !curr_html_formatter "@?";
  L.reset_delayed_prints ();
  Config.forcing_delayed_prints := false

(** Start a session, and create a new html fine for the node if it does not exist yet *)
let start_session node (loc: Location.t) proc_name session source =
  let node_id = Procdesc.Node.get_id node in
  (if NodesHtml.start_node
      (node_id :> int) loc proc_name
      (Procdesc.Node.get_preds node)
      (Procdesc.Node.get_succs node)
      (Procdesc.Node.get_exn node)
      source
   then
     F.fprintf !curr_html_formatter "%a<LISTING>%a</LISTING>%a"
       Io_infer.Html.pp_start_color Pp.Green
       (Procdesc.Node.pp_instrs (Pp.html Green) None ~sub_instrs: true) node
       Io_infer.Html.pp_end_color ());
  F.fprintf !curr_html_formatter "%a%a"
    Io_infer.Html.pp_hline ()
    (Io_infer.Html.pp_session_link source ~with_name: true [".."])
    ((node_id :> int), session, loc.Location.line);
  F.fprintf !curr_html_formatter "<LISTING>%a"
    Io_infer.Html.pp_start_color Pp.Black

let node_start_session node session source =
  if Config.write_html then
    let loc = Procdesc.Node.get_loc node in
    let pname = Procdesc.Node.get_proc_name node in
    start_session node loc pname session source

(** Finish a session, and perform delayed print actions if required *)
let node_finish_session node source =
  if not Config.test then force_delayed_prints ()
  else L.reset_delayed_prints ();
  if Config.write_html then begin
    F.fprintf !curr_html_formatter "</LISTING>%a"
      Io_infer.Html.pp_end_color ();
    NodesHtml.finish_node
      (Procdesc.Node.get_proc_name node)
      (Procdesc.Node.get_id node :> int)
      source
  end

(** Write html file for the procedure.
    The boolean indicates whether to print whole seconds only *)
let write_proc_html source whole_seconds pdesc =
  if Config.write_html then
    begin
      let pname = Procdesc.get_proc_name pdesc in
      let nodes = List.sort ~cmp:Procdesc.Node.compare (Procdesc.get_nodes pdesc) in
      let linenum = (Procdesc.Node.get_loc (List.hd_exn nodes)).Location.line in
      let fd, fmt =
        Io_infer.Html.create
          (DB.Results_dir.Abs_source_dir source)
          [Typ.Procname.to_filename pname] in
      F.fprintf fmt "<center><h1>Procedure %a</h1></center>@\n"
        (Io_infer.Html.pp_line_link source
           ~text: (Some (Escape.escape_xml (Typ.Procname.to_string pname)))
           [])
        linenum;
      List.iter
        ~f:(fun n ->
            Io_infer.Html.pp_node_link
              []
              (Procdesc.Node.get_proc_name n)
              ~description:(Procdesc.Node.get_description (Pp.html Black) n)
              ~preds:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_preds n) :> int list)
              ~succs:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_succs n) :> int list)
              ~exn:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_exn n) :> int list)
              ~isvisited:(is_visited n)
              ~isproof:false
              fmt (Procdesc.Node.get_id n :> int))
        nodes;
      (match Specs.get_summary pname with
       | None ->
           ()
       | Some summary ->
           Specs.pp_summary_html source Black ~whole_seconds fmt summary;
           Io_infer.Html.close (fd, fmt))
    end

(** Creare a hash table mapping line numbers to the set of errors occurring on that line *)
let create_table_err_per_line err_log =
  let err_per_line = Hashtbl.create 17 in
  let add_err _ loc _ _ _ err_name desc _ _ _ _ =
    let err_str =
      Localise.to_string err_name ^
      " " ^
      (F.asprintf "%a" Localise.pp_error_desc desc) in
    try
      let set = Hashtbl.find err_per_line loc.Location.line in
      Hashtbl.replace err_per_line loc.Location.line (String.Set.add set err_str)
    with Not_found ->
      Hashtbl.add err_per_line loc.Location.line (String.Set.singleton err_str) in
  Errlog.iter add_err err_log;
  err_per_line

(** Create error message for html file *)
let create_err_message err_string =
  "\n<div class=\"msg\" style=\"margin-left:9ex\">" ^ err_string ^ "</div>"

let write_html_proc source proof_cover table_nodes_at_linenum global_err_log proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let process_node n =
    let lnum = (Procdesc.Node.get_loc n).Location.line in
    let curr_nodes =
      try Hashtbl.find table_nodes_at_linenum lnum
      with Not_found -> [] in
    Hashtbl.replace table_nodes_at_linenum lnum (n :: curr_nodes) in
  let proc_loc = Procdesc.get_loc proc_desc in
  let process_proc =
    Procdesc.is_defined proc_desc &&
    SourceFile.equal proc_loc.Location.file source &&
    match AttributesTable.find_file_capturing_procedure proc_name with
    | None -> true
    | Some (source_captured, _) ->
        SourceFile.equal source_captured (Procdesc.get_loc proc_desc).file in
  if process_proc then
    begin
      List.iter ~f:process_node (Procdesc.get_nodes proc_desc);
      match Specs.get_summary proc_name with
      | None ->
          ()
      | Some summary ->
          List.iter
            ~f:(fun sp ->
                proof_cover := Specs.Visitedset.union sp.Specs.visited !proof_cover)
            (Specs.get_specs_from_payload summary);
          Errlog.update global_err_log summary.Specs.attributes.ProcAttributes.err_log
    end

(** Create filename.ext.html. *)
let write_html_file linereader filename procs =
  let fname_encoding = SourceFile.encoding filename in
  let (fd, fmt) =
    Io_infer.Html.create
      (DB.Results_dir.Abs_source_dir filename)
      [".."; fname_encoding] in
  let pp_prelude () =
    F.fprintf fmt "<center><h1>File %a </h1></center>\n<table class=\"code\">\n"
      SourceFile.pp filename in
  let print_one_line proof_cover table_nodes_at_linenum table_err_per_line line_number =
    let line_html =
      match LineReader.from_file_linenum linereader filename line_number with
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
        String.Set.elements errset
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
    List.iter
      ~f:(fun n ->
          let isproof =
            Specs.Visitedset.mem (Procdesc.Node.get_id n, []) !proof_cover in
          Io_infer.Html.pp_node_link
            [fname_encoding]
            (Procdesc.Node.get_proc_name n)
            ~description:(Procdesc.Node.get_description (Pp.html Black) n)
            ~preds:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_preds n) :> int list)
            ~succs:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_succs n) :> int list)
            ~exn:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_exn n) :> int list)
            ~isvisited:(is_visited n)
            ~isproof
            fmt (Procdesc.Node.get_id n :> int))
      nodes_at_linenum;
    List.iter
      ~f:(fun n ->
          match Procdesc.Node.get_kind n with
          | Procdesc.Node.Start_node proc_name ->
              let num_specs = List.length (Specs.get_specs proc_name) in
              let label =
                (Escape.escape_xml (Typ.Procname.to_string proc_name)) ^
                ": " ^
                (string_of_int num_specs) ^
                " specs" in
              Io_infer.Html.pp_proc_link [fname_encoding] proc_name fmt label
          | _ ->
              ())
      nodes_at_linenum;
    List.iter
      ~f:(fun err_string ->
          F.fprintf fmt "%s" (create_err_message err_string))
      errors_at_linenum;
    F.fprintf fmt "%s" "</td></tr>\n" in

  pp_prelude ();
  let global_err_log = Errlog.empty () in
  let table_nodes_at_linenum = Hashtbl.create 11 in
  let proof_cover = ref Specs.Visitedset.empty in
  List.iter ~f:(write_html_proc filename proof_cover table_nodes_at_linenum global_err_log) procs;
  let table_err_per_line = create_table_err_per_line global_err_log in
  let linenum = ref 0 in

  try
    while true do
      incr linenum;
      print_one_line proof_cover table_nodes_at_linenum table_err_per_line !linenum
    done
  with End_of_file ->
    (F.fprintf fmt "%s" "</table>\n";
     Errlog.pp_html filename [fname_encoding] fmt global_err_log;
     Io_infer.Html.close (fd, fmt))

(** Create filename.ext.html for each file in the exe_env. *)
let write_all_html_files exe_env =
  if Config.write_html then
    let linereader = LineReader.create () in
    Exe_env.iter_files
      (fun _ cfg ->
         let source_files_in_cfg =
           let files = ref SourceFile.Set.empty in
           Cfg.iter_proc_desc cfg
             (fun _ proc_desc ->
                if Procdesc.is_defined proc_desc
                then
                  let file = (Procdesc.get_loc proc_desc).Location.file in
                  files := SourceFile.Set.add file !files);
           !files in
         SourceFile.Set.iter
           (fun file ->
              write_html_file linereader file (Cfg.get_all_procs cfg))
           source_files_in_cfg)
      exe_env
