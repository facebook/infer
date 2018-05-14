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
      assert false
      (* execution never reaches here *)
    with End_of_file ->
      In_channel.close cin ;
      Array.of_list (List.rev !lines)


  let file_data (hash: t) fname =
    try Some (Hashtbl.find hash fname) with Caml.Not_found ->
      try
        let lines_arr = read_file (SourceFile.to_abs_path fname) in
        Hashtbl.add hash fname lines_arr ; Some lines_arr
      with exn when SymOp.exn_not_failure exn -> None


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

(** Return true if the node was visited during footprint and during re-execution*)
let node_is_visited node =
  match Summary.get (Procdesc.Node.get_proc_name node) with
  | None ->
      (false, false)
  | Some summary ->
      let stats = summary.Summary.stats in
      let node_id = (Procdesc.Node.get_id node :> int) in
      let is_visited_fp = Summary.Stats.is_visited_fp stats node_id in
      let is_visited_re = Summary.Stats.is_visited_re stats node_id in
      (is_visited_fp, is_visited_re)


(** Return true if the node was visited during analysis *)
let is_visited node =
  let visited_fp, visited_re = node_is_visited node in
  visited_fp || visited_re


let pp_node_link path_to_root ?proof_cover ~description fmt node =
  let description =
    if description then Procdesc.Node.get_description (Pp.html Black) node else ""
  in
  let isproof =
    match proof_cover with
    | Some proof_cover ->
        BiabductionSummary.Visitedset.mem (Procdesc.Node.get_id node, []) proof_cover
    | None ->
        false
  in
  Io_infer.Html.pp_node_link path_to_root
    (Procdesc.Node.get_proc_name node)
    ~description
    ~preds:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_preds node) :> int list)
    ~succs:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_succs node) :> int list)
    ~exn:(List.map ~f:Procdesc.Node.get_id (Procdesc.Node.get_exn node) :> int list)
    ~isvisited:(is_visited node) ~isproof fmt
    (Procdesc.Node.get_id node :> int)


(* =============== START of module NodesHtml =============== *)

(** Print information into html files for nodes
    when starting and finishing the processing of a node *)
module NodesHtml : sig
  val start_node :
    int -> Location.t -> Typ.Procname.t -> Procdesc.Node.t list -> Procdesc.Node.t list
    -> Procdesc.Node.t list -> SourceFile.t -> bool

  val finish_node : Typ.Procname.t -> int -> SourceFile.t -> unit
end = struct
  let log_files = Hashtbl.create 11

  let pp_node_link fmt node = pp_node_link [".."] ~description:false fmt node

  let start_node nodeid loc proc_name preds succs exns source =
    let node_fname = Io_infer.Html.node_filename proc_name nodeid in
    let modified = Io_infer.Html.modified_during_analysis source ["nodes"; node_fname] in
    let needs_initialization, (fd, fmt) =
      if modified then (false, Io_infer.Html.open_out source ["nodes"; node_fname])
      else (true, Io_infer.Html.create (DB.Results_dir.Abs_source_dir source) ["nodes"; node_fname])
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
      Pp.seq pp_node_link fmt preds ;
      F.fprintf fmt "<br>SUCCS: @\n" ;
      Pp.seq pp_node_link fmt succs ;
      F.fprintf fmt "<br>EXN: @\n" ;
      Pp.seq pp_node_link fmt exns ;
      F.fprintf fmt "<br>@\n" ;
      F.pp_print_flush fmt () ;
      true )
    else false


  let finish_node proc_name nodeid source =
    let node_fname = Io_infer.Html.node_filename proc_name nodeid in
    let fd = Hashtbl.find log_files (node_fname, source) in
    Unix.close fd ;
    curr_html_formatter := F.std_formatter
end

(* =============== END of module NodesHtml =============== *)
(* =============== Printing functions =============== *)

(** Execute the delayed print actions *)
let force_delayed_print fmt =
  let pe_default = if Config.write_html then Pp.html Black else Pp.text in
  function
    | L.PTatom, a ->
        let a : Sil.atom = Obj.obj a in
        Sil.pp_atom pe_default fmt a
    | L.PTattribute, a ->
        let a : PredSymb.t = Obj.obj a in
        F.pp_print_string fmt (PredSymb.to_string pe_default a)
    | L.PTdecrease_indent, n ->
        let n : int = Obj.obj n in
        for _ = 1 to n do F.fprintf fmt "@]" done
    | L.PTexp, e ->
        let e : Exp.t = Obj.obj e in
        Sil.pp_exp_printenv pe_default fmt e
    | L.PTexp_list, el ->
        let el : Exp.t list = Obj.obj el in
        Sil.pp_exp_list pe_default fmt el
    | L.PThpred, hpred ->
        let hpred : Sil.hpred = Obj.obj hpred in
        Sil.pp_hpred pe_default fmt hpred
    | L.PTincrease_indent, n ->
        let n : int = Obj.obj n in
        F.fprintf fmt "%s@[" (String.make (2 * n) ' ')
    | L.PTinstr, i ->
        let i : Sil.instr = Obj.obj i in
        if Config.write_html then
          F.fprintf fmt "%a%a%a" Io_infer.Html.pp_start_color Pp.Green
            (Sil.pp_instr (Pp.html Green))
            i Io_infer.Html.pp_end_color ()
        else Sil.pp_instr Pp.text fmt i
    | L.PTinstr_list, il ->
        let il : Sil.instr list = Obj.obj il in
        if Config.write_html then
          F.fprintf fmt "%a%a%a" Io_infer.Html.pp_start_color Pp.Green
            (Sil.pp_instr_list (Pp.html Green))
            il Io_infer.Html.pp_end_color ()
        else Sil.pp_instr_list Pp.text fmt il
    | L.PTjprop_list, shallow_jpl ->
        let (shallow: bool), (jpl: Prop.normal BiabductionSummary.Jprop.t list) =
          Obj.obj shallow_jpl
        in
        BiabductionSummary.Jprop.pp_list pe_default shallow fmt jpl
    | L.PTjprop_short, jp ->
        let jp : Prop.normal BiabductionSummary.Jprop.t = Obj.obj jp in
        BiabductionSummary.Jprop.pp_short pe_default fmt jp
    | L.PTloc, loc ->
        let loc : Location.t = Obj.obj loc in
        Location.pp fmt loc
    | L.PTnode_instrs, b_n ->
        let (b: bool), (io: Sil.instr option), (n: Procdesc.Node.t) = Obj.obj b_n in
        if Config.write_html then
          F.fprintf fmt "%a%a%a" Io_infer.Html.pp_start_color Pp.Green
            (Procdesc.Node.pp_instrs (Pp.html Green) io ~sub_instrs:b)
            n Io_infer.Html.pp_end_color ()
        else Procdesc.Node.pp_instrs Pp.text io ~sub_instrs:b fmt n
    | L.PToff, off ->
        let off : Sil.offset = Obj.obj off in
        Sil.pp_offset pe_default fmt off
    | L.PToff_list, offl ->
        let offl : Sil.offset list = Obj.obj offl in
        Sil.pp_offset_list pe_default fmt offl
    | L.PTpathset, ps ->
        let ps : Paths.PathSet.t = Obj.obj ps in
        F.fprintf fmt "%a@\n" (Paths.PathSet.pp pe_default) ps
    | L.PTpi, pi ->
        let pi : Sil.atom list = Obj.obj pi in
        Prop.pp_pi pe_default fmt pi
    | L.PTpath, path ->
        let path : Paths.Path.t = Obj.obj path in
        Paths.Path.pp fmt path
    | L.PTprop, p ->
        let p : Prop.normal Prop.t = Obj.obj p in
        Prop.pp_prop pe_default fmt p
    | L.PTproplist, x ->
        let (p: Prop.normal Prop.t), (pl: Prop.normal Prop.t list) = Obj.obj x in
        Propgraph.pp_proplist pe_default "PROP" (p, false) fmt pl
    | L.PTprop_list_with_typ, plist ->
        let pl : Prop.normal Prop.t list = Obj.obj plist in
        Prop.pp_proplist_with_typ pe_default fmt pl
    | L.PTprop_with_typ, p ->
        let p : Prop.normal Prop.t = Obj.obj p in
        Prop.pp_prop_with_typ pe_default fmt p
    | L.PTpvar, pvar ->
        let pvar : Pvar.t = Obj.obj pvar in
        Pvar.pp pe_default fmt pvar
    | L.PTsexp, se ->
        let se : Sil.strexp = Obj.obj se in
        Sil.pp_sexp pe_default fmt se
    | L.PTsexp_list, sel ->
        let sel : Sil.strexp list = Obj.obj sel in
        Sil.pp_sexp_list pe_default fmt sel
    | L.PTsigma, sigma ->
        let sigma : Sil.hpred list = Obj.obj sigma in
        Prop.pp_sigma pe_default fmt sigma
    | L.PTspec, spec ->
        let spec : Prop.normal BiabductionSummary.spec = Obj.obj spec in
        BiabductionSummary.pp_spec
          (if Config.write_html then Pp.html Blue else Pp.text)
          None fmt spec
    | L.PTstr, s ->
        let s : string = Obj.obj s in
        F.pp_print_string fmt s
    | L.PTstr_color, s ->
        let (s: string), (c: Pp.color) = Obj.obj s in
        if Config.write_html then
          F.fprintf fmt "%a%s%a" Io_infer.Html.pp_start_color c s Io_infer.Html.pp_end_color ()
        else F.pp_print_string fmt s
    | L.PTstrln, s ->
        let s : string = Obj.obj s in
        F.fprintf fmt "%s@\n" s
    | L.PTstrln_color, s ->
        let (s: string), (c: Pp.color) = Obj.obj s in
        if Config.write_html then
          F.fprintf fmt "%a%s%a@\n" Io_infer.Html.pp_start_color c s Io_infer.Html.pp_end_color ()
        else F.fprintf fmt "%s@\n" s
    | L.PTsub, sub ->
        let sub : Sil.subst = Obj.obj sub in
        Prop.pp_sub pe_default fmt sub
    | L.PTtexp_full, te ->
        let te : Exp.t = Obj.obj te in
        Sil.pp_texp_full pe_default fmt te
    | L.PTtyp_full, t ->
        let t : Typ.t = Obj.obj t in
        Typ.pp_full pe_default fmt t
    | L.PTtyp_list, tl ->
        let tl : Typ.t list = Obj.obj tl in
        Pp.seq (Typ.pp pe_default) fmt tl
    | L.PTerror, s ->
        let s : string = Obj.obj s in
        if Config.write_html then
          F.fprintf fmt "%aERROR: %s%a" Io_infer.Html.pp_start_color Pp.Red s
            Io_infer.Html.pp_end_color ()
        else F.fprintf fmt "ERROR: %s" s
    | L.PTwarning, s ->
        let s : string = Obj.obj s in
        if Config.write_html then
          F.fprintf fmt "%aWARNING: %s%a" Io_infer.Html.pp_start_color Pp.Orange s
            Io_infer.Html.pp_end_color ()
        else F.fprintf fmt "WARNING: %s" s
    | L.PTinfo, s ->
        let s : string = Obj.obj s in
        if Config.write_html then
          F.fprintf fmt "%aINFO: %s%a" Io_infer.Html.pp_start_color Pp.Blue s
            Io_infer.Html.pp_end_color ()
        else F.fprintf fmt "INFO: %s" s


(** Set printer hook as soon as this module is loaded *)
let () = L.printer_hook := force_delayed_print

(** Execute the delayed print actions *)
let force_delayed_prints () =
  Config.forcing_delayed_prints := true ;
  F.fprintf !curr_html_formatter "@?" ;
  (* flush html stream *)
  List.iter ~f:(force_delayed_print !curr_html_formatter) (List.rev (L.get_delayed_prints ())) ;
  F.fprintf !curr_html_formatter "@?" ;
  L.reset_delayed_prints () ;
  Config.forcing_delayed_prints := false


(** Start a session, and create a new html fine for the node if it does not exist yet *)
let start_session ~pp_name node (loc: Location.t) proc_name session source =
  let node_id = Procdesc.Node.get_id node in
  if
    NodesHtml.start_node
      (node_id :> int)
      loc proc_name (Procdesc.Node.get_preds node) (Procdesc.Node.get_succs node)
      (Procdesc.Node.get_exn node) source
  then
    F.fprintf !curr_html_formatter "%a<LISTING>%a</LISTING>%a" Io_infer.Html.pp_start_color
      Pp.Green
      (Procdesc.Node.pp_instrs (Pp.html Green) None ~sub_instrs:true)
      node Io_infer.Html.pp_end_color () ;
  F.fprintf !curr_html_formatter "%a%a %t" Io_infer.Html.pp_hline ()
    (Io_infer.Html.pp_session_link source ~with_name:true [".."] ~proc_name)
    ((node_id :> int), session, loc.Location.line) pp_name ;
  F.fprintf !curr_html_formatter "<LISTING>%a" Io_infer.Html.pp_start_color Pp.Black


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
    F.fprintf !curr_html_formatter "</LISTING>%a@?" Io_infer.Html.pp_end_color () ;
    let source = (Procdesc.Node.get_loc node).file in
    NodesHtml.finish_node
      (Procdesc.Node.get_proc_name node)
      (Procdesc.Node.get_id node :> int)
      source )


(** Write html file for the procedure.
    The boolean indicates whether to print whole seconds only *)
let write_proc_html pdesc =
  if Config.write_html then (
    let pname = Procdesc.get_proc_name pdesc in
    let source = (Procdesc.get_loc pdesc).file in
    let nodes = List.sort ~compare:Procdesc.Node.compare (Procdesc.get_nodes pdesc) in
    let linenum = (Procdesc.Node.get_loc (List.hd_exn nodes)).Location.line in
    let fd, fmt =
      Io_infer.Html.create (DB.Results_dir.Abs_source_dir source) [Typ.Procname.to_filename pname]
    in
    F.fprintf fmt "<center><h1>Procedure %a</h1></center>@\n"
      (Io_infer.Html.pp_line_link source
         ~text:(Some (Escape.escape_xml (Typ.Procname.to_string pname)))
         [])
      linenum ;
    Pp.seq (pp_node_link [] ~description:true) fmt nodes ;
    match Summary.get pname with
    | None ->
        ()
    | Some summary ->
        Summary.pp_html source Black fmt summary ;
        Io_infer.Html.close (fd, fmt) )


(** Creare a hash table mapping line numbers to the set of errors occurring on that line *)
let create_table_err_per_line err_log =
  let err_per_line = Hashtbl.create 17 in
  let add_err (key: Errlog.err_key) (err_data: Errlog.err_data) =
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


let write_html_proc source proof_cover table_nodes_at_linenum global_err_log proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let process_node n =
    let lnum = (Procdesc.Node.get_loc n).Location.line in
    let curr_nodes = try Hashtbl.find table_nodes_at_linenum lnum with Caml.Not_found -> [] in
    Hashtbl.replace table_nodes_at_linenum lnum (n :: curr_nodes)
  in
  let proc_loc = Procdesc.get_loc proc_desc in
  let process_proc =
    Procdesc.is_defined proc_desc && SourceFile.equal proc_loc.Location.file source
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
        List.iter
          ~f:(fun sp ->
            proof_cover :=
              BiabductionSummary.Visitedset.union sp.BiabductionSummary.visited !proof_cover )
          (Tabulation.get_specs_from_payload summary) ;
        Errlog.update global_err_log (Summary.get_err_log summary) )


(** Create filename.ext.html. *)
let write_html_file linereader filename procs =
  let fname_encoding = DB.source_file_encoding filename in
  let fd, fmt =
    Io_infer.Html.create (DB.Results_dir.Abs_source_dir filename) [".."; fname_encoding]
  in
  let pp_prelude () =
    F.fprintf fmt "<center><h1>File %a </h1></center>@\n<table class=\"code\">@\n" SourceFile.pp
      filename
  in
  let print_one_line proof_cover table_nodes_at_linenum table_err_per_line line_number =
    let line_html =
      match LineReader.from_file_linenum linereader filename line_number with
      | Some line_raw ->
          Escape.escape_xml line_raw
      | None ->
          raise End_of_file
    in
    let nodes_at_linenum =
      try Hashtbl.find table_nodes_at_linenum line_number with Caml.Not_found -> []
    in
    let errors_at_linenum =
      try
        let errset = Hashtbl.find table_err_per_line line_number in
        String.Set.elements errset
      with Caml.Not_found -> []
    in
    F.fprintf fmt "<tr><td class=\"num\" id=\"LINE%d\">%d</td><td class=\"line\">%s " line_number
      line_number line_html ;
    Pp.seq
      (pp_node_link [fname_encoding] ~proof_cover:!proof_cover ~description:true)
      fmt nodes_at_linenum ;
    List.iter
      ~f:(fun n ->
        match Procdesc.Node.get_kind n with
        | Procdesc.Node.Start_node proc_name ->
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
      nodes_at_linenum ;
    List.iter ~f:(pp_err_message fmt) errors_at_linenum ;
    F.fprintf fmt "</td></tr>@\n"
  in
  pp_prelude () ;
  let global_err_log = Errlog.empty () in
  let table_nodes_at_linenum = Hashtbl.create 11 in
  let proof_cover = ref BiabductionSummary.Visitedset.empty in
  List.iter ~f:(write_html_proc filename proof_cover table_nodes_at_linenum global_err_log) procs ;
  let table_err_per_line = create_table_err_per_line global_err_log in
  let linenum = ref 0 in
  try
    while true do
      incr linenum ;
      print_one_line proof_cover table_nodes_at_linenum table_err_per_line !linenum
    done
  with End_of_file ->
    F.fprintf fmt "</table>@\n" ;
    Errlog.pp_html filename [fname_encoding] fmt global_err_log ;
    Io_infer.Html.close (fd, fmt)


(** Create filename.ext.html for each file in the cluster. *)
let write_all_html_files cluster =
  let exe_env = Exe_env.mk cluster in
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
  Exe_env.iter_files
    (fun _ cfg ->
      let source_files_in_cfg, pdescs_in_cfg =
        Typ.Procname.Hash.fold
          (fun _ proc_desc (files, pdescs) ->
            let updated_files =
              if Procdesc.is_defined proc_desc then
                let file = (Procdesc.get_loc proc_desc).Location.file in
                if is_whitelisted file then SourceFile.Set.add file files else files
              else files
            in
            (updated_files, proc_desc :: pdescs) )
          cfg (SourceFile.Set.empty, [])
      in
      SourceFile.Set.iter
        (fun file -> write_html_file linereader file pdescs_in_cfg)
        source_files_in_cfg )
    exe_env
