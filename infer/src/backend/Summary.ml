(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module CLOpt = CommandLineOption

module Stats = struct
  type t =
    { failure_kind: SymOp.failure_kind option
          (** what type of failure stopped the analysis (if any) *)
    ; symops: int  (** Number of SymOp's throughout the whole analysis of the function *)
    ; mutable nodes_visited: IntSet.t  (** Nodes visited *) }

  let empty = {failure_kind= None; symops= 0; nodes_visited= IntSet.empty}

  let is_visited stats node_id = IntSet.mem node_id stats.nodes_visited

  let add_visited stats node_id = stats.nodes_visited <- IntSet.add node_id stats.nodes_visited

  let update ?(add_symops = 0) ?failure_kind stats =
    let symops = stats.symops + add_symops in
    let failure_kind = match failure_kind with None -> stats.failure_kind | some -> some in
    {stats with symops; failure_kind}


  let pp_failure_kind_opt fmt failure_kind_opt =
    match failure_kind_opt with
    | Some failure_kind ->
        SymOp.pp_failure_kind fmt failure_kind
    | None ->
        F.pp_print_string fmt "NONE"


  let pp fmt {failure_kind; symops} =
    F.fprintf fmt "FAILURE:%a SYMOPS:%d@\n" pp_failure_kind_opt failure_kind symops
end

module Status = struct
  type t =
    | Pending  (** the summary has been created by the procedure has not been analyzed yet *)
    | Analyzed  (** the analysis of the procedure is finished *)

  let to_string = function Pending -> "Pending" | Analyzed -> "Analyzed"

  let pp fmt status = F.pp_print_string fmt (to_string status)

  let is_analyzed = function Analyzed -> true | _ -> false
end

include struct
  (* ignore dead modules added by @@deriving fields *)
  [@@@warning "-60"]

  type t =
    { payloads: Payloads.t
    ; mutable sessions: int
    ; stats: Stats.t
    ; status: Status.t
    ; proc_desc: Procdesc.t
    ; err_log: Errlog.t
    ; mutable callee_pnames: Procname.Set.t }
  [@@deriving fields]
end

let get_status summary = summary.status

let get_proc_desc summary = summary.proc_desc

let get_attributes summary = Procdesc.get_attributes summary.proc_desc

let get_proc_name summary = (get_attributes summary).ProcAttributes.proc_name

let get_ret_type summary = (get_attributes summary).ProcAttributes.ret_type

let get_formals summary = (get_attributes summary).ProcAttributes.formals

let get_err_log summary = summary.err_log

let get_loc summary = (get_attributes summary).ProcAttributes.loc

let pp_errlog fmt err_log =
  F.fprintf fmt "ERRORS: @[<h>%a@]@\n%!" Errlog.pp_errors err_log ;
  F.fprintf fmt "WARNINGS: @[<h>%a@]" Errlog.pp_warnings err_log


let pp_signature fmt summary =
  let pp_formal fmt (p, typ) = F.fprintf fmt "%a %a" (Typ.pp_full Pp.text) typ Mangled.pp p in
  F.fprintf fmt "%a %a(%a)" (Typ.pp_full Pp.text) (get_ret_type summary) Procname.pp
    (get_proc_name summary) (Pp.seq ~sep:", " pp_formal) (get_formals summary)


let pp_no_stats_specs fmt summary =
  F.fprintf fmt "%a@\n" pp_signature summary ;
  F.fprintf fmt "%a@\n" Status.pp summary.status


let pp_text fmt summary =
  pp_no_stats_specs fmt summary ;
  F.fprintf fmt "%a@\n%a%a" pp_errlog (get_err_log summary) Stats.pp summary.stats
    (Payloads.pp Pp.text) summary.payloads


let pp_html source fmt summary =
  F.pp_force_newline fmt () ;
  Pp.html_with_color Black pp_no_stats_specs fmt summary ;
  F.fprintf fmt "<br />%a<br />@\n" Stats.pp summary.stats ;
  Errlog.pp_html source [] fmt (get_err_log summary) ;
  Io_infer.Html.pp_hline fmt () ;
  F.fprintf fmt "<LISTING>@\n" ;
  Payloads.pp (Pp.html Black) fmt summary.payloads ;
  F.fprintf fmt "</LISTING>@\n"


module OnDisk = struct
  type cache = t Procname.Hash.t

  let cache : cache = Procname.Hash.create 128

  let clear_cache () = Procname.Hash.clear cache

  (** Remove an element from the cache of summaries. Contrast to reset which re-initializes a
      summary keeping the same Procdesc and updates the cache accordingly. *)
  let remove_from_cache pname = Procname.Hash.remove cache pname

  (** Add the summary to the table for the given function *)
  let add (proc_name : Procname.t) (summary : t) : unit =
    Procname.Hash.replace cache proc_name summary


  let specs_filename_and_crc pname =
    let pname_file, crc = Procname.to_filename_and_crc pname in
    (pname_file ^ Config.specs_files_suffix, crc)


  let specs_filename pname = specs_filename_and_crc pname |> fst

  (** Return the path to the .specs file for the given procedure in the current results directory *)
  let specs_filename_of_procname pname =
    let filename, crc = specs_filename_and_crc pname in
    let sharded_filename =
      if Serialization.is_shard_mode then
        let shard_dirs =
          String.sub crc ~pos:0 ~len:Config.specs_shard_depth
          |> String.concat_map ~sep:"/" ~f:Char.to_string
        in
        shard_dirs ^/ filename
      else filename
    in
    DB.filename_from_string (ResultsDir.get_path Specs ^/ sharded_filename)


  (** paths to the .specs file for the given procedure in the models folder *)
  let specs_models_filename pname =
    DB.filename_from_string (Filename.concat Config.biabduction_models_dir (specs_filename pname))


  let summary_serializer : t Serialization.serializer =
    Serialization.create_serializer Serialization.Key.summary


  (** Load procedure summary from the given file *)
  let load_from_file specs_file =
    BackendStats.incr_summary_file_try_load () ;
    let opt = Serialization.read_from_file summary_serializer specs_file in
    if Option.is_some opt then BackendStats.incr_summary_read_from_disk () ;
    opt


  (** Load procedure summary for the given procedure name and update spec table *)
  let load_summary_to_spec_table proc_name =
    let summ_opt =
      match load_from_file (specs_filename_of_procname proc_name) with
      | None when BiabductionModels.mem proc_name ->
          load_from_file (specs_models_filename proc_name)
      | summ_opt ->
          summ_opt
    in
    Option.iter ~f:(add proc_name) summ_opt ;
    summ_opt


  let get proc_name =
    match Procname.Hash.find cache proc_name with
    | summary ->
        BackendStats.incr_summary_cache_hits () ;
        Some summary
    | exception Caml.Not_found ->
        BackendStats.incr_summary_cache_misses () ;
        load_summary_to_spec_table proc_name


  (** Try to find the attributes for a defined proc. First look at specs (to get attributes computed
      by analysis) then look at the attributes table. If no attributes can be found, return None. *)
  let proc_resolve_attributes proc_name =
    match get proc_name with
    | Some summary ->
        Some (get_attributes summary)
    | None ->
        Attributes.load proc_name


  (** Save summary for the procedure into the spec database *)
  let store (summ : t) =
    let final_summary = {summ with status= Status.Analyzed} in
    let proc_name = get_proc_name final_summary in
    (* Make sure the summary in memory is identical to the saved one *)
    add proc_name final_summary ;
    Serialization.write_to_file summary_serializer
      (specs_filename_of_procname proc_name)
      ~data:final_summary


  let reset proc_desc =
    let summary =
      { sessions= 0
      ; payloads= Payloads.empty
      ; stats= Stats.empty
      ; status= Status.Pending
      ; proc_desc
      ; err_log= Errlog.empty ()
      ; callee_pnames= Procname.Set.empty }
    in
    Procname.Hash.replace cache (Procdesc.get_proc_name proc_desc) summary ;
    summary


  let reset_all ~filter () =
    let reset proc_name =
      let filename = specs_filename_of_procname proc_name in
      BackendStats.incr_summary_file_try_load () ;
      Serialization.read_from_file summary_serializer filename
      |> Option.iter ~f:(fun summary ->
             BackendStats.incr_summary_read_from_disk () ;
             let blank_summary = reset summary.proc_desc in
             Serialization.write_to_file summary_serializer filename ~data:blank_summary )
    in
    Procedures.get_all ~filter () |> List.iter ~f:reset


  let delete pname =
    let filename = specs_filename_of_procname pname |> DB.filename_to_string in
    (* Unix_error is raised if the file isn't present so do nothing in this case *)
    (try Unix.unlink filename with Unix.Unix_error _ -> ()) ;
    remove_from_cache pname


  (** return the list of the .specs files in the results dir *)
  let load_specfiles () =
    let is_specs_file fname = Filename.check_suffix fname Config.specs_files_suffix in
    let do_file acc path = if is_specs_file path then path :: acc else acc in
    let result_specs_dir = DB.filename_to_string DB.Results_dir.specs_dir in
    Utils.directory_fold do_file [] result_specs_dir


  let print_usage_exit err_s =
    L.user_error "Load Error: %s@\n@." err_s ;
    Config.print_usage_exit ()


  let spec_files_from_cmdline () =
    if CLOpt.is_originator then (
      (* Find spec files specified by command-line arguments.  Not run at init time since the specs
         files may be generated between init and report time. *)
      List.iter
        ~f:(fun arg ->
          if
            (not (Filename.check_suffix arg Config.specs_files_suffix))
            && not (String.equal arg ".")
          then print_usage_exit ("file " ^ arg ^ ": arguments must be .specs files") )
        Config.anon_args ;
      if Config.test_filtering then (
        Inferconfig.test () ;
        L.exit 0 ) ;
      if List.is_empty Config.anon_args then load_specfiles () else List.rev Config.anon_args )
    else load_specfiles ()


  (** Create an iterator which loads spec files one at a time *)
  let summary_iterator spec_files =
    let sorted_spec_files = List.sort ~compare:String.compare (spec_files ()) in
    let do_spec f fname =
      match load_from_file (DB.filename_from_string fname) with
      | None ->
          L.(die UserError) "Error: cannot open file %s@." fname
      | Some summary ->
          f summary
    in
    let iterate f = List.iter ~f:(do_spec f) sorted_spec_files in
    iterate


  let iter_specs_from_config ~f = summary_iterator spec_files_from_cmdline f

  let iter_specs ~f = summary_iterator load_specfiles f

  let pp_specs_from_config fmt =
    iter_specs_from_config ~f:(fun summary ->
        F.fprintf fmt "Procedure: %a@\n%a@." Procname.pp (get_proc_name summary) pp_text summary )
end
