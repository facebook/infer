(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PolyVariantEqual

module Stats = struct
  type t =
    { failure_kind: SymOp.failure_kind option
          (** what type of failure stopped the analysis (if any) *)
    ; symops: int  (** Number of SymOp's throughout the whole analysis of the function *)
    ; mutable nodes_visited: IntSet.t  (** Nodes visited *) }

  let empty = {failure_kind= None; symops= 0; nodes_visited= IntSet.empty}

  let is_visited stats node_id = IntSet.mem node_id stats.nodes_visited

  let add_visited stats node_id = stats.nodes_visited <- IntSet.add node_id stats.nodes_visited

  let nb_visited {nodes_visited} = IntSet.cardinal nodes_visited

  let update ?(add_symops = 0) ?failure_kind stats =
    let symops = stats.symops + add_symops in
    let failure_kind = match failure_kind with None -> stats.failure_kind | some -> some in
    {stats with symops; failure_kind}


  let failure_kind {failure_kind} = failure_kind

  let symops {symops} = symops

  let pp_failure_kind_opt fmt failure_kind_opt =
    match failure_kind_opt with
    | Some failure_kind ->
        SymOp.pp_failure_kind fmt failure_kind
    | None ->
        F.pp_print_string fmt "NONE"


  let failure_kind_to_string {failure_kind} = F.asprintf "%a" pp_failure_kind_opt failure_kind

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

type t =
  { payloads: Payloads.t
  ; sessions: int ref
  ; stats: Stats.t
  ; status: Status.t
  ; proc_desc: Procdesc.t
  ; err_log: Errlog.t }

let get_status summary = summary.status

let get_proc_desc summary = summary.proc_desc

let get_attributes summary = Procdesc.get_attributes summary.proc_desc

let get_proc_name summary = (get_attributes summary).ProcAttributes.proc_name

let get_ret_type summary = (get_attributes summary).ProcAttributes.ret_type

let get_formals summary = (get_attributes summary).ProcAttributes.formals

let get_err_log summary = summary.err_log

let get_loc summary = (get_attributes summary).ProcAttributes.loc

type cache = t Typ.Procname.Hash.t

let cache : cache = Typ.Procname.Hash.create 128

let clear_cache () = Typ.Procname.Hash.clear cache

let pp_errlog fmt err_log =
  F.fprintf fmt "ERRORS: @[<h>%a@]@\n%!" Errlog.pp_errors err_log ;
  F.fprintf fmt "WARNINGS: @[<h>%a@]" Errlog.pp_warnings err_log


let pp_signature fmt summary =
  let pp_formal fmt (p, typ) = F.fprintf fmt "%a %a" (Typ.pp_full Pp.text) typ Mangled.pp p in
  F.fprintf fmt "%a %a(%a)" (Typ.pp_full Pp.text) (get_ret_type summary) Typ.Procname.pp
    (get_proc_name summary) (Pp.seq ~sep:", " pp_formal) (get_formals summary)


let get_signature summary = F.asprintf "%a" pp_signature summary

let pp_no_stats_specs fmt summary =
  F.fprintf fmt "%a@\n" pp_signature summary ;
  F.fprintf fmt "%a@\n" Status.pp summary.status


let pp_text fmt summary =
  pp_no_stats_specs fmt summary ;
  F.fprintf fmt "%a@\n%a%a" pp_errlog (get_err_log summary) Stats.pp summary.stats
    (Payloads.pp Pp.text) summary.payloads


let pp_html source fmt summary =
  F.pp_force_newline fmt () ;
  Io_infer.Html.with_color Black pp_no_stats_specs fmt summary ;
  F.fprintf fmt "<br />%a<br />@\n" Stats.pp summary.stats ;
  Errlog.pp_html source [] fmt (get_err_log summary) ;
  Io_infer.Html.pp_hline fmt () ;
  F.fprintf fmt "<LISTING>@\n" ;
  Payloads.pp (Pp.html Black) fmt summary.payloads ;
  F.fprintf fmt "</LISTING>@\n"


(** Add the summary to the table for the given function *)
let add (proc_name : Typ.Procname.t) (summary : t) : unit =
  Typ.Procname.Hash.replace cache proc_name summary


let specs_filename pname =
  let pname_file = Typ.Procname.to_filename pname in
  pname_file ^ Config.specs_files_suffix


(** path to the .specs file for the given procedure in the current results directory *)
let res_dir_specs_filename pname =
  DB.Results_dir.path_to_filename DB.Results_dir.Abs_root
    [Config.specs_dir_name; specs_filename pname]


(** paths to the .specs file for the given procedure in the current spec libraries *)
let specs_library_filename specs_dir pname =
  DB.filename_from_string (Filename.concat specs_dir (specs_filename pname))


(** paths to the .specs file for the given procedure in the models folder *)
let specs_models_filename pname =
  DB.filename_from_string (Filename.concat Config.models_dir (specs_filename pname))


let has_model pname = Sys.file_exists (DB.filename_to_string (specs_models_filename pname)) = `Yes

let summary_serializer : t Serialization.serializer =
  Serialization.create_serializer Serialization.Key.summary


(** Load procedure summary from the given file *)
let load_from_file specs_file = Serialization.read_from_file summary_serializer specs_file

(** Load procedure summary for the given procedure name and update spec table *)
let load_summary_to_spec_table =
  let rec or_load_summary_libs specs_dirs proc_name summ_opt =
    match (summ_opt, specs_dirs) with
    | Some _, _ | _, [] ->
        summ_opt
    | None, specs_dir :: specs_dirs ->
        load_from_file (specs_library_filename specs_dir proc_name)
        |> or_load_summary_libs specs_dirs proc_name
  in
  let load_summary_ziplibs zip_specs_filename =
    let zip_specs_path = Filename.concat Config.specs_dir_name zip_specs_filename in
    ZipLib.load summary_serializer zip_specs_path
  in
  let or_from f_load f_filenames proc_name summ_opt =
    match summ_opt with Some _ -> summ_opt | None -> f_load (f_filenames proc_name)
  in
  fun proc_name ->
    let summ_opt =
      load_from_file (res_dir_specs_filename proc_name)
      |> or_from load_from_file specs_models_filename proc_name
      |> or_from load_summary_ziplibs specs_filename proc_name
      |> or_load_summary_libs Config.specs_library proc_name
    in
    Option.iter ~f:(add proc_name) summ_opt ;
    summ_opt


let get proc_name =
  try Some (Typ.Procname.Hash.find cache proc_name) with Caml.Not_found ->
    load_summary_to_spec_table proc_name


(** Check if the procedure is from a library:
    It's not defined, and there is no spec file for it. *)
let proc_is_library proc_attributes =
  if not proc_attributes.ProcAttributes.is_defined then
    match get proc_attributes.ProcAttributes.proc_name with None -> true | Some _ -> false
  else false


(** Try to find the attributes for a defined proc.
    First look at specs (to get attributes computed by analysis)
    then look at the attributes table.
    If no attributes can be found, return None.
*)
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
    (res_dir_specs_filename proc_name)
    ~data:final_summary


let init_summary proc_desc =
  let summary =
    { sessions= ref 0
    ; payloads= Payloads.empty
    ; stats= Stats.empty
    ; status= Status.Pending
    ; proc_desc
    ; err_log= Errlog.empty () }
  in
  Typ.Procname.Hash.replace cache (Procdesc.get_proc_name proc_desc) summary ;
  summary


let dummy =
  let dummy_attributes =
    ProcAttributes.default (SourceFile.invalid __FILE__) Typ.Procname.empty_block
  in
  let dummy_proc_desc = Procdesc.from_proc_attributes dummy_attributes in
  init_summary dummy_proc_desc


(** Reset a summary rebuilding the dependents and preserving the proc attributes if present. *)
let reset proc_desc = init_summary proc_desc

let reset_all ~filter () =
  let reset proc_name =
    let filename = res_dir_specs_filename proc_name in
    Serialization.read_from_file summary_serializer filename
    |> Option.iter ~f:(fun summary ->
           let blank_summary = reset summary.proc_desc in
           Serialization.write_to_file summary_serializer filename ~data:blank_summary )
  in
  Procedures.get_all ~filter () |> List.iter ~f:reset


module SummaryValue = struct
  type nonrec t = t option

  let label = "summary"
end

module SummaryServer = Memcached.Make (SummaryValue)
