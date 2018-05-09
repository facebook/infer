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
open PolyVariantEqual

(** Specifications and spec table *)

module L = Logging
module F = Format

(* =============== START of support for spec tables =============== *)

(** Module for joined props *)
module Jprop = struct
  (** type aliases for component of t values that compare should ignore *)
  type id_ = int

  let compare_id_ _ _ = 0

  (** Remember when a prop is obtained as the join of two other props; the first parameter is an id *)
  type 'a t = Prop of id_ * 'a Prop.t | Joined of id_ * 'a Prop.t * 'a t * 'a t
  [@@deriving compare]

  (** Comparison for joined_prop *)
  let compare jp1 jp2 = compare (fun _ _ -> 0) jp1 jp2

  (** Return true if the two join_prop's are equal *)
  let equal jp1 jp2 = Int.equal (compare jp1 jp2) 0

  let to_prop = function Prop (_, p) -> p | Joined (_, p, _, _) -> p

  let rec sorted_gen_free_vars tenv =
    let open Sequence.Generator in
    function
      | Prop (_, p) ->
          Prop.dfs_sort tenv p |> Prop.sorted_gen_free_vars
      | Joined (_, p, jp1, jp2) ->
          Prop.dfs_sort tenv p |> Prop.sorted_gen_free_vars
          >>= fun () -> sorted_gen_free_vars tenv jp1 >>= fun () -> sorted_gen_free_vars tenv jp2


  let rec normalize tenv = function
    | Prop (n, p) ->
        Prop (n, Prop.normalize tenv p)
    | Joined (n, p, jp1, jp2) ->
        Joined (n, Prop.normalize tenv p, normalize tenv jp1, normalize tenv jp2)


  (** Return a compact representation of the jprop *)
  let rec compact sh = function
    | Prop (n, p) ->
        Prop (n, Prop.prop_compact sh p)
    | Joined (n, p, jp1, jp2) ->
        Joined (n, Prop.prop_compact sh p, compact sh jp1, compact sh jp2)


  (** Print the toplevel prop *)
  let pp_short pe f jp = Prop.pp_prop pe f (to_prop jp)

  (** Dump the toplevel prop *)
  let d_shallow (jp: Prop.normal t) = L.add_print_action (L.PTjprop_short, Obj.repr jp)

  (** Get identifies of the jprop *)
  let get_id = function Prop (n, _) -> n | Joined (n, _, _, _) -> n

  (** Print a list of joined props, the boolean indicates whether to print subcomponents of joined props *)
  let pp_list pe shallow f jplist =
    let rec pp_seq_newline f = function
      | [] ->
          ()
      | [Prop (n, p)] ->
          F.fprintf f "PROP %d:@\n%a" n (Prop.pp_prop pe) p
      | [Joined (n, p, p1, p2)] ->
          if not shallow then F.fprintf f "%a@\n" pp_seq_newline [p1] ;
          if not shallow then F.fprintf f "%a@\n" pp_seq_newline [p2] ;
          F.fprintf f "PROP %d (join of %d,%d):@\n%a" n (get_id p1) (get_id p2) (Prop.pp_prop pe) p
      | jp :: l ->
          F.fprintf f "%a@\n" pp_seq_newline [jp] ;
          pp_seq_newline f l
    in
    pp_seq_newline f jplist


  (** dump a joined prop list, the boolean indicates whether to print toplevel props only *)
  let d_list (shallow: bool) (jplist: Prop.normal t list) =
    L.add_print_action (L.PTjprop_list, Obj.repr (shallow, jplist))


  let rec gen_free_vars =
    let open Sequence.Generator in
    function
      | Prop (_, p) ->
          Prop.gen_free_vars p
      | Joined (_, p, jp1, jp2) ->
          Prop.gen_free_vars p >>= fun () -> gen_free_vars jp1 >>= fun () -> gen_free_vars jp2


  let free_vars jp = Sequence.Generator.run (gen_free_vars jp)

  let rec jprop_sub sub = function
    | Prop (n, p) ->
        Prop (n, Prop.prop_sub sub p)
    | Joined (n, p, jp1, jp2) ->
        let p' = Prop.prop_sub sub p in
        let jp1' = jprop_sub sub jp1 in
        let jp2' = jprop_sub sub jp2 in
        Joined (n, p', jp1', jp2')


  let filter (f: 'a t -> 'b option) jpl =
    let rec do_filter acc = function
      | [] ->
          acc
      | (Prop _ as jp) :: jpl -> (
        match f jp with Some x -> do_filter (x :: acc) jpl | None -> do_filter acc jpl )
      | (Joined (_, _, jp1, jp2) as jp) :: jpl ->
        match f jp with
        | Some x ->
            do_filter (x :: acc) jpl
        | None ->
            do_filter acc (jpl @ [jp1; jp2])
    in
    do_filter [] jpl


  let rec map (f: 'a Prop.t -> 'b Prop.t) = function
    | Prop (n, p) ->
        Prop (n, f p)
    | Joined (n, p, jp1, jp2) ->
        Joined (n, f p, map f jp1, map f jp2)

  (*
  let rec jprop_sub sub = function
    | Prop (n, p) -> Prop (n, Prop.prop_sub sub p)
    | Joined (n, p, jp1, jp2) ->
        Joined (n, Prop.prop_sub sub p, jprop_sub sub jp1, jprop_sub sub jp2)
*)
end

(***** End of module Jprop *****)

module Visitedset = Caml.Set.Make (struct
  type t = Procdesc.Node.id * int list

  let compare (node_id1, _) (node_id2, _) = Procdesc.Node.compare_id node_id1 node_id2
end)

let visited_str vis =
  let s = ref "" in
  let lines = ref Int.Set.empty in
  let do_one (_, ns) =
    (* if List.length ns > 1 then
       begin
       let ss = ref "" in
       List.iter ~f:(fun n -> ss := !ss ^ " " ^ string_of_int n) ns;
       L.out "Node %d has lines %s@." node !ss
       end; *)
    List.iter ~f:(fun n -> lines := Int.Set.add !lines n) ns
  in
  Visitedset.iter do_one vis ;
  Int.Set.iter ~f:(fun n -> s := !s ^ " " ^ string_of_int n) !lines ;
  !s


(** A spec consists of:
    pre: a joined prop
    post: a list of props with path
    visited: a list of pairs (node_id, line) for the visited nodes *)
type 'a spec = {pre: 'a Jprop.t; posts: ('a Prop.t * Paths.Path.t) list; visited: Visitedset.t}

(** encapsulate type for normalized specs *)
module NormSpec : sig
  type t

  val normalize : Tenv.t -> Prop.normal spec -> t

  val tospecs : t list -> Prop.normal spec list

  val compact : Sil.sharing_env -> t -> t
  (** Return a compact representation of the spec *)

  val erase_join_info_pre : Tenv.t -> t -> t
  (** Erase join info from pre of spec *)
end = struct
  type t = Prop.normal spec

  let tospecs specs = specs

  let gen_free_vars tenv (spec: Prop.normal spec) =
    let open Sequence.Generator in
    Jprop.sorted_gen_free_vars tenv spec.pre
    >>= fun () ->
    ISequence.gen_sequence_list spec.posts ~f:(fun (p, _) ->
        Prop.dfs_sort tenv p |> Prop.sorted_gen_free_vars )


  let free_vars tenv spec = Sequence.Generator.run (gen_free_vars tenv spec)

  let spec_sub tenv sub spec =
    { pre= Jprop.normalize tenv (Jprop.jprop_sub sub spec.pre)
    ; posts=
        List.map ~f:(fun (p, path) -> (Prop.normalize tenv (Prop.prop_sub sub p), path)) spec.posts
    ; visited= spec.visited }


  (** Convert spec into normal form w.r.t. variable renaming *)
  let normalize tenv (spec: Prop.normal spec) : Prop.normal spec =
    let idlist = free_vars tenv spec |> Ident.hashqueue_of_sequence |> Ident.HashQueue.keys in
    let count = ref 0 in
    let sub =
      Sil.subst_of_list
        (List.map
           ~f:(fun id -> incr count ; (id, Exp.Var (Ident.create_normal Ident.name_spec !count)))
           idlist)
    in
    spec_sub tenv sub spec


  (** Return a compact representation of the spec *)
  let compact sh spec =
    let pre = Jprop.compact sh spec.pre in
    let posts = List.map ~f:(fun (p, path) -> (Prop.prop_compact sh p, path)) spec.posts in
    {pre; posts; visited= spec.visited}


  (** Erase join info from pre of spec *)
  let erase_join_info_pre tenv spec =
    let spec' = {spec with pre= Jprop.Prop (1, Jprop.to_prop spec.pre)} in
    normalize tenv spec'
end

(** Convert spec into normal form w.r.t. variable renaming *)
let spec_normalize = NormSpec.normalize

(** Cast a list of normalized specs to a list of specs *)
let normalized_specs_to_specs = NormSpec.tospecs

(** Execution statistics *)
type stats =
  { stats_failure: SymOp.failure_kind option
        (** what type of failure stopped the analysis (if any) *)
  ; symops: int  (** Number of SymOp's throughout the whole analysis of the function *)
  ; mutable nodes_visited_fp: IntSet.t  (** Nodes visited during the footprint phase *)
  ; mutable nodes_visited_re: IntSet.t  (** Nodes visited during the re-execution phase *) }

type status = Pending | Analyzed [@@deriving compare]

let string_of_status = function Pending -> "Pending" | Analyzed -> "Analyzed"

let pp_status fmt status = F.pp_print_string fmt (string_of_status status)

let equal_status = [%compare.equal : status]

type phase = FOOTPRINT | RE_EXECUTION [@@deriving compare]

let equal_phase = [%compare.equal : phase]

(** Payload: results of some analysis *)
type payload =
  { annot_map: AnnotReachabilityDomain.astate option
  ; buffer_overrun: BufferOverrunDomain.Summary.t option
  ; crashcontext_frame: Stacktree_t.stacktree option
  ; litho: LithoDomain.astate option
  ; preposts: NormSpec.t list option
  ; quandary: QuandarySummary.t option
  ; racerd: RacerDDomain.summary option
  ; resources: ResourceLeakDomain.summary option
  ; siof: SiofDomain.astate option
  ; typestate: unit TypeState.t option
  ; uninit: UninitDomain.summary option
  ; cost: CostDomain.summary option
  ; starvation: StarvationDomain.summary option }

type summary =
  { phase: phase  (** in FOOTPRINT phase or in RE_EXECUTION PHASE *)
  ; payload: payload  (** payload containing the result of some analysis *)
  ; sessions: int ref  (** Session number: how many nodes went trough symbolic execution *)
  ; stats: stats  (** statistics: execution time and list of errors *)
  ; status: status  (** Analysis status of the procedure *)
  ; proc_desc: Procdesc.t  (** Proc desc of the procdure *) }

let get_status summary = summary.status

let get_proc_desc summary = summary.proc_desc

let get_attributes summary = Procdesc.get_attributes summary.proc_desc

let get_proc_name summary = (get_attributes summary).ProcAttributes.proc_name

let get_ret_type summary = (get_attributes summary).ProcAttributes.ret_type

let get_formals summary = (get_attributes summary).ProcAttributes.formals

let get_err_log summary = (get_attributes summary).ProcAttributes.err_log

let get_loc summary = (get_attributes summary).ProcAttributes.loc

(** Return the current phase for the proc *)
let get_phase summary = summary.phase

type spec_tbl = summary Typ.Procname.Hash.t

let spec_tbl : spec_tbl = Typ.Procname.Hash.create 128

let clear_spec_tbl () = Typ.Procname.Hash.clear spec_tbl

let pp_failure_kind_opt fmt failure_kind_opt =
  match failure_kind_opt with
  | Some failure_kind ->
      SymOp.pp_failure_kind fmt failure_kind
  | None ->
      F.pp_print_string fmt "NONE"


let pp_errlog fmt err_log =
  F.fprintf fmt "ERRORS: @[<h>%a@]@\n%!" Errlog.pp_errors err_log ;
  F.fprintf fmt "WARNINGS: @[<h>%a@]" Errlog.pp_warnings err_log


let pp_stats fmt stats =
  F.fprintf fmt "FAILURE:%a SYMOPS:%d@\n" pp_failure_kind_opt stats.stats_failure stats.symops


(** Print the spec *)
let pp_spec pe num_opt fmt spec =
  let num_str =
    match num_opt with
    | None ->
        "----------"
    | Some (n, tot) ->
        Format.sprintf "%d of %d [nvisited:%s]" n tot (visited_str spec.visited)
  in
  let pre = Jprop.to_prop spec.pre in
  let pe_post = Prop.prop_update_obj_sub pe pre in
  let post_list = List.map ~f:fst spec.posts in
  match pe.Pp.kind with
  | TEXT ->
      F.fprintf fmt "--------------------------- %s ---------------------------@\n" num_str ;
      F.fprintf fmt "PRE:@\n%a@\n" (Prop.pp_prop Pp.text) pre ;
      F.fprintf fmt "%a@\n" (Propgraph.pp_proplist pe_post "POST" (pre, true)) post_list ;
      F.pp_print_string fmt "----------------------------------------------------------------"
  | HTML ->
      F.fprintf fmt "--------------------------- %s ---------------------------@\n" num_str ;
      F.fprintf fmt "PRE:@\n%a%a%a@\n" Io_infer.Html.pp_start_color Pp.Blue
        (Prop.pp_prop (Pp.html Blue))
        pre Io_infer.Html.pp_end_color () ;
      (Propgraph.pp_proplist pe_post "POST" (pre, true)) fmt post_list ;
      F.pp_print_string fmt "----------------------------------------------------------------"


(** Dump a spec *)
let d_spec (spec: 'a spec) = L.add_print_action (L.PTspec, Obj.repr spec)

let pp_specs pe fmt specs =
  let total = List.length specs in
  let cnt = ref 0 in
  match pe.Pp.kind with
  | TEXT ->
      List.iter
        ~f:(fun spec ->
          incr cnt ;
          (pp_spec pe (Some (!cnt, total))) fmt spec )
        specs
  | HTML ->
      List.iter
        ~f:(fun spec ->
          incr cnt ;
          F.fprintf fmt "%a<br>@\n" (pp_spec pe (Some (!cnt, total))) spec )
        specs


let describe_phase summary =
  ("Phase", if equal_phase summary.phase FOOTPRINT then "FOOTPRINT" else "RE_EXECUTION")


(** Return the signature of a procedure declaration as a string *)
let get_signature summary =
  let s = ref "" in
  List.iter
    ~f:(fun (p, typ) ->
      let pp f = F.fprintf f "%a %a" (Typ.pp_full Pp.text) typ Mangled.pp p in
      let decl = F.asprintf "%t" pp in
      s := if String.equal !s "" then decl else !s ^ ", " ^ decl )
    (get_formals summary) ;
  F.asprintf "%a %a(%s)" (Typ.pp_full Pp.text) (get_ret_type summary) Typ.Procname.pp
    (get_proc_name summary) !s


let get_specs_from_preposts preposts = Option.value_map ~f:NormSpec.tospecs ~default:[] preposts

let get_specs_from_payload summary = get_specs_from_preposts summary.payload.preposts

let pp_summary_no_stats_specs fmt summary =
  let pp_pair fmt (x, y) = F.fprintf fmt "%s: %s" x y in
  F.fprintf fmt "%s@\n" (get_signature summary) ;
  F.fprintf fmt "%a@\n" pp_status summary.status ;
  F.fprintf fmt "%a@\n" pp_pair (describe_phase summary)


let pp_payload pe fmt
    { preposts
    ; typestate
    ; crashcontext_frame
    ; quandary
    ; siof
    ; racerd
    ; litho
    ; buffer_overrun
    ; annot_map
    ; uninit
    ; cost
    ; starvation } =
  let pp_opt prefix pp fmt = function
    | Some x ->
        F.fprintf fmt "%s: %a@\n" prefix pp x
    | None ->
        ()
  in
  F.fprintf fmt "%a%a%a%a%a%a%a%a%a%a%a%a@\n"
    (pp_opt "PrePosts" (pp_specs pe))
    (Option.map ~f:NormSpec.tospecs preposts)
    (pp_opt "TypeState" (TypeState.pp TypeState.unit_ext))
    typestate
    (pp_opt "CrashContext" Crashcontext.pp_stacktree)
    crashcontext_frame
    (pp_opt "Quandary" QuandarySummary.pp)
    quandary (pp_opt "Siof" SiofDomain.pp) siof
    (pp_opt "RacerD" RacerDDomain.pp_summary)
    racerd (pp_opt "Litho" LithoDomain.pp) litho
    (pp_opt "BufferOverrun" BufferOverrunDomain.Summary.pp)
    buffer_overrun
    (pp_opt "AnnotationReachability" AnnotReachabilityDomain.pp)
    annot_map
    (pp_opt "Uninitialised" UninitDomain.pp_summary)
    uninit
    (pp_opt "Cost" CostDomain.pp_summary)
    cost
    (pp_opt "Starvation" StarvationDomain.pp_summary)
    starvation


let pp_summary_text fmt summary =
  let pe = Pp.text in
  pp_summary_no_stats_specs fmt summary ;
  F.fprintf fmt "%a@\n%a%a" pp_errlog (get_err_log summary) pp_stats summary.stats (pp_payload pe)
    summary.payload


let pp_summary_html source color fmt summary =
  let pe = Pp.html color in
  Io_infer.Html.pp_start_color fmt Black ;
  F.fprintf fmt "@\n%a" pp_summary_no_stats_specs summary ;
  Io_infer.Html.pp_end_color fmt () ;
  F.fprintf fmt "<br />%a<br />@\n" pp_stats summary.stats ;
  Errlog.pp_html source [] fmt (get_err_log summary) ;
  Io_infer.Html.pp_hline fmt () ;
  F.fprintf fmt "<LISTING>@\n" ;
  pp_payload pe fmt summary.payload ;
  F.fprintf fmt "</LISTING>@\n"


let empty_stats =
  {stats_failure= None; symops= 0; nodes_visited_fp= IntSet.empty; nodes_visited_re= IntSet.empty}


let payload_compact sh payload =
  match payload.preposts with
  | Some specs ->
      {payload with preposts= Some (List.map ~f:(NormSpec.compact sh) specs)}
  | None ->
      payload


(** Return a compact representation of the summary *)
let summary_compact sh summary = {summary with payload= payload_compact sh summary.payload}

(** Add the summary to the table for the given function *)
let add_summary (proc_name: Typ.Procname.t) (summary: summary) : unit =
  Typ.Procname.Hash.replace spec_tbl proc_name summary


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


let summary_exists_in_models pname =
  Sys.file_exists (DB.filename_to_string (specs_models_filename pname)) = `Yes


let summary_serializer : summary Serialization.serializer =
  Serialization.create_serializer Serialization.Key.summary


(** Load procedure summary from the given file *)
let load_summary specs_file = Serialization.read_from_file summary_serializer specs_file

(** Load procedure summary for the given procedure name and update spec table *)
let load_summary_to_spec_table =
  let rec or_load_summary_libs specs_dirs proc_name summ_opt =
    match (summ_opt, specs_dirs) with
    | Some _, _ | _, [] ->
        summ_opt
    | None, specs_dir :: specs_dirs ->
        load_summary (specs_library_filename specs_dir proc_name)
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
      None |> or_from load_summary res_dir_specs_filename proc_name
      |> or_from load_summary specs_models_filename proc_name
      |> or_from load_summary_ziplibs specs_filename proc_name
      |> or_load_summary_libs Config.specs_library proc_name
    in
    Option.iter ~f:(add_summary proc_name) summ_opt ;
    summ_opt


let get_summary proc_name =
  try Some (Typ.Procname.Hash.find spec_tbl proc_name) with Caml.Not_found ->
    load_summary_to_spec_table proc_name


let get_summary_unsafe s proc_name =
  match get_summary proc_name with
  | None ->
      L.(die InternalError)
        "[%s] Specs.get_summary_unsafe: %a Not found" s Typ.Procname.pp proc_name
  | Some summary ->
      summary


(** Check if the procedure is from a library:
    It's not defined, and there is no spec file for it. *)
let proc_is_library proc_attributes =
  if not proc_attributes.ProcAttributes.is_defined then
    match get_summary proc_attributes.ProcAttributes.proc_name with
    | None ->
        true
    | Some _ ->
        false
  else false


(** Try to find the attributes for a defined proc.
    First look at specs (to get attributes computed by analysis)
    then look at the attributes table.
    If no attributes can be found, return None.
*)
let proc_resolve_attributes proc_name =
  match get_summary proc_name with
  | Some summary ->
      Some (get_attributes summary)
  | None ->
      Attributes.load proc_name


(** Like proc_resolve_attributes but start from a proc_desc. *)
let pdesc_resolve_attributes proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  match proc_resolve_attributes proc_name with
  | Some proc_attributes ->
      proc_attributes
  | None ->
      (* this should not happen *)
      assert false


(** Save summary for the procedure into the spec database *)
let store_summary (summ1: summary) =
  let summ2 =
    if Config.save_compact_summaries then summary_compact (Sil.create_sharing_env ()) summ1
    else summ1
  in
  let final_summary = {summ2 with status= Analyzed} in
  let proc_name = get_proc_name final_summary in
  (* Make sure the summary in memory is identical to the saved one *)
  add_summary proc_name final_summary ;
  Serialization.write_to_file summary_serializer
    (res_dir_specs_filename proc_name)
    ~data:final_summary


let empty_payload =
  { preposts= None
  ; typestate= None
  ; annot_map= None
  ; crashcontext_frame= None
  ; quandary= None
  ; resources= None
  ; siof= None
  ; racerd= None
  ; litho= None
  ; buffer_overrun= None
  ; uninit= None
  ; cost= None
  ; starvation= None }


(** [init_summary (depend_list, nodes,
    proc_flags, calls, in_out_calls_opt, proc_attributes)]
    initializes the summary for [proc_name] given dependent procs in list [depend_list]. *)
let init_summary proc_desc =
  let summary =
    { phase= FOOTPRINT
    ; sessions= ref 0
    ; payload= empty_payload
    ; stats= empty_stats
    ; status= Pending
    ; proc_desc }
  in
  Typ.Procname.Hash.replace spec_tbl (Procdesc.get_proc_name proc_desc) summary ;
  summary


let dummy =
  let dummy_attributes = ProcAttributes.default Typ.Procname.empty_block in
  let dummy_proc_desc = Procdesc.from_proc_attributes dummy_attributes in
  init_summary dummy_proc_desc


(** Reset a summary rebuilding the dependents and preserving the proc attributes if present. *)
let reset_summary proc_desc = init_summary proc_desc

(* =============== END of support for spec tables =============== *)
