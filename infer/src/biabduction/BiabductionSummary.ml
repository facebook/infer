(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Specifications and spec table *)

module L = Logging
module F = Format

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
  let d_shallow (jp : Prop.normal t) = L.d_pp_with_pe pp_short jp

  (** Get identifies of the jprop *)
  let get_id = function Prop (n, _) -> n | Joined (n, _, _, _) -> n

  (** Print a list of joined props, the boolean indicates whether to print subcomponents of joined props *)
  let pp_list pe ~shallow f jplist =
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
  let d_list ~(shallow : bool) (jplist : Prop.normal t list) =
    L.d_pp_with_pe (pp_list ~shallow) jplist


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


  let filter (f : 'a t -> 'b option) jpl =
    let rec do_filter acc = function
      | [] ->
          acc
      | (Prop _ as jp) :: jpl -> (
        match f jp with Some x -> do_filter (x :: acc) jpl | None -> do_filter acc jpl )
      | (Joined (_, _, jp1, jp2) as jp) :: jpl -> (
        match f jp with
        | Some x ->
            do_filter (x :: acc) jpl
        | None ->
            do_filter acc (jpl @ [jp1; jp2]) )
    in
    do_filter [] jpl


  let rec map (f : 'a Prop.t -> 'b Prop.t) = function
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

module Visitedset = struct
  include Caml.Set.Make (struct
    type t = Procdesc.Node.id * int list

    let compare (node_id1, _) (node_id2, _) = Procdesc.Node.compare_id node_id1 node_id2
  end)

  let pp fmt visitedset =
    let collect_lines (_, ns) acc = List.fold ns ~f:Int.Set.add ~init:acc in
    let lines = fold collect_lines visitedset Int.Set.empty in
    Pp.seq F.pp_print_int fmt (Int.Set.elements lines)
end

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

  let gen_free_vars tenv (spec : Prop.normal spec) =
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
  let normalize tenv (spec : Prop.normal spec) : Prop.normal spec =
    let idlist = free_vars tenv spec |> Ident.hashqueue_of_sequence |> Ident.HashQueue.keys in
    let count = ref 0 in
    let sub =
      Sil.subst_of_list
        (List.map
           ~f:(fun id ->
             incr count ;
             (id, Exp.Var (Ident.create_normal Ident.name_spec !count)) )
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

type phase = FOOTPRINT | RE_EXECUTION [@@deriving compare]

let equal_phase = [%compare.equal: phase]

let string_of_phase = function FOOTPRINT -> "FOOTPRINT" | RE_EXECUTION -> "RE_EXECUTION"

let string_of_phase_short = function FOOTPRINT -> "FP" | RE_EXECUTION -> "RE"

(** Print the spec *)
let pp_spec0 pe num_opt fmt spec =
  let pp_num_opt fmt = function
    | None ->
        F.pp_print_string fmt "----------"
    | Some (n, tot) ->
        F.fprintf fmt "%d of %d [nvisited:%a]" n tot Visitedset.pp spec.visited
  in
  let pre = Jprop.to_prop spec.pre in
  let pe_post = Prop.prop_update_obj_sub pe pre in
  let post_list = List.map ~f:fst spec.posts in
  match pe.Pp.kind with
  | TEXT ->
      F.fprintf fmt "--------------------------- %a ---------------------------@\n" pp_num_opt
        num_opt ;
      F.fprintf fmt "PRE:@\n%a@\n" (Prop.pp_prop Pp.text) pre ;
      F.fprintf fmt "%a@\n" (Propgraph.pp_proplist pe_post "POST" (pre, true)) post_list ;
      F.pp_print_string fmt "----------------------------------------------------------------"
  | HTML ->
      F.fprintf fmt "--------------------------- %a ---------------------------@\n" pp_num_opt
        num_opt ;
      F.fprintf fmt "PRE:@\n" ;
      Io_infer.Html.with_color Blue (Prop.pp_prop (Pp.html Blue)) fmt pre ;
      F.pp_force_newline fmt () ;
      Propgraph.pp_proplist pe_post "POST" (pre, true) fmt post_list ;
      F.pp_print_string fmt "----------------------------------------------------------------"


let pp_spec f spec = pp_spec0 (if Config.write_html then Pp.html Blue else Pp.text) None f spec

let pp_specs pe fmt specs =
  let total = List.length specs in
  match pe.Pp.kind with
  | TEXT ->
      List.iteri specs ~f:(fun cnt spec -> pp_spec0 pe (Some (cnt + 1, total)) fmt spec)
  | HTML ->
      List.iteri specs ~f:(fun cnt spec ->
          F.fprintf fmt "%a<br>@\n" (pp_spec0 pe (Some (cnt + 1, total))) spec )


let get_specs_from_preposts preposts = Option.value_map ~f:NormSpec.tospecs ~default:[] preposts

type t = {preposts: NormSpec.t list; phase: phase}

let opt_get_phase = function None -> FOOTPRINT | Some {phase} -> phase

let pp pe fmt {preposts; phase} =
  F.fprintf fmt "phase= %s@\n%a" (string_of_phase phase) (pp_specs pe) (NormSpec.tospecs preposts)
