(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** NOTE: If performance needs to be improved, especially memory, consider using [ProcCfg.Normal].
    One node per instruction leads to bigger invariant maps, and more states (with less than perfect
    sharing of memory). *)
module CFG = ProcCfg.NormalOneInstrPerNode

module PPNode = struct
  include CFG.Node

  let pp fmt node = pp_id fmt (id node)

  let dummy () : t =
    (* This is to fulfill ocamlgraph interface types, it should not appear in the graph. Otherwise
       it's a lineage implementation bug, meaning some code is adding edges without specifying
       a label. *)
    let dummy_procname =
      Procname.make_erlang ~module_name:"__infer__lineage__" ~function_name:"__dummy__" ~arity:0
    in
    let dummy_instr_index = -42 in
    (Procdesc.Node.dummy dummy_procname, dummy_instr_index)


  let procname t = Procdesc.Node.get_proc_name (underlying_node t)
end

open LineageShape.StdModules
module Shapes = LineageShape.Summary

type shapes = Shapes.t option

(** For easier nested-tuples: [a & b & c] is [(a, (b, c))] *)
let ( & ) x y = (x, y)

module F = struct
  (* Pretty printing common utilities *)

  let arg = Fmt.fmt "$arg%d"

  let ret = Fmt.any "$ret"

  let captured = Fmt.fmt "$cap%d"

  let fun_ = Fmt.any "fun"

  let self = Fmt.any "self"

  let in_procname pp_v fmt (proc, v) = Fmt.pf fmt "%a.%a" Procname.pp_verbose proc pp_v v

  let with_path pp_v fmt (v, path) = Fmt.pf fmt "%a%a" pp_v v FieldPath.pp path

  let arg_path fmt = with_path arg fmt

  let ret_path fmt path = with_path ret fmt (() & path)
end

module Local = struct
  module T = struct
    type t =
      | ConstantAtom of string
      | ConstantInt of string
      | ConstantString of string
      | Cell of (Cell.t[@sexp.opaque])
    [@@deriving compare, equal, sexp, hash]
  end

  include T
  include Comparable.Make (T)

  let pp fmt local =
    match local with
    | ConstantAtom atom_name ->
        Fmt.string fmt atom_name
    | ConstantInt digits ->
        Fmt.string fmt digits
    | ConstantString s ->
        Fmt.string fmt s
    | Cell cell ->
        Cell.pp fmt cell


  module Set = struct
    include Set

    (** Shortcut for adding a Cell-variant local *)
    let add_cell set cell = add set (Cell cell)
  end
end

module Vertex = struct
  module T = struct
    type t =
      | Local of ((Local.t * PPNode.t)[@sexp.opaque])
      | Argument of int * FieldPath.t
      | ArgumentOf of (Procname.t[@sexp.opaque]) * int * FieldPath.t
          (** For lack of reliable information, the fieldpath contains the path of the argument as
              it is set by the current procedure. It may correspond to differently-pathed nodes in
              the Lineage graph of the corresponding callee. *)
      | Captured of int
      | CapturedBy of (Procname.t[@sexp.opaque]) * int
      | Return of FieldPath.t
      | ReturnOf of (Procname.t[@sexp.opaque]) * FieldPath.t
          (** FieldPath component: see ArgumentOf. *)
      | Self
      | Function of (Procname.t[@sexp.opaque])
    [@@deriving compare, equal, sexp, hash]
  end

  include T
  include Comparable.Make (T)

  let is_abstract_cell = function Local (Cell cell, _node) -> Cell.is_abstract cell | _ -> false

  let cell_local node cell = Local (Cell cell, node)

  let pp fmt vertex =
    match vertex with
    | Local (local, node) ->
        Format.fprintf fmt "%a@@%a" Local.pp local PPNode.pp node
    | Argument (index, field_path) ->
        F.arg_path fmt (index & field_path)
    | Captured index ->
        F.captured fmt index
    | Return field_path ->
        F.ret_path fmt field_path
    | CapturedBy (proc_name, index) ->
        F.in_procname F.captured fmt (proc_name & index)
    | ArgumentOf (proc_name, index, field_path) ->
        F.in_procname F.arg_path fmt (proc_name & index & field_path)
    | ReturnOf (proc_name, field_path) ->
        F.in_procname F.ret_path fmt (proc_name & field_path)
    | Function proc_name ->
        F.in_procname F.fun_ fmt (proc_name & ())
    | Self ->
        F.self fmt ()
end

module Edge = struct
  module Kind = struct
    module T = struct
      (** - INV1. There is no direct edge from ReturnOf to ArgumentOf. In that case a Return edge is
            followed by a Call edge, with a Local in the middle (which may be a temporary variable).
          - INV2: There is no loop, because they don't mean anything. *)
      type t =
        | Direct  (** Immediate copy; e.g., assigment or passing an argument *)
        | Call  (** Target is ArgumentOf *)
        | Return  (** Source is ReturnOf *)
        | Capture  (** [X=1, F=fun()->X end] has Capture edge from X to F *)
        | Builtin  (** Edge coming from a suppressed builtin call, ultimately exported as a Copy *)
        | Summary of {callee: Procname.t; shape_is_preserved: bool}
            (** Summarizes the effect of a procedure call *)
        | DynamicCallFunction
        | DynamicCallModule
      [@@deriving compare, equal, sexp, variants]
    end

    include T
    include Comparable.Make (T)

    let to_rank = Variants.to_rank

    let pp fmt kind =
      match kind with
      | Capture ->
          Format.fprintf fmt "Capture"
      | Direct ->
          Format.fprintf fmt "Direct"
      | Call ->
          Format.fprintf fmt "Call"
      | Return ->
          Format.fprintf fmt "Return"
      | Builtin ->
          Format.fprintf fmt "Builtin"
      | Summary {shape_is_preserved; callee} ->
          Format.fprintf fmt "Summary(%a)" Procname.pp callee ;
          if not shape_is_preserved then Format.fprintf fmt "#"
      | DynamicCallFunction ->
          Format.fprintf fmt "DynamicCallFunction"
      | DynamicCallModule ->
          Format.fprintf fmt "DynamicCallModule"
  end

  type kind = Kind.t [@@deriving compare, equal, sexp]

  type t = {kind: kind; node: (PPNode.t[@sexp.opaque])} [@@deriving compare, equal, sexp, fields]

  let default = {kind= Direct; node= PPNode.dummy ()}

  let pp fmt {kind; node} = Fmt.pf fmt "%a@@%a" Kind.pp kind PPNode.pp node

  let pp_e fmt (src, edge, dst) =
    Format.fprintf fmt "@[<2>[%a@ ->@ %a@ (%a)]@]@;" Vertex.pp src Vertex.pp dst pp edge


  let location {node; _} = PPNode.loc node

  let procname {node; _} = PPNode.procname node
end

module G = struct
  (** INV: Constants occur only as sources of edges. NOTE: Constants are "local" because type [data]
      associates them with a location, in a proc. *)

  module Impl = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Vertex) (Edge)
  include Impl

  let pp fmt graph =
    Format.fprintf fmt
      "@[<v 2>LineageGraph:@ @[<hov2>%d vertices@ @[%a@]@]@;@[<v2>%d edges@ @[%a@]@]@]@;"
      (nb_vertex graph)
      (Fmt.iter ~sep:Fmt.sp iter_vertex Vertex.pp)
      graph (nb_edges graph) (Fmt.iter iter_edges_e Edge.pp_e) graph


  let of_vertices vertices = List.fold ~init:empty ~f:add_vertex vertices

  let preserves_shape ((src, {kind; _}, dst) : E.t) =
    match kind with
    | Direct ->
        Fn.non Vertex.is_abstract_cell src && Fn.non Vertex.is_abstract_cell dst
    | Builtin ->
        (* Builtins are considered as unknown functions for shape preservation purposes.
           TODO T154077173: does this introduce undesirable imprecision? *)
        false
    | Call ->
        (* Target is ArgumentOf *)
        Fn.non Vertex.is_abstract_cell src
    | Return ->
        (* Source is ReturnOf *)
        Fn.non Vertex.is_abstract_cell dst
    | Summary {shape_is_preserved} ->
        shape_is_preserved
    | Capture ->
        false (* TODO T154077173: investigate if more precision is worthwile *)
    | DynamicCallFunction | DynamicCallModule ->
        false (* TODO T154077173: model as a call? *)
end

module Unified = struct
  module UVertex = struct
    type v =
      | Local of Local.t * (PPNode.t[@sexp.opaque])
      | Argument of int * FieldPath.t
      | Return of FieldPath.t
      | Captured of int
      | Function
    [@@deriving sexp, compare, equal, hash]

    let pp_v fmt v =
      match v with
      | Local (local, _node) ->
          Local.pp fmt local
      | Argument (index, path) ->
          F.arg_path fmt (index, path)
      | Return path ->
          F.ret_path fmt path
      | Captured index ->
          F.captured fmt index
      | Function ->
          F.fun_ fmt ()


    type t = {procname: Procname.t; vertex: v} [@@deriving sexp, compare, equal, hash]

    let pp fmt {procname; vertex} = F.in_procname pp_v fmt (procname, vertex)
  end

  module UEdge = struct
    include Edge

    let pp fmt {kind; _} =
      (* Print the edge mimicking the json terminology *)
      match kind with
      | Direct ->
          Fmt.string fmt "Copy"
      | Call ->
          Fmt.string fmt "Call"
      | Return ->
          Fmt.string fmt "Return"
      | Capture ->
          Fmt.string fmt "Capture"
      | Builtin ->
          Fmt.string fmt "Builtin"
      | Summary {callee; _} ->
          Fmt.pf fmt "Derive.%a" Procname.pp_verbose callee
      | DynamicCallFunction ->
          Fmt.string fmt "DynamicCallFunction"
      | DynamicCallModule ->
          Fmt.string fmt "DynamicCallModule"
  end

  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (UVertex) (UEdge)

  let transform_v fetch_shapes procname (vertex : Vertex.t) : UVertex.t list =
    match vertex with
    | Local (local, node) ->
        [{procname; vertex= Local (local, node)}]
    | Argument (index, path) ->
        [{procname; vertex= Argument (index, path)}]
    | ArgumentOf (callee, index, path) ->
        let shapes = fetch_shapes callee in
        Shapes.map_argument shapes index path ~f:(fun path' ->
            {UVertex.procname= callee; vertex= Argument (index, path')} )
    | Captured index ->
        [{procname; vertex= Captured index}]
    | CapturedBy (closure, index) ->
        [{procname= closure; vertex= Captured index}]
    | Return path ->
        [{procname; vertex= Return path}]
    | ReturnOf (callee, path) ->
        let shapes = fetch_shapes callee in
        Shapes.map_return shapes path ~f:(fun path' ->
            {UVertex.procname= callee; vertex= Return path'} )
    | Self ->
        [{procname; vertex= Function}]
    | Function fun_ ->
        [{procname= fun_; vertex= Function}]


  let transform_e fetch_shapes procname (src, edge, dst) : G.E.t list =
    let srcs = transform_v fetch_shapes procname src in
    let dsts = transform_v fetch_shapes procname dst in
    List.cartesian_product srcs dsts |> List.map ~f:(fun (src', dst') -> (src', edge, dst'))


  module Dot = struct
    include Graph.Graphviz.Dot (struct
      include G

      module Color = struct
        let blue_gray_900 = 0x263238

        let blue_gray_600 = 0x546E7A

        let edge = blue_gray_600

        let vertex = blue_gray_900
      end

      let vertex_name v = string_of_int @@ [%hash: UVertex.t] v

      let graph_attributes _ = []

      let default_vertex_attributes _ =
        [`Shape `Box; `Style `Rounded; `Color Color.vertex; `Color Color.vertex]


      let vertex_attributes v = [`Label (Fmt.to_to_string UVertex.pp v)]

      let get_subgraph _ = None

      let default_edge_attributes _ = [`Color Color.edge; `Fontcolor Color.edge]

      let edge_attributes (_, edge, _) = [`Label (Fmt.to_to_string UEdge.pp edge)]
    end)

    let pp = fprint_graph
  end
end

(** Helper function. *)
let get_formals proc_desc : Var.t list =
  let f (pvar, _typ) = Var.of_pvar pvar in
  List.map ~f (Procdesc.get_pvar_formals proc_desc)


(** Helper function. *)
let get_captured proc_desc : Var.t list =
  let f {CapturedVar.pvar} = Var.of_pvar pvar in
  List.map ~f (Procdesc.get_captured proc_desc)


module Tito : sig
  (** TITO stands for "taint-in taint-out". In this context a tito flow is a data path from an
      argument field to a field of the return node, without going through call edges; more
      precisely, [i#foo#bar] is a tito path to [ret#baz] if there is a path from
      [Argument (i, [foo, bar])] to [Return baz] not going through [ArgumentOf _] nodes.

      As the abstraction mandates, fields are to be considered in a prefix sense: a path from
      [i#foo] to [ret#bar] means that any subfield of [i#foo] flows into all fields of [ret#bar].

      A path from [i#foo] to [ret#bar] can be marked as "shape-preserving", meaning that the
      argument field and the return field have the same shape AND that the data flows induced by the
      function call will go from each field of the argument into the same field of the return. This
      allows for more precise analysis of functions such as `id(X) -> X` when called on structured
      concrete arguments.

      Note: shape preservation indicates direct copying, thus a return field cannot be
      "shape-preserved" from more than one parameter field. The implementation will try to detect
      this and raise, but that should be considered a programming error. *)

  (** Sets of tito flows *)
  type t

  val pp : t Fmt.t

  val empty : t
  (** Empty tito: no argument field can flow into any return field *)

  val full : arity:int -> t
  (** A full tito has paths from every argument field to every return field. Due to the prefix
      abstraction, it is equivalent to having a path from every [Argument (i, [])] to [Return []].*)

  val add :
       arg_index:int
    -> arg_field_path:FieldPath.t
    -> ret_field_path:FieldPath.t
    -> shape_is_preserved:bool
    -> t
    -> t

  val fold :
       t
    -> init:'accum
    -> f:
         (   arg_index:int
          -> arg_field_path:FieldPath.t
          -> ret_field_path:FieldPath.t
          -> shape_is_preserved:bool
          -> 'accum
          -> 'accum )
    -> 'accum
end = struct
  module ArgPathSet = struct
    module FieldPathSet = struct
      (* Sets of field paths, that shall be associated to an argument index *)

      module M = Set.Make_tree (FieldPath)
      include M

      let pp ~arg_index =
        (* Prints: $argN#foo#bar $argN#other#field *)
        let pp_field_path = Fmt.using (fun path -> arg_index & path) F.arg_path in
        IFmt.Labelled.iter ~sep:Fmt.sp M.iter pp_field_path
    end

    (* Marshallable maps from integer indices. Note: using an array instead of Map could improve the
       performance (arguments indices are small and contiguous). *)
    module IntMap = Map.Make_tree (Int)

    (* An ArgPathSet is a set of field paths for each argument index *)
    type t = FieldPathSet.t IntMap.t

    let pp : t Fmt.t =
      (* Prints: $arg1#arg#field $arg1#other $arg2#foo#bar $arg3 *)
      let pp_binding fmt (arg_index, path_set) = FieldPathSet.pp ~arg_index fmt path_set in
      IFmt.Labelled.iter_bindings ~sep:Fmt.sp IntMap.iteri pp_binding


    let singleton ~arg_index ~arg_field_path =
      IntMap.singleton arg_index (FieldPathSet.singleton arg_field_path)


    let full ~arity =
      IntMap.of_sequence_exn
      @@ Sequence.init arity ~f:(fun arg_index -> (arg_index, FieldPathSet.singleton []))


    let add t ~arg_index ~arg_field_path =
      IntMap.update t arg_index ~f:(function
        | None ->
            FieldPathSet.singleton arg_field_path
        | Some field_path_set ->
            FieldPathSet.add field_path_set arg_field_path )


    let fold t ~f ~init =
      IntMap.fold
        ~f:(fun ~key:arg_index ~data:field_path_set acc ->
          FieldPathSet.fold
            ~f:(fun acc arg_field_path -> f ~arg_index ~arg_field_path acc)
            ~init:acc field_path_set )
        ~init t
  end

  module Sources = struct
    (** Data flow sources to be associated with return fields. *)

    (** A source is either a single shape-preserved argument index, or a "shape mixed" set of those.

        Remark: a shape-mixed set can be a singleton (eg. for unknown/recursive one-parameter
        functions -- such as [mix(X) -> mix(X).]). Empty "shape-mixed" sets pose no theoretical
        issue -- but we just don't generate them. *)
    type t =
      | Shape_preserved of {arg_index: int; arg_field_path: FieldPath.t}
      | Shape_mixed of ArgPathSet.t

    let pp fmt source =
      match source with
      | Shape_preserved {arg_index; arg_field_path} ->
          Fmt.pf fmt "=%a" F.arg_path (arg_index & arg_field_path)
      | Shape_mixed arg_path_set ->
          Fmt.pf fmt "{%a}" ArgPathSet.pp arg_path_set


    let singleton ~arg_index ~arg_field_path ~shape_is_preserved =
      if shape_is_preserved then Shape_preserved {arg_index; arg_field_path}
      else Shape_mixed (ArgPathSet.singleton ~arg_index ~arg_field_path)


    let full ~arity = Shape_mixed (ArgPathSet.full ~arity)

    let add source ~arg_index ~arg_field_path ~shape_is_preserved =
      match source with
      | Shape_mixed arg_path_set when not shape_is_preserved ->
          Shape_mixed (ArgPathSet.add arg_path_set ~arg_index ~arg_field_path)
      | Shape_mixed _ | Shape_preserved _ ->
          L.die InternalError
            "Lineage invariant broken: formal return shape cannot be preserved from several \
             sources."


    let fold source ~init ~f =
      match source with
      | Shape_preserved {arg_index; arg_field_path} ->
          f ~arg_index ~arg_field_path ~shape_is_preserved:true init
      | Shape_mixed arg_path_set ->
          ArgPathSet.fold
            ~f:(fun ~arg_index ~arg_field_path acc ->
              f ~arg_index ~arg_field_path ~shape_is_preserved:false acc )
            ~init arg_path_set
  end

  (* Marshallable maps from formal return field paths *)
  module RetPathMap = Map.Make_tree (FieldPath)

  (** A [Tito.t] is a collection, for every return field path, of its TITO sources. Note: for any
      [fields] path, if [i#fields] is a Tito source for [ret#fields'], then every [i#fields#foo]
      subfield of it should be considered as also being one (even if not explicitly present in the
      map value). *)
  type t = Sources.t RetPathMap.t

  let pp =
    (* Prints: ($ret#field: $arg0#foo) ($ret#other:$arg2 $arg3#bar) *)
    IFmt.Labelled.iter_bindings ~sep:Fmt.comma RetPathMap.iteri
      Fmt.(parens @@ pair ~sep:IFmt.colon_sp F.ret_path Sources.pp)


  let empty = RetPathMap.empty

  let add ~arg_index ~arg_field_path ~ret_field_path ~shape_is_preserved tito =
    RetPathMap.update tito ret_field_path ~f:(function
      | None ->
          (* No TITO arg for this ret field yet. We create one. *)
          Sources.singleton ~arg_index ~arg_field_path ~shape_is_preserved
      | Some source ->
          Sources.add source ~arg_index ~arg_field_path ~shape_is_preserved )


  let fold tito ~init ~f =
    RetPathMap.fold tito ~init ~f:(fun ~key:ret_field_path ~data:arg_path_set acc ->
        Sources.fold
          ~f:(fun ~arg_index ~arg_field_path ~shape_is_preserved acc ->
            f ~arg_index ~arg_field_path ~ret_field_path ~shape_is_preserved acc )
          ~init:acc arg_path_set )


  let full ~arity = RetPathMap.singleton [] (Sources.full ~arity)
end

module Summary = struct
  type tito_arguments = Tito.t

  type t = {graph: G.t; tito_arguments: tito_arguments; has_unsupported_features: bool}
  [@@deriving fields]

  let pp_tito_arguments fmt arguments =
    Format.fprintf fmt "@[@[<2>TitoArguments:@ {@;@[%a@]@]@,}@]" Tito.pp arguments


  let pp fmt {graph; tito_arguments} =
    Format.fprintf fmt "@;@[<v2>LineageSummary.@;%a@;%a@]" pp_tito_arguments tito_arguments G.pp
      graph


  let extract_tito shapes proc_desc (graph : G.t) =
    (* - Construct an adjacency list representation of the reversed graph.
       - Collect a set of nodes that break shape preservation, due to either being abstract cells, or
         being targets of edges that have this effect. *)
    let shape_mixing_nodes =
      let add_dst_if_shape_mixing ((_, _, dst) as edge) set =
        if G.preserves_shape edge then set else Set.add set dst
      in
      G.fold_edges_e add_dst_if_shape_mixing graph Vertex.Set.empty
    in
    (* Do a DFS, to see which arguments are reachable from a return field path. *)
    let rec dfs (shape_is_preserved, seen) node =
      if not (G.mem_vertex graph node) then (
        L.debug Analysis Verbose "Mysteriously missing node from lineage." ;
        (shape_is_preserved, seen) )
      else if Set.mem seen node then (shape_is_preserved, seen)
      else
        let seen = Set.add seen node in
        let incoming = G.pred_e graph node in
        let parents =
          List.filter_map
            ~f:(fun (src, {Edge.kind; _}, _) ->
              match kind with Call | Return -> None | _ -> Some src )
            incoming
        in
        let shape_is_preserved =
          (* That return field path is shape-preserved if it is only reachable by a direct path
             through shape-preserving one-source nodes. *)
          shape_is_preserved
          && (not (Set.mem shape_mixing_nodes node))
          &&
          match parents with
          | [] | [_] ->
              true
          | _ ->
              (* TODO T154077173: `ite(true, X, Y) -> X`: X and Y are shape-preserved, but support of
                 ite(_, {a, b}, 42) may be not trivial due to not-same-shaped parallel writes. *)
              false
        in
        List.fold ~init:(shape_is_preserved, seen) ~f:dfs parents
    in
    let collect_reachable return_field_path =
      dfs (true, Vertex.Set.empty) (Vertex.Return return_field_path)
    in
    (* Collect reachable arguments from a Return field path. *)
    let collect_tito tito ret_field_path =
      let shape_is_preserved, reachable = collect_reachable ret_field_path in
      Set.fold reachable ~init:tito ~f:(fun acc (node : G.vertex) ->
          match node with
          | Argument (arg_index, arg_field_path) ->
              Tito.add ~arg_index ~arg_field_path ~ret_field_path ~shape_is_preserved acc
          | _ ->
              acc )
    in
    let ret_var_path = VarPath.pvar (Procdesc.get_ret_var proc_desc) in
    let return_field_paths =
      Shapes.fold_cells shapes ret_var_path ~init:[] ~f:(fun acc cell ->
          Cell.field_path cell :: acc )
    in
    List.fold return_field_paths ~init:Tito.empty ~f:collect_tito


  (** Reduces the size of the graph by possibly some vertices and some edges. Consider the following
      pattern: A -1-> B -2-> C. (Here, A, B and C are vertices; 1 and 2 are edges.) The basic trick
      is to transform such patterns into A -1-> C when: (i) B is "uninteresting" and (ii) 2 is
      "uninteresting". Vertex is uninteresting when it corresponds to a temporary variable
      introduced by the frontend; Edges are uninteresting when they are of kind [Direct].

      The basic trick is slightly generalized below: B may have 1 edge on one side, but 0..oo on the
      other side. This allows considerably more reduction in size.

      Note0: Because 0 is allowed in the "general" trick, (a) it is possible that interesting vertex
      is removed from the graph (but this happens only for vertex that is not connected in lineage
      to other interesting vertex); and (b) it is possible that the number of vertices is reduced
      more than the number of edges (otherwise, each transform step would reduce both the number of
      vertices and the number of edges by exactly 1, so the reduction would be equal to the number
      of transform steps applied).

      Note1: The implementation assumes no loops; otherwise, it may not terminate. (This is easy to
      change, but there should be no loops in lineage anyway.)

      Note2: The algorithm guarantees that the number of edges does not increase, and that the
      number of vertices does not increase. *)
  let remove_temporaries proc_desc graph =
    (* A check for vertex interestingness, used in the graph simplification that follows. *)
    let special_variables =
      let formals = get_formals proc_desc in
      let ret_var = Var.of_pvar (Procdesc.get_ret_var proc_desc) in
      Var.Set.of_list (ret_var :: formals)
    in
    let is_interesting_vertex (vertex : G.vertex) =
      match vertex with
      | Local (Cell cell, _node) ->
          Cell.var_appears_in_source_code cell
          && not (Var.Set.mem (Cell.var cell) special_variables)
      | Argument _ | Captured _ | Return _ | CapturedBy _ | ArgumentOf _ | ReturnOf _ ->
          true
      | Local (ConstantAtom _, _) | Local (ConstantInt _, _) | Local (ConstantString _, _) ->
          true
      | Function _ | Self ->
          true
    in
    let is_interesting_edge ((src, {kind; _}, dst) : G.edge) =
      match kind with
      | Direct -> (
        match (src, dst) with
        | Argument (_, _ :: _), _ | _, Return (_ :: _) ->
            (* The edge will have non-empty injection/projection metadata. *)
            true
        | _ ->
            false )
      | Builtin ->
          (* Suppressed builtins are considered as direct edges for being simplification
             candidates. *)
          false
      | Call | Return | Capture | Summary _ | DynamicCallFunction | DynamicCallModule ->
          true
    in
    let merge_edges (src_ab, {Edge.kind= kind_ab; node= node_ab}, _)
        (_, {Edge.kind= kind_bc; node= node_bc}, dst_bc) : G.edge =
      (* Merge both edges together, keeping source of the first one, the target of the second one,
         and the kind of node of the most interesting one. The interesting order is Direct < Builtin
         < anything. *)
      let kind, node =
        (* Both nodes should map to the same location, so we could just use the first one, but we
           keep the one corresponding to the kind for good measure and least surprise. *)
        match (kind_ab, kind_bc) with
        | Direct, _ ->
            (kind_bc, node_bc)
        | _, Direct ->
            (kind_ab, node_ab)
        | Builtin, _ (* not Direct *) ->
            (kind_bc, node_bc)
        | _ (* not Direct *), Builtin ->
            (kind_ab, node_ab)
        | _ ->
            L.die InternalError "I can't merge two interesting edges together"
      in
      (src_ab, {Edge.kind; node}, dst_bc)
    in
    (* The set [todo] contains vertices considered for removal. We initialize this set with all
     * uninteresting vertices. The main loop of the algorithm extracts a vertex from [todo] (in
     * some deterministic but unspecified order) and checks if it meets a condition for removal.
     * If so, it removes it from the graph, and puts its former uninteresting neighbours back in
     * [todo], so that their condition for removal will be re-examined. (A note on termination:
     * Let m be the number of edges between uninteresting nodes; let n be the number of
     * uninteresting nodes. There are at most m+n insertions into the set [todo]: n when [todo]
     * is initialized, and at most m while in the main loop. And each iteration of the main loop
     * does one deletion from the set [todo].) *)
    let todo =
      (* Initialise todo with the set of non interesting vertices *)
      G.fold_vertex
        (fun vertex set -> if is_interesting_vertex vertex then set else Set.add set vertex)
        graph Vertex.Set.empty
    in
    let rec simplify todo graph =
      match Set.choose todo with
      | None ->
          graph
      | Some vertex ->
          if is_interesting_vertex vertex then L.die InternalError "Shouldn't happen" ;
          let todo = Set.remove todo vertex in
          let before = G.pred_e graph vertex in
          let after = G.succ_e graph vertex in
          let before_interesting = List.exists ~f:is_interesting_edge before in
          let after_interesting = List.exists ~f:is_interesting_edge after in
          let short list = match list with [] | [_] -> true | _ -> false in
          let before_short = short before in
          let after_short = short after in
          if (before_short || after_short) && not (before_interesting && after_interesting) then
            let pairs = List.cartesian_product before after in
            (* (A) remove old edges *)
            let graph = G.remove_vertex graph vertex in
            let do_pair (todo, graph) ((edge_ab : G.edge), (edge_bc : G.edge)) =
              let ((keep_src, _, keep_dst) as keep_edge) = merge_edges edge_ab edge_bc in
              if Vertex.equal keep_src keep_dst then
                L.die InternalError "OOPS: I don't work with loops." ;
              (* (B) add new edges *)
              let graph = G.add_edge_e graph keep_edge in
              (* The following two lines insert at most 1 vertex into [todo] for each [edge_ab]/
               * [edge_bc] edge that gets removed from the graph, in step (A) above. This is where
               * the earlier claim that at most m insertions happen in the main loop. However,
               * this claim is invalidated if one of the [edge_ab]/[edge_bc] edges gets re-added
               * to the graph in step (B) above, which may happen if there is an (isolated) loop
               * in the graph. *)
              let todo = if is_interesting_vertex keep_src then todo else Set.add todo keep_src in
              let todo = if is_interesting_vertex keep_dst then todo else Set.add todo keep_dst in
              (todo, graph)
            in
            let todo, graph = List.fold ~init:(todo, graph) ~f:do_pair pairs in
            simplify todo graph
          else simplify todo graph
    in
    simplify todo graph


  (** Given a graph, computes tito_arguments, and makes a summary. *)
  let make shapes proc_desc has_unsupported_features graph =
    (* We need to compute the Tito arguments before reducing the graph to make use of information
       hold by temporary nodes (eg. if they're abstract or not). *)
    let tito_arguments = extract_tito shapes proc_desc graph in
    let graph =
      if Config.lineage_keep_temporaries then graph else remove_temporaries proc_desc graph
    in
    {graph; tito_arguments; has_unsupported_features}
end

module Out = struct
  module Json = struct
    type location_id = int64 [@@deriving yojson_of]

    type node_id = int64 [@@deriving yojson_of]

    type state_id = int64 [@@deriving yojson_of]

    (* These correspond to CFG nodes in Infer. *)
    type _location =
      { id: location_id
      ; function_: string [@key "function"]
      ; file: string
      ; line: int option (* might be unknown for some locations *) }
    [@@deriving yojson_of, yojson_fields]

    type location = {location: _location} [@@deriving yojson_of]

    (* These correspond to abstract states in AbsInt. *)
    type _state = {id: state_id; location: location_id} [@@deriving yojson_of]

    type state = {state: _state} [@@deriving yojson_of]

    module TermType = struct
      type t =
        | UserVariable
        | TemporaryVariable
        | ConstantAtom
        | ConstantInt
        | ConstantString
        | Argument
        | Return
        | Function
      [@@deriving variants]
    end

    type term_type = TermType.t

    let rank_of_term_type = TermType.Variants.to_rank

    let yojson_of_term_type (typ : term_type) =
      match typ with
      | UserVariable ->
          `String "UserVariable"
      | TemporaryVariable ->
          `String "Temporary" (* T106560112 *)
      | ConstantAtom ->
          `String "ConstantAtom"
      | ConstantInt ->
          `String "ConstantInt"
      | ConstantString ->
          `String "ConstantString"
      | Argument ->
          `String "Argument"
      | Return ->
          `String "Return"
      | Function ->
          `String "Function"


    type term =
      { term_name: string [@key "name" (* T106560112 *)]
      ; term_type: term_type [@key "variable_type" (* T106560112 *)] }
    [@@deriving yojson_of]

    type _node = {id: node_id; state: state_id; term: term [@key "variable" (* T106560112 *)]}
    [@@deriving yojson_of]

    type node = {node: _node} [@@deriving yojson_of]

    type edge_type =
      | Call
      | Capture
      | Copy
      | Derive
      | DynamicCallFunction
      | DynamicCallModule
      | Return

    module EdgeMetadata = struct
      type procname_with_simple_json = Procname.t

      let yojson_of_procname_with_simple_json procname =
        `String (Procname.to_string_verbose procname)


      type t =
        { inject: FieldPath.t [@default []] [@yojson_drop_default.equal]
        ; project: FieldPath.t [@default []] [@yojson_drop_default.equal]
        ; derives: procname_with_simple_json option [@yojson.option] }
      [@@deriving yojson_of, fields]

      let empty = {inject= []; project= []; derives= None}

      let is_empty = function {inject= []; project= []; derives= None} -> true | _ -> false

      let with_inject ~(dst : Vertex.t) t =
        let inject = match dst with ArgumentOf (_, _, path) | Return path -> path | _ -> [] in
        {t with inject}


      let with_project ~(src : Vertex.t) t =
        let project = match src with ReturnOf (_, path) | Argument (_, path) -> path | _ -> [] in
        {t with project}


      let with_derives ~(kind : Edge.kind) t =
        match kind with Summary {callee; _} -> {t with derives= Some callee} | _ -> t


      let map_fields ~inject ~project ~derives t =
        [inject t.inject; project t.project; derives t.derives]
    end

    let yojson_of_edge_type typ =
      match typ with
      | Capture ->
          `String "Capture"
      | Call ->
          `String "Call"
      | Copy ->
          `String "Copy"
      | Derive ->
          `String "Derive"
      | DynamicCallFunction ->
          `String "DynamicCallFunction"
      | DynamicCallModule ->
          `String "DynamicCallModule"
      | Return ->
          `String "Return"


    type _edge =
      { source: node_id
      ; target: node_id
      ; edge_type: edge_type
      ; edge_metadata: EdgeMetadata.t [@yojson_drop_if EdgeMetadata.is_empty]
      ; location: location_id }
    [@@deriving yojson_of]

    type edge = {edge: _edge} [@@deriving yojson_of]

    type _function = {name: string; has_unsupported_features: bool} [@@deriving yojson_of]

    type function_ = {function_: _function [@key "function"]} [@@deriving yojson_of]

    type entity_type = Edge | Function | Location | Node | State
    [@@deriving compare, equal, hash, sexp]
  end

  let channel_ref = ref None

  let get_pid_channel () =
    (* We keep the old simple-lineage output dir for historical reasons and should change it to
       lineage once no external infra code depends on it anymore *)
    let output_dir = Filename.concat Config.results_dir "simple-lineage" in
    Unix.mkdir_p output_dir ;
    match !channel_ref with
    | None ->
        let filename = Format.asprintf "lineage-%a.json" Pid.pp (Unix.getpid ()) in
        let channel = Filename.concat output_dir filename |> Out_channel.create in
        let close_channel () =
          Option.iter !channel_ref ~f:Out_channel.close_no_err ;
          channel_ref := None
        in
        Epilogues.register ~f:close_channel ~description:"close output channel for lineage" ;
        channel_ref := Some channel ;
        channel
    | Some channel ->
        channel


  type state_local = Start of Location.t | Exit of Location.t | Normal of PPNode.t

  module Id = struct
    (** Internal representation of an Id. *)
    type t = Z.t

    (** Largest prime that fits in 63 bits. *)
    let modulo_i64 = Int64.of_string "9223372036854775783"

    let modulo_z = Z.of_int64 modulo_i64

    let zero : t = Z.zero

    let one : t = Z.one

    let two : t = Z.of_int 2

    let coefficients =
      let open Sequence.Generator in
      let rec gen prng =
        yield (Z.of_int64 (Random.State.int64 prng modulo_i64)) >>= fun () -> gen prng
      in
      Sequence.memoize (run (gen (Random.State.make [|Config.lineage_seed|])))


    let of_sequence ids =
      let hash_add old_hash (a, b) =
        let open Z in
        (old_hash + (a * b)) mod modulo_z
      in
      Sequence.fold ~init:zero ~f:hash_add (Sequence.zip ids coefficients)


    let of_list ids = of_sequence (Sequence.of_list ids)

    let of_state_local (state : state_local) : t =
      match state with
      | Start _ ->
          of_list [zero]
      | Exit _ ->
          of_list [one]
      | Normal n ->
          of_list [two; Z.of_int (PPNode.hash n)]


    (** Workaround: [String.hash] leads to many collisions. *)
    let of_string s =
      of_sequence
        (Sequence.map
           ~f:(fun c -> Z.of_int (int_of_char c))
           (Sequence.of_seq (Caml.String.to_seq s)) )


    let of_procname procname : t = of_string (Procname.hashable_name procname)

    let of_term_type (term_type : Json.term_type) = Z.of_int (Json.rank_of_term_type term_type)

    let of_term {Json.term_name; term_type} : t =
      of_list [of_string term_name; of_term_type term_type]


    let of_kind (kind : Edge.kind) : t = Z.of_int (Edge.Kind.to_rank kind)

    let of_field_path field_path = of_string (Fmt.to_to_string FieldPath.pp field_path)

    let of_option of_elt option =
      match option with None -> of_list [] | Some elt -> of_list [of_elt elt]


    let of_edge_metadata edge_metadata =
      of_list
      @@ Json.EdgeMetadata.map_fields edge_metadata ~inject:of_field_path ~project:of_field_path
           ~derives:(of_option of_procname)


    (** Converts the internal representation to an [int64], as used by the [Out] module. *)
    let out id : int64 =
      try Z.to_int64 id with Z.Overflow -> L.die InternalError "Hash does not fit in int64"
  end

  (** Like [G.vertex], but :

      - Without the ability to refer to other procedures, which makes it "local".
      - With some information lost/summarised, such as fields of procedure arguments/return
        (although the Derive edges will be generated taking fields into account, we only output one
        node for each argument in the Json graph to denote function calls). *)
  type local_vertex = Argument of int | Captured of int | Return | Local of Local.t | Function

  let pp_local_vertex fmt vertex =
    match vertex with
    | Argument index ->
        F.arg fmt index
    | Captured index ->
        F.captured fmt index
    | Return ->
        F.ret fmt ()
    | Local local ->
        Local.pp fmt local
    | Function ->
        F.fun_ fmt ()


  let term_of_vertex (vertex : local_vertex) : Json.term =
    let term_name = Fmt.to_to_string pp_local_vertex vertex in
    let term_type : Json.term_type =
      match vertex with
      | Argument _ | Captured _ ->
          Argument
      | Return ->
          Return
      | Local (Cell cell) ->
          if Cell.var_appears_in_source_code cell then UserVariable else TemporaryVariable
      | Local (ConstantAtom _) ->
          ConstantAtom
      | Local (ConstantInt _) ->
          ConstantInt
      | Local (ConstantString _) ->
          ConstantString
      | Function ->
          Function
    in
    {Json.term_name; term_type}


  module JsonCacheKey = struct
    module T = struct
      type t = Json.entity_type * Int64.t [@@deriving compare, equal, hash, sexp]
    end

    include T
    include Hashable.Make (T)
  end

  let with_dedup_cache f =
    let cache = JsonCacheKey.Hash_set.create () in
    f cache


  let write_json (json_dedup_cache : JsonCacheKey.Hash_set.t) outchan =
    let really_write_json json =
      Yojson.Safe.to_channel outchan json ;
      Out_channel.newline outchan
    in
    if Config.lineage_dedup then ( fun category id json ->
      let key = (category, Id.out id) in
      if not (Hash_set.mem json_dedup_cache key) then (
        Hash_set.add json_dedup_cache key ;
        really_write_json json ) )
    else fun _category _id json -> really_write_json json


  let write_location json_dedup_cache outchan ~write procname (state_local : state_local) : Id.t =
    let procname_id = Id.of_procname procname in
    let state_local_id = Id.of_state_local state_local in
    let location_id = Id.of_list [procname_id; state_local_id] in
    ( if write then
        let location =
          match state_local with
          | Start location | Exit location ->
              location
          | Normal node ->
              PPNode.loc node
        in
        let function_ = Procname.hashable_name procname in
        let file =
          if Location.equal Location.dummy location then "unknown"
          else SourceFile.to_rel_path location.Location.file
        in
        let line = if location.Location.line < 0 then None else Some location.Location.line in
        write_json json_dedup_cache outchan Location location_id
          (Json.yojson_of_location {location= {id= Id.out location_id; function_; file; line}}) ) ;
    location_id


  let write_state json_dedup_cache outchan ~write procname (state : state_local) : Id.t =
    let location_id = write_location json_dedup_cache outchan ~write procname state in
    if write then
      write_json json_dedup_cache outchan State location_id
        (Json.yojson_of_state {state= {id= Id.out location_id; location= Id.out location_id}}) ;
    location_id


  let write_vertex json_dedup_cache outchan proc_desc (vertex : Vertex.t) =
    let save ?(write = true) procname state_local local_vertex =
      let state_id = write_state json_dedup_cache outchan ~write procname state_local in
      let term = term_of_vertex local_vertex in
      let node_id = Id.of_list [state_id; Id.of_term term] in
      if write then
        write_json json_dedup_cache outchan Node node_id
          (Json.yojson_of_node {node= {id= Id.out node_id; state= Id.out state_id; term}}) ;
      node_id
    in
    let procname = Procdesc.get_proc_name proc_desc in
    let start = Start (Procdesc.Node.get_loc (Procdesc.get_start_node proc_desc)) in
    let exit = Exit (Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc)) in
    match vertex with
    | Local (var, node) ->
        save procname (Normal node) (Local var)
    | Argument (index, _field_path) ->
        save procname start (Argument index)
    | ArgumentOf (callee_procname, index, _field_path) ->
        save callee_procname (Start Location.dummy) (Argument index)
    | Captured index ->
        save procname start (Captured index)
    | CapturedBy (lambda_procname, index) ->
        save ~write:false lambda_procname (Start Location.dummy) (Captured index)
    | Return _ ->
        save procname exit Return
    | ReturnOf (callee_procname, _field_path) ->
        save callee_procname (Exit Location.dummy) Return
    | Self ->
        save procname start Function
    | Function procname ->
        save ~write:false procname (Start Location.dummy) Function


  let write_edge json_dedup_cache outchan proc_desc ((src, {kind; node}, dst) : G.edge) =
    let src_id = write_vertex json_dedup_cache outchan proc_desc src in
    let dst_id = write_vertex json_dedup_cache outchan proc_desc dst in
    let kind_id = Id.of_kind kind in
    let location_id =
      let procname = Procdesc.get_proc_name proc_desc in
      write_location json_dedup_cache outchan ~write:true procname (Normal node)
    in
    let edge_type =
      match kind with
      | Call ->
          Json.Call
      | Capture ->
          Json.Capture
      | Direct ->
          Json.Copy
      | Builtin ->
          Json.Copy
      | Summary _ ->
          Json.Derive
      | DynamicCallFunction ->
          Json.DynamicCallFunction
      | DynamicCallModule ->
          Json.DynamicCallModule
      | Return ->
          Json.Return
    in
    (* As the [Return ret_path] nodes will all be merged in a single [Return], we add the [Injection
        ret_path] metadata to their incoming Copy edges to encode the information that they're
        flowing into a specific field path of the returned value.

       We similarly generate projection metadata from [Argument (index, path)] nodes, that will be
       merged into a summarising [Argument index] one; and from [ReturnOf] and [ArgumentOf]
       interprocedural nodes. *)
    let edge_metadata =
      Json.EdgeMetadata.(empty |> with_inject ~dst |> with_project ~src |> with_derives ~kind)
    in
    let metadata_id =
      (* Contrary to other ids which are computed from the source components, this one must be
         computed on the generated metadata since it doesn't exist as-is in the source. *)
      Id.of_edge_metadata edge_metadata
    in
    let edge_id = Id.of_list [src_id; dst_id; kind_id; metadata_id; location_id] in
    write_json json_dedup_cache outchan Edge edge_id
      (Json.yojson_of_edge
         { edge=
             { source= Id.out src_id
             ; target= Id.out dst_id
             ; edge_type
             ; edge_metadata
             ; location= Id.out location_id } } ) ;
    edge_id


  let write_graph json_dedup_cache outchan proc_desc graph =
    let record_edge edge = ignore (write_edge json_dedup_cache outchan proc_desc edge) in
    G.iter_edges_e record_edge graph


  let write_summary json_dedup_cache outchan proc_desc {Summary.graph; has_unsupported_features} =
    let procname = Procdesc.get_proc_name proc_desc in
    let fun_id = Id.of_procname procname in
    write_json json_dedup_cache outchan Function fun_id
      (Json.yojson_of_function_
         {function_= {name= Procname.hashable_name procname; has_unsupported_features}} ) ;
    let _fun_id = write_vertex json_dedup_cache outchan proc_desc Self in
    write_graph json_dedup_cache outchan proc_desc graph ;
    Out_channel.flush outchan


  let report_summary proc_desc summary =
    let outchan = get_pid_channel () in
    with_dedup_cache write_summary outchan proc_desc summary
end

(** A summary is computed by taking the union of all partial graphs present in the final invariant
    map. Partial graphs are stored in abstract states only because it is convenient. But, they do
    not influence how abstract states are joined, widened, etc, which explains why below all
    functions having to do with the abstract domain are dummies. *)
module PartialGraph : sig
  type t

  include AbstractDomain.WithBottom with type t := t

  val pp : t Fmt.t

  val add_edge : node:PPNode.t -> kind:Edge.kind -> src:Vertex.t -> dst:Vertex.t -> t -> t

  val aggregate : t list -> G.t
end = struct
  type t = G.edge list

  let pp = Fmt.box @@ Fmt.list Edge.pp_e

  let bottom = []

  let is_bottom = List.is_empty

  let leq ~lhs:_ ~rhs:_ = true

  let join _ _ = bottom

  let widen ~prev:_ ~next ~num_iters:_ = next

  let add_edge ~node ~kind ~src ~dst partial_graph =
    let added = (src, {Edge.kind; node}, dst) :: partial_graph in
    match ((kind : Edge.kind), Vertex.equal dst src, (dst : Vertex.t), (src : Vertex.t)) with
    | Direct, true, _, _ ->
        partial_graph (* skip Direct loops *)
    | Builtin, true, _, _ ->
        partial_graph (* skip Builtin loops *)
    | Summary _, true, _, _ ->
        partial_graph (* skip Summary loops*)
    | _, true, _, _ ->
        L.die InternalError "There shall be no fancy (%a) loops!" Edge.Kind.pp kind
    | Call, _, ArgumentOf _, _ ->
        added
    | Call, _, _, _ ->
        L.die InternalError "Call edges shall return ArgumentOf!"
    | Return, _, _, ReturnOf _ ->
        added
    | Return, _, _, _ ->
        L.die InternalError "Return edges shall come form ReturnOf!"
    | _ ->
        added


  let aggregate partial_graph_list : G.t =
    List.fold
      ~f:(fun acc partial_graph -> List.fold ~f:G.add_edge_e ~init:acc partial_graph)
      ~init:G.empty partial_graph_list
end

module Domain : sig
  (** As seen from the outside, an abstract state has two handlable components:

      - A partial set of Lineage graph edges: the final graph for a procedure shall be constructed
        by accumulating the edges collected at each procedure node and adding special edges for the
        parameters and return;
      - A boolean [has_unsupported_features] that records if the current procedure makes use of
        Erlang features unsupported by Infer. *)

  include AbstractDomain.WithBottom

  module Src : sig
    (** Constructors for source nodes of edges in the flow graph.

        Each function corresponds to a collection of {!G.vertex}, along with a signal for each
        vertex that can be used to branch on specific destinations and/or edge kinds. *)

    type +'signal t

    val function_ : Procname.t -> unit t

    val argument : int -> FieldPath.t t

    val captured : int -> unit t

    val return_of : Ident.t -> Procname.t -> FieldPath.t t
    (** Expects the [Ident.t] that will store the returned value to derive the fields. Remark: this
        is an awkwardness that is currently required because the flow recording goes from sources to
        dests. It would look nicer if [Src] and [Dst] were unified in a single module (with an
        arrow-like structure). *)

    val atom : string -> unit t

    val local_set : Local.Set.t -> unit t

    val var_path : VarPath.t -> FieldPath.t t

    val var : Var.t -> FieldPath.t t

    val pvar : Pvar.t -> FieldPath.t t

    val silence : _ t -> unit t
  end

  module Dst : sig
    (** Constructors for destination nodes of edges in the flow graph.

        Each function corresponds to a collection of {!G.vertex}. It requires a slot (that should be
        obtained as the signal from the source) to build this collection and will return its own
        signal for each of the built vertices. *)

    type (-'slot, +'signal) t

    val captured_by : Procname.t -> int -> (unit, unit) t

    val argument_of : Procname.t -> int -> (FieldPath.t, unit) t
    (** The slot is the sub-path of the argument that is being written, as known by the caller
        procedure. *)

    val return : (FieldPath.t, unit) t

    val var_path : VarPath.t -> (FieldPath.t, FieldPath.t) t
    (** The slot is a sub path to be subscripted from the VarPath argument, yielding a dst_path of
        which cells will be searched. Will signal the path extracted from dst_path for each cell
        (see {!Cell.path_from_origin}).

        For instance, if [X#foo#bar#baz] exists, [var_path(X, slot=foo)] will return #bar#baz as its
        signal. *)

    val var : Var.t -> (FieldPath.t, FieldPath.t) t

    val ident : Ident.t -> (FieldPath.t, FieldPath.t) t

    val all_fields : (FieldPath.t, 'signal) t -> (unit, 'signal) t
    (** Will write into the "root" a destination that expect field paths by providing the empty
        field path for the slot. *)
  end

  val partial_graph : t -> PartialGraph.t
  (** Extract the edges accumulated in an abstract state. *)

  val clear_partial_graph : t -> t
  (** Forget the collected edges. One can use this when going to analyse another instruction as the
      finalisation of the procedure analysis will go through all nodes to collect the complete edges
      set. *)

  val has_unsupported_features : t -> bool
  (** Check if the abstract state recorded unsupported Erlang features. *)

  val record_supported : Procname.t -> t -> t
  (** If the given procedure is an unsupported Erlang feature, record that in the abstract state. *)

  (** {2 Record data flow} *)

  val add_flow :
    shapes:shapes -> node:PPNode.t -> kind:Edge.kind -> src:'a Src.t -> dst:('a, _) Dst.t -> t -> t
  (** Record flow from nodes under the source into nodes with matching slot under the destination.
      Note: this function ignores the destination signals. *)

  val add_var_path_flow :
       shapes:shapes
    -> node:PPNode.t
    -> kind:Edge.kind
    -> src:VarPath.t
    -> dst:VarPath.t
    -> ?exclude:FieldLabel.t
    -> t
    -> t
  (** A variable path specific version of {!add_flow} that will also check for shape equality.

      If the [exclude] field label is set, flows whose source or destination starts with this field
      label won't be added. *)
end = struct
  module LastWrites = AbstractDomain.FiniteMultiMap (Cell) (PPNode)
  module HasUnsupportedFeatures = AbstractDomain.BooleanOr

  (** Actual domain for iterations *)
  module Real = AbstractDomain.PairWithBottom (LastWrites) (HasUnsupportedFeatures)

  (** Stored with abstract states for convenience, not used for iteration purposes *)
  module Unit = PartialGraph

  include AbstractDomain.PairWithBottom (Real) (Unit)

  type domain = t

  let partial_graph (_, partial_graph) = partial_graph

  let clear_partial_graph (real, _partial_graph) = (real, PartialGraph.bottom)

  let last_writes ((last_writes, _), _) = last_writes

  let has_unsupported_features ((_, has_unsupported_features), _) = has_unsupported_features

  let record_supported name (((last_writes, has_unsupported_features), partial_graph) : t) : t =
    let has_unsupported_features =
      has_unsupported_features || Procname.is_erlang_unsupported name
    in
    ((last_writes, has_unsupported_features), partial_graph)


  module Fold = struct
    (** Sequences of elements encoded by a fold function accumulating over domains.

        Implementation note: we use domains as accumulators as using a generic type would bring us
        into the bureaucracy hell of higher order polymorphism. The other generic alternative would
        be to use iterators [('a -> unit) -> unit], but since we're in fact only using this to build
        domains we can just instantiate folds to that specific type. *)

    (** The type of sequences of ['a], represented by their fold function over domains. Order of
        calls to the accumulating argument function is generally unspecified. *)
    type 'a t = (domain -> 'a -> domain) -> domain -> domain

    let singleton (elt : 'a) : 'a t = fun f init -> f init elt

    let filter ~f (x : 'a t) : 'a t =
     fun g init -> x (fun accum elt -> if f elt then g accum elt else accum) init
  end

  module Src = struct
    (** A source is a sequence of vertices and slots that can be built from shapes at a procedure
        current node. *)
    type 'slot t = shapes -> PPNode.t -> (Vertex.t * 'slot) Fold.t

    let single_vertex (vertex : Vertex.t) : unit t = fun _shapes _node -> Fold.singleton (vertex, ())

    let function_ procname : unit t = single_vertex (Function procname)

    let argument index : FieldPath.t t =
     fun shapes _node f astate ->
      Shapes.fold_argument
        ~f:(fun accum arg_path -> f accum (Argument (index, arg_path), arg_path))
        ~init:astate shapes index []


    let captured i : unit t = single_vertex (Captured i)

    let return_of ret_id proc_name : FieldPath.t t =
     fun shapes _node f astate ->
      Shapes.fold_cells
        ~f:(fun accum ret_cell ->
          let ret_path = Cell.field_path ret_cell in
          f accum (ReturnOf (proc_name, ret_path), ret_path) )
        ~init:astate shapes (VarPath.ident ret_id)


    let fold_cell_last_writes ~(slot : 'slot) cell : (G.vertex * 'slot) Fold.t =
     (* Fold over the last_writes sources of the cell and an explicit unique slot. *)
     fun f astate ->
      LastWrites.find_fold
        (fun lw_node acc -> f acc (Vertex.cell_local lw_node cell, slot))
        cell (last_writes astate) astate


    let local (local : Local.t) : unit t =
     fun _shapes node f astate ->
      match local with
      | ConstantAtom _ | ConstantInt _ | ConstantString _ ->
          f astate (Vertex.Local (local, node), ())
      | Cell cell ->
          fold_cell_last_writes ~slot:() cell f astate


    let local_set local_set : unit t =
     fun shapes node f astate ->
      Set.fold ~f:(fun acc one_local -> local one_local shapes node f acc) ~init:astate local_set


    let atom atom_name : unit t = local (ConstantAtom atom_name)

    let var_path var_path : FieldPath.t t =
     fun shapes _node f astate ->
      Shapes.fold_cells
        ~f:(fun acc cell ->
          fold_cell_last_writes ~slot:(Cell.path_from_origin ~origin:var_path cell) cell f acc )
        ~init:astate shapes var_path


    let var var : FieldPath.t t = var_path (VarPath.var var)

    let pvar pvar : FieldPath.t t = var_path (VarPath.pvar pvar)

    let exclude_field field_label (src : FieldPath.t t) : FieldPath.t t =
     fun shapes node f astate ->
      Fold.filter
        ~f:(fun (_vertex, signal) -> not (FieldPath.first_field_is field_label signal))
        (src shapes node) f astate


    let silence (src : _ t) : unit t =
     fun shapes node f astate ->
      src shapes node (fun acc (vertex, _slot) -> f acc (vertex, ())) astate
  end

  module Dst = struct
    (** A source is a sequence of vertices, built from a slot, that can use shapes and procedure
        current node information. *)
    type ('slot, 'signal) t = shapes -> PPNode.t -> 'slot -> (Vertex.t * 'signal) Fold.t

    let single_vertex (vertex : Vertex.t) : (unit, unit) t =
     fun _shapes _node () -> Fold.singleton (vertex, ())


    let captured_by i proc_name : (unit, unit) t = single_vertex (CapturedBy (i, proc_name))

    let argument_of proc_name index : (FieldPath.t, unit) t =
     fun _shapes _node arg_path ->
      Fold.singleton (Vertex.ArgumentOf (proc_name, index, arg_path), ())


    let return : (FieldPath.t, unit) t =
     (* The return special destination will be used in conjunction with the returned variable as the
        source, and since they both have the same fields, we should not need to use [fold_return]
        here. Indeed every [field_path] slot should exactly correspond to one [Return field_path]
        vertex. We still proceed with [fold_return] for good measure and future-proofing. *)
     fun shapes _node field_path f astate ->
      Shapes.fold_return shapes field_path
        ~f:(fun acc final_path -> f acc (Return final_path, ()))
        ~init:astate


    let var_path var_path : (FieldPath.t, FieldPath.t) t =
     fun shapes node sub_path f astate ->
      let dst_path = VarPath.sub_path var_path sub_path in
      Shapes.fold_cells
        ~f:(fun acc dst_cell ->
          let cell_sub_path = Cell.path_from_origin ~origin:dst_path dst_cell in
          f acc (Vertex.cell_local node dst_cell, cell_sub_path) )
        ~init:astate shapes dst_path


    let var var : (FieldPath.t, FieldPath.t) t = var_path (VarPath.var var)

    let ident ident : (FieldPath.t, FieldPath.t) t = var_path (VarPath.ident ident)

    let all_fields (dst : (FieldPath.t, 'signal) t) : (unit, 'signal) t =
     fun shapes node () -> dst shapes node []


    let exclude_field field_label (dst : ('a, FieldPath.t) t) : ('a, FieldPath.t) t =
     fun shapes node slot f astate ->
      Fold.filter
        ~f:(fun (_vertex, signal) -> not (FieldPath.first_field_is field_label signal))
        (dst shapes node slot) f astate
  end

  let add_flow_edge ~node ~kind ~(src : Vertex.t) ~(dst : Vertex.t)
      ((last_writes, has_unsupported_features), partial_graph) : t =
    let partial_graph = PartialGraph.add_edge ~node ~kind ~src ~dst partial_graph in
    let last_writes =
      match dst with
      | Local (Cell cell, _) ->
          LastWrites.set_to_single_value cell node last_writes
      | _ ->
          last_writes
    in
    ((last_writes, has_unsupported_features), partial_graph)


  (** Record flow from nodes under the source into nodes with matching slot under the destination.
      [kind_f] can use the destination signal to record edges with specific kinds. *)
  let add_flow_f ~shapes ~node ~kind_f ~(src : 'a Src.t) ~(dst : ('a, _) Dst.t) astate : t =
    (* Note: we keep this one for historical reasons, but if [add_flow] below keeps being the only
       use, we can inline it away.

       Since this is the reason for destination signals to exist, they could then also be removed.

       Non-exhaustive reasons why we may want to use this again:
       - Generate shape-preserving information depending on cell sources/destinations
       - Generate interprocedural projection/injection field information on call/returns
    *)
    src shapes node
      (fun accum (src_vertex, src_signal) ->
        dst shapes node src_signal
          (fun accum (dst_vertex, dst_signal) ->
            add_flow_edge ~node ~kind:(kind_f src_signal dst_signal) ~src:src_vertex ~dst:dst_vertex
              accum )
          accum )
      astate


  let add_flow ~shapes ~node ~kind ~src ~dst =
    add_flow_f ~shapes ~node ~kind_f:(fun _ _ -> kind) ~src ~dst


  let add_var_path_flow ~shapes ~node ~kind ~src ~dst ?exclude astate =
    Shapes.assert_equal_shapes shapes src dst ;
    let src = Src.var_path src in
    let dst = Dst.var_path dst in
    let src, dst =
      match exclude with
      | None ->
          (src, dst)
      | Some field_label ->
          (* Since abstraction may truncate either the source path or the destination one, we
             exclude from both. *)
          (Src.exclude_field field_label src, Dst.exclude_field field_label dst)
    in
    add_flow ~shapes ~node ~kind ~src ~dst astate
end

module TransferFunctions = struct
  module CFG = CFG
  module Domain = Domain
  module Src = Domain.Src
  module Dst = Domain.Dst

  (** The payload returned by the interprocedural analysis of dependency procedures *)
  type payload = Summary.t option * shapes

  (** Un-nest options from a payload result *)
  let join_payload (payload_result : payload AnalysisResult.t) : payload =
    match payload_result with Error _ -> (None, None) | Ok payload -> payload


  type analysis_data = shapes * payload InterproceduralAnalysis.t

  (** If an expression is made of a single variable, return it. *)
  let exp_as_single_var (e : Exp.t) : Var.t option =
    match e with
    | Exp.Lvar pvar ->
        Some (Var.of_pvar pvar)
    | Exp.Var id ->
        Some (Var.of_id id)
    | Exp.Lfield (_, _, _)
    | Exp.UnOp (_, _, _)
    | Exp.BinOp (_, _, _)
    | Exp.Exn _
    | Exp.Closure _
    | Exp.Const _
    | Exp.Cast (_, _)
    | Exp.Lindex (_, _)
    | Exp.Sizeof _ ->
        None


  (** If an expression is made of a single variable path, return it. *)
  let exp_as_single_var_path (e : Exp.t) : VarPath.t option =
    let rec aux field_path_acc = function
      | Exp.Lvar pvar ->
          Some (VarPath.make (Var.of_pvar pvar) field_path_acc)
      | Exp.Var id ->
          Some (VarPath.make (Var.of_id id) field_path_acc)
      | Exp.Lfield (e, fieldname, _) ->
          aux (FieldLabel.fieldname fieldname :: field_path_acc) e
      | Exp.UnOp (_, _, _)
      | Exp.BinOp (_, _, _)
      | Exp.Exn _
      | Exp.Closure _
      | Exp.Const _
      | Exp.Cast (_, _)
      | Exp.Lindex (_, _)
      | Exp.Sizeof _ ->
          None
    in
    aux [] e


  (** Assume an expression is made of a single variable path and return it. Dies if that assumption
      does not hold. *)
  let exp_as_single_var_path_exn exp =
    match exp_as_single_var_path exp with
    | Some var_path ->
        var_path
    | None ->
        (* Debugging hint: if you run into this assertion, the easiest is likely to change the
           frontend to introduce a temporary variable. *)
        L.die InternalError "Unexpected non-variable-path expression"


  (** Return the free cells that can be derived from a variable path *)
  let free_locals_from_path shapes var_path =
    Shapes.fold_cells shapes var_path ~f:Local.Set.add_cell ~init:Local.Set.empty


  (** Return constants and free cells that occur in [e].

      Note: we build (and return) the set, rather than for instance directly folding over its
      elements, to make sure we end up with unique free locals. *)
  let rec free_locals_of_exp shapes (e : Exp.t) : Local.Set.t =
    match e with
    | Lvar pvar ->
        free_locals_from_path shapes (VarPath.pvar pvar)
    | Var id ->
        free_locals_from_path shapes (VarPath.ident id)
    | Lfield _ ->
        (* We only allow (sequences of) fields to be "applied" to a single variable, yielding a single path *)
        let var_path = exp_as_single_var_path_exn e in
        free_locals_from_path shapes var_path
    | Const (Cint x) ->
        Local.Set.singleton (ConstantInt (IntLit.to_string x))
    | Const (Cstr x) ->
        Local.Set.singleton (ConstantString x)
    | Const (Cfun _) | Const (Cfloat _) | Const (Cclass _) ->
        Local.Set.empty
    | Closure _ ->
        Local.Set.empty
    | UnOp (_, e1, _) | Exn e1 | Cast (_, e1) ->
        free_locals_of_exp shapes e1
    | Sizeof {dynamic_length= Some e1} ->
        free_locals_of_exp shapes e1
    | Sizeof {dynamic_length= None} ->
        Local.Set.empty
    | BinOp (_, e1, e2) | Lindex (e1, e2) ->
        Local.Set.union (free_locals_of_exp shapes e1) (free_locals_of_exp shapes e2)


  (** Return variables that are captured by the closures occurring in [e]. *)
  let captured_locals_of_exp shapes (e : Exp.t) : Local.Set.t =
    let add locals {Exp.captured_vars} =
      List.fold captured_vars ~init:locals ~f:(fun locals (_exp, pvar, _typ, _mode) ->
          Local.Set.union locals (free_locals_from_path shapes (VarPath.pvar pvar)) )
    in
    Sequence.fold ~init:Local.Set.empty ~f:add (Exp.closures e)


  let lambdas_of_exp (e : Exp.t) : Procname.Set.t =
    let add procnames {Exp.name} = Procname.Set.add name procnames in
    Sequence.fold ~init:Procname.Set.empty ~f:add (Exp.closures e)


  type read_set =
    { free_locals: Local.Set.t (* constants and free variables *)
    ; captured_locals: Local.Set.t (* captured variables *)
    ; lambdas: Procname.Set.t (* names of closures *) }

  let read_set_of_exp shapes e =
    { free_locals= free_locals_of_exp shapes e
    ; captured_locals= captured_locals_of_exp shapes e
    ; lambdas= lambdas_of_exp e }


  let procname_of_exp (e : Exp.t) : Procname.t option =
    match e with Closure {name} | Const (Cfun name) -> Some name | _ -> None


  (** For all closures in [instr], record capture flows. *)
  let add_cap_flows shapes node (instr : Sil.instr) (astate : Domain.t) : Domain.t =
    let rec one_exp astate (exp : Exp.t) =
      match exp with
      | Var _ | Const _ | Lvar _ | Sizeof _ ->
          astate
      | UnOp (_, e, _) | Exn e | Cast (_, e) | Lfield (e, _, _) ->
          one_exp astate e
      | BinOp (_, e1, e2) | Lindex (e1, e2) ->
          one_exp (one_exp astate e1) e2
      | Closure c ->
          closure astate c
    and closure astate ({name; captured_vars} : Exp.closure) =
      let one_var index astate (_exp, pvar, _typ, _mode) =
        Domain.add_flow ~shapes ~node ~kind:Direct
          ~src:(Src.silence @@ Src.pvar pvar)
          ~dst:(Dst.captured_by name index) astate
      in
      List.foldi ~init:astate ~f:one_var captured_vars
    in
    List.fold ~init:astate ~f:one_exp (Sil.exps_of_instr instr)


  let warn_on_complex_arg arg_exp =
    L.debug Analysis Verbose
      "Lineage: the analysis assumes that the frontend uses only single-var expressions as actual \
       arguments, otherwise it will lose precision (found actual argument `%a` instead).@;"
      Exp.pp arg_exp


  (* Add Call flow from the concrete arguments to the special ArgumentOf nodes *)
  let add_arg_flows shapes (call_node : PPNode.t) (callee_pname : Procname.t)
      (argument_list : Exp.t list) (astate : Domain.t) : Domain.t =
    let add_one_arg_flows index astate actual_arg =
      match exp_as_single_var actual_arg with
      | None ->
          (* The Erlang frontend probably doesn't generate non-single-var arguments. If that
             happens however, we may simply collect all the locals from the actual argument and
             have them flow on the full formal parameter (that is inject into the empty list of
             nested fields) *)
          warn_on_complex_arg actual_arg ;
          let read_set = free_locals_of_exp shapes actual_arg in
          Domain.add_flow ~shapes ~node:call_node ~kind:Call ~src:(Src.local_set read_set)
            ~dst:(Dst.all_fields @@ Dst.argument_of callee_pname index)
            astate
      | Some actual_arg_var ->
          (* The concrete argument is a single var: we collect all its cells and have
             them flow onto the corresponding field path of the formal parameter. *)
          Domain.add_flow ~shapes ~node:call_node ~kind:Call ~src:(Src.var actual_arg_var)
            ~dst:(Dst.argument_of callee_pname index)
            astate
    in
    List.foldi argument_list ~init:astate ~f:add_one_arg_flows


  (* Add Return flow from the special ReturnOf nodes to the destination variable of a call *)
  let add_ret_flows shapes node (callee_pname : Procname.t) (ret_id : Ident.t) (astate : Domain.t) :
      Domain.t =
    Domain.add_flow ~shapes ~node ~kind:Return
      ~src:(Src.return_of ret_id callee_pname)
      ~dst:(Dst.ident ret_id) astate


  let add_lambda_edges shapes node written_var_path lambdas astate =
    let add_one_lambda one_lambda astate =
      Domain.add_flow ~shapes ~node ~kind:Direct
        ~dst:(Dst.all_fields @@ Dst.var_path written_var_path)
        ~src:(Src.function_ one_lambda) astate
    in
    Procname.Set.fold add_one_lambda lambdas astate


  let exec_assignment shapes node dst_path src_exp astate =
    match exp_as_single_var_path src_exp with
    | Some src_path ->
        (* Simple assignment of the form [dst := src_path]: we copy the fields of [src_path] into
           the corresponding fields of [dst]. *)
        Domain.add_var_path_flow ~shapes ~node ~kind:Direct ~src:src_path ~dst:dst_path astate
    | None ->
        (* Complex assignment of any other form: we copy every cell under the source to (every cell
           under) the destination, and also process the potential captured indices and lambdas. *)
        let {free_locals; captured_locals; lambdas} = read_set_of_exp shapes src_exp in
        astate
        |> Domain.add_flow ~shapes ~node ~kind:Direct
             ~dst:(Dst.all_fields @@ Dst.var_path dst_path)
             ~src:(Src.local_set free_locals)
        |> Domain.add_flow ~shapes ~node ~kind:Capture
             ~dst:(Dst.all_fields @@ Dst.var_path dst_path)
             ~src:(Src.local_set captured_locals)
        |> add_lambda_edges shapes node dst_path lambdas


  (* Add Summary (or Direct if this is a suppressed builtin call) edges from the concrete arguments
     to the concrete destination variable of a call, as specified by the tito_arguments summary information *)
  let add_tito (shapes : shapes) node kind_f (tito : Tito.t) (argument_list : Exp.t list)
      (ret_id : Ident.t) (astate : Domain.t) : Domain.t =
    let add_one_tito_flow ~arg_index ~arg_field_path ~ret_field_path ~shape_is_preserved astate =
      let arg_expr = List.nth_exn argument_list arg_index in
      let ret_path = VarPath.make (Var.of_id ret_id) ret_field_path in
      match exp_as_single_var_path arg_expr with
      | None ->
          warn_on_complex_arg arg_expr ;
          let kind = kind_f ~shape_is_preserved:false in
          Domain.add_flow ~shapes ~node ~kind
            ~dst:(Dst.all_fields @@ Dst.var_path ret_path)
            ~src:(Src.local_set @@ free_locals_of_exp shapes arg_expr)
            astate
      | Some arg_path ->
          let add_write ~src ~dst eta_args =
            if shape_is_preserved then Domain.add_var_path_flow ~src ~dst eta_args
            else
              Domain.add_flow
                ~src:(Src.silence @@ Src.var_path src)
                ~dst:(Dst.all_fields @@ Dst.var_path dst)
                eta_args
          in
          let kind = kind_f ~shape_is_preserved in
          add_write ~shapes ~node ~kind ~dst:ret_path
            ~src:(VarPath.sub_path arg_path arg_field_path)
            astate
    in
    Tito.fold tito ~init:astate ~f:add_one_tito_flow


  (* Add all the possible Summary/Direct (see add_tito) call edges from arguments to destination for
     when no summary is available. *)
  let add_tito_all (shapes : shapes) node kind_f (argument_list : Exp.t list) (ret_id : Ident.t)
      (astate : Domain.t) : Domain.t =
    let arity = List.length argument_list in
    let tito_full = Tito.full ~arity in
    add_tito shapes node kind_f tito_full argument_list ret_id astate


  (* Add the relevant Summary/Direct call edges from concrete arguments to the destination, depending
     on the presence of a summary. *)
  let add_summary_flows shapes node kind_f (callee_summary : Summary.t option)
      (argument_list : Exp.t list) (ret_id : Ident.t) (astate : Domain.t) : Domain.t =
    match callee_summary with
    | None ->
        add_tito_all shapes node kind_f argument_list ret_id astate
    | Some {Summary.tito_arguments} ->
        add_tito shapes node kind_f tito_arguments argument_list ret_id astate


  let generic_call_model shapes node analyze_dependency ret_id procname args astate =
    let rm_builtin = (not Config.lineage_include_builtins) && BuiltinDecl.is_declared procname in
    let if_not_builtin transform state = if rm_builtin then state else transform state in
    let kind_f ~shape_is_preserved : Edge.kind =
      if rm_builtin then Builtin else Summary {callee= procname; shape_is_preserved}
    in
    let callee_summary, _callee_shapes = join_payload (analyze_dependency procname) in
    astate |> Domain.record_supported procname
    |> if_not_builtin (add_arg_flows shapes node procname args)
    |> if_not_builtin (add_ret_flows shapes node procname ret_id)
    |> add_summary_flows shapes node kind_f callee_summary args ret_id


  module CustomModel = struct
    let nop_model _shapes _node _analyze_dependency _ret_id _procname _args astate =
      (* A model that does nothing. *)
      astate


    let with_call_flow model shapes node analyze_dependency ret_id procname args astate =
      (* A model transformer that will run the original model and add [Call] and [Return] flows as if
         it was a standard procedure.

         The idea is to use the original model to generate edges that would be more precise than the
         default Derive ones, but still have the function appear in the graph in the standard way,
         for instance to be able to find flows into it. *)
      let _ : payload AnalysisResult.t =
        (* Request procname payload to have it registered in the callgraph *)
        analyze_dependency procname
      in
      astate
      |> model shapes node analyze_dependency ret_id procname args
      |> add_arg_flows shapes node procname args
      |> add_ret_flows shapes node procname ret_id


    let call_unqualified shapes node analyze_dependency ret_id procname args astate =
      match args with
      | fun_ :: _ ->
          astate
          |> generic_call_model shapes node analyze_dependency ret_id procname args
          |> Domain.add_flow ~shapes ~node ~kind:DynamicCallFunction
               ~dst:(Dst.all_fields @@ Dst.ident ret_id)
               ~src:(Src.local_set @@ free_locals_of_exp shapes fun_)
      | _ ->
          L.die InternalError "Expecting at least one argument for '__erlang_call_unqualified'"


    let call_qualified shapes node analyze_dependency ret_id procname args astate =
      match args with
      | module_ :: fun_ :: _ ->
          astate
          |> generic_call_model shapes node analyze_dependency ret_id procname args
          |> Domain.add_flow ~shapes ~node ~kind:DynamicCallFunction
               ~dst:(Dst.all_fields @@ Dst.ident ret_id)
               ~src:(Src.local_set @@ free_locals_of_exp shapes fun_)
          |> Domain.add_flow ~shapes ~node ~kind:DynamicCallModule
               ~dst:(Dst.all_fields @@ Dst.ident ret_id)
               ~src:(Src.local_set @@ free_locals_of_exp shapes module_)
      | _ ->
          L.die InternalError "Expecting at least two arguments for '__erlang_call_qualified'"


    let make_atom shapes node _analyze_dependency ret_id _procname (args : Exp.t list) astate =
      let atom_name =
        match args with
        | Const (Cstr atom_name) :: _ ->
            atom_name
        | _ ->
            L.die InternalError "Expecting first argument of 'make_atom' to be its name"
      in
      Domain.add_flow ~shapes ~node ~kind:Direct
        ~dst:(Dst.all_fields @@ Dst.ident ret_id)
        ~src:(Src.atom atom_name) astate


    let make_tuple shapes node _analyze_dependency ret_id _procname (args : Exp.t list) astate =
      let size = List.length args in
      let tuple_type = ErlangTypeName.Tuple size in
      let field_names = ErlangTypeName.tuple_field_names size in
      let field_label name = FieldLabel.make_fieldname (ErlangType tuple_type) name in
      let ret_path name = VarPath.make (Var.of_id ret_id) [field_label name] in
      List.fold2_exn
        ~f:(fun astate field_name arg ->
          exec_assignment shapes node (ret_path field_name) arg astate )
        ~init:astate field_names args


    (** Helper to read a map element into a destination path *)
    let maps_get shapes node dst_path ~key_exp ~map_exp astate =
      (* Erlang frontend currently always puts arguments in variables. We could fall back to
         generic calls otherwise but that would introduce undesirable imprecision. We thus keep
         the failure for now and let precise support be done if the need arises. *)
      let key_path = exp_as_single_var_path_exn key_exp in
      let map_path = exp_as_single_var_path_exn map_exp in
      Shapes.fold_field_labels
        ~f:(fun astate_acc field_label ->
          Domain.add_var_path_flow ~shapes ~node ~kind:Direct
            ~src:(VarPath.sub_label map_path field_label)
            ~dst:dst_path astate_acc )
        ~fallback:
          (Domain.add_flow ~shapes ~node ~kind:Direct
             ~src:(Src.silence @@ Src.var_path map_path)
             ~dst:(Dst.all_fields @@ Dst.var_path dst_path) )
        ~init:astate shapes key_path


    let maps_get_2 shapes node _analyze_dependency ret_id _procname args astate =
      match args with
      | [key_exp; map_exp] ->
          let dst_path = VarPath.ident ret_id in
          maps_get shapes node dst_path ~key_exp ~map_exp astate
      | _ ->
          L.die InternalError "`maps:get/2` expects two arguments"


    let maps_get_3 shapes node analyze_dependency ret_id procname args astate =
      match args with
      | [key_exp; map_exp; default_exp] ->
          astate
          |> maps_get_2 shapes node analyze_dependency ret_id procname [key_exp; map_exp]
          |> exec_assignment shapes node (VarPath.ident ret_id) default_exp
      | _ ->
          L.die InternalError "`maps:get/3` expects three arguments"


    let maps_find shapes node _analyze_dependency ret_id _procname args astate =
      match args with
      | [key_exp; map_exp] ->
          let atom_ok = Src.atom "ok" in
          let atom_error = Src.atom "error" in
          let dst_tuple_path index =
            VarPath.make (Var.of_id ret_id) [FieldLabel.tuple_elem_zero_based ~size:2 ~index]
          in
          astate
          (* key is present => {ok, Value} *)
          |> Domain.add_flow ~shapes ~node ~kind:Direct ~src:atom_ok
               ~dst:(Dst.all_fields @@ Dst.var_path @@ dst_tuple_path 0)
          |> maps_get shapes node (dst_tuple_path 1) ~key_exp ~map_exp
          (* key is absent => error *)
          |> Domain.add_flow ~shapes ~node ~kind:Direct ~src:atom_error
               ~dst:(Dst.all_fields @@ Dst.ident ret_id)
      | _ ->
          L.die InternalError "`maps:find` expects three arguments"


    let maps_new =
      (* [maps:new] adds no flow by itself. Used in conjunction with {!with_call_flow}, one can
           generate a single edge from [maps:new.ret] to the ret_id destination variable. *)
      nop_model


    let maps_put shapes node _analyze_dependency ret_id _procname args astate =
      match args with
      | [key_exp; value_exp; map_exp] ->
          (* The Erlang frontend currently always puts arguments in variables. We could fall back to
             generic calls otherwise but that would introduce undesirable imprecision. We thus keep
             this assumption explicit for now and let precise support be implemented later if the
             need arises. *)
          let ret_path = VarPath.ident ret_id in
          let key_path = exp_as_single_var_path_exn key_exp in
          let value_path = exp_as_single_var_path_exn value_exp in
          let map_path = exp_as_single_var_path_exn map_exp in
          (* First copy the argument map into the returned map. *)
          let excluded_new_key =
            (* Precision optimisation: if the newly put key is statically known to be a single label,
               then the corresponding field from the source map does not flow into the resulting
               map.

               Note that we need the single-label property for this optimisation to be
               correct. Eg. on [M' = put(foo|bar, val, M)], both [M#foo] and [val] may flow into
               M'#foo. *)
            Shapes.as_field_label_singleton shapes key_path
          in
          let astate =
            Domain.add_var_path_flow ~shapes ~node ~kind:Direct ~src:map_path ~dst:ret_path
              ?exclude:excluded_new_key astate
          in
          (* Then have the put value flow into the put-key-indexed cells. *)
          Shapes.fold_field_labels
            ~f:(fun astate_acc field_label ->
              Domain.add_var_path_flow ~shapes ~node ~kind:Direct ~src:value_path
                ~dst:(VarPath.sub_label ret_path field_label)
                astate_acc )
            ~fallback:
              (Domain.add_flow ~shapes ~node ~kind:Direct
                 ~src:(Src.silence @@ Src.var_path value_path)
                 ~dst:(Dst.all_fields @@ Dst.var_path ret_path) )
            ~init:astate shapes key_path
      | _ ->
          L.die InternalError "`maps:put` expects three arguments"


    let make_map shapes node _analyze_dependency ret_id _procname args astate =
      let ret_path = VarPath.ident ret_id in
      let args = List.chunks_of ~length:2 args in
      List.fold
        ~f:(fun acc_astate key_and_value ->
          let [key_exp; value_exp] = key_and_value [@@warning "-partial-match"] in
          let key_path = exp_as_single_var_path_exn key_exp in
          let value_path = exp_as_single_var_path_exn value_exp in
          Shapes.fold_field_labels
            ~f:(fun astate_acc field_label ->
              Domain.add_var_path_flow ~shapes ~node ~kind:Direct ~src:value_path
                ~dst:(VarPath.sub_label ret_path field_label)
                astate_acc )
            ~fallback:
              (Domain.add_flow ~shapes ~node ~kind:Direct
                 ~src:(Src.silence @@ Src.var_path value_path)
                 ~dst:(Dst.all_fields @@ Dst.var_path ret_path) )
            ~init:acc_astate shapes key_path )
        ~init:astate args


    let make_cons shapes node _analyze_dependency ret_id _procname (args : Exp.t list) astate =
      let cons_type = Typ.ErlangType Cons in
      let head_field = FieldLabel.make_fieldname cons_type ErlangTypeName.cons_head in
      let tail_field = FieldLabel.make_fieldname cons_type ErlangTypeName.cons_tail in
      let ret_path field = VarPath.make (Var.of_id ret_id) [field] in
      match args with
      | [head_exp; tail_exp] ->
          astate
          |> exec_assignment shapes node (ret_path head_field) head_exp
          |> exec_assignment shapes node (ret_path tail_field) tail_exp
      | _ ->
          assert false


    let custom_call_models =
      let apply arity =
        Procname.make_erlang ~module_name:ErlangTypeName.erlang_namespace ~function_name:"apply"
          ~arity
      in
      let pairs =
        [ (BuiltinDecl.__erlang_make_atom, make_atom)
        ; (BuiltinDecl.__erlang_make_tuple, make_tuple)
        ; (BuiltinDecl.__erlang_make_map, with_call_flow make_map)
        ; ( Procname.make_erlang ~module_name:"maps" ~function_name:"new" ~arity:0
          , with_call_flow maps_new )
        ; ( Procname.make_erlang ~module_name:"maps" ~function_name:"get" ~arity:2
          , with_call_flow maps_get_2 )
        ; ( Procname.make_erlang ~module_name:"maps" ~function_name:"get" ~arity:3
          , with_call_flow maps_get_3 )
        ; ( Procname.make_erlang ~module_name:"maps" ~function_name:"find" ~arity:2
          , with_call_flow maps_find )
        ; ( Procname.make_erlang ~module_name:"maps" ~function_name:"put" ~arity:3
          , with_call_flow maps_put )
        ; (BuiltinDecl.__erlang_make_cons, make_cons)
        ; (apply 2, call_unqualified)
        ; (apply 3, call_qualified) ]
      in
      Stdlib.List.to_seq pairs |> Procname.Map.of_seq


    let get name =
      if Procname.is_erlang_call_unqualified name then Some call_unqualified
      else if Procname.is_erlang_call_qualified name then Some call_qualified
      else Procname.Map.find_opt name custom_call_models
  end

  let exec_named_call shapes node analyze_dependency ret_id name args astate : Domain.t =
    let model = CustomModel.get name |> Option.value ~default:generic_call_model in
    model shapes node analyze_dependency ret_id name args astate


  let exec_call shapes node analyze_dependency ret_id fun_exp args astate : Domain.t =
    let callee_pname = procname_of_exp fun_exp in
    let args = List.map ~f:fst args in
    match callee_pname with
    | None ->
        let arity = List.length args in
        let erlang_call_name = Procname.erlang_call_unqualified ~arity in
        exec_named_call shapes node analyze_dependency ret_id erlang_call_name (fun_exp :: args)
          astate
    | Some name ->
        exec_named_call shapes node analyze_dependency ret_id name args astate


  let exec_instr astate (shapes, {InterproceduralAnalysis.analyze_dependency; _}) node instr_index
      (instr : Sil.instr) =
    if not (Int.equal instr_index 0) then
      L.die InternalError "Lineage: INV broken: CFGs should be single instruction@\n" ;
    let astate = Domain.clear_partial_graph astate (* Don't repeat edges *) in
    let astate = add_cap_flows shapes node instr astate in
    match instr with
    | Load {id; e; _} ->
        if Ident.is_none id then astate else exec_assignment shapes node (VarPath.ident id) e astate
    | Store {e1= Lvar lhs; e2; _} ->
        exec_assignment shapes node (VarPath.pvar lhs) e2 astate
    | Store _ ->
        L.debug Analysis Verbose "Lineage: The only lhs I can handle (now) for Store is Lvar@\n" ;
        astate
    | Call ((ret_id, _ret_typ), name, args, _location, _flags) ->
        exec_call shapes node analyze_dependency ret_id name args astate
    | Sil.Prune (_, _, _, _) | Sil.Metadata _ ->
        astate


  let pp_session_name _node fmt = Format.pp_print_string fmt "Lineage"
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let unskipped_checker ({InterproceduralAnalysis.proc_desc} as analysis) (shapes : shapes) =
  let analysis_data = (shapes, analysis) in
  let cfg = CFG.from_pdesc proc_desc in
  (* Build the initial abstract state *)
  let initial_astate =
    let formals = get_formals proc_desc in
    let captured = get_captured proc_desc in
    let start_node = CFG.start_node cfg in
    let add_arg_flow i astate arg_var =
      Domain.add_flow ~shapes ~node:start_node ~kind:Direct ~src:(Domain.Src.argument i)
        ~dst:(Domain.Dst.var arg_var) astate
    in
    let add_cap_flow i astate var =
      Domain.add_flow ~shapes ~node:start_node ~kind:Direct
        ~dst:(Domain.Dst.all_fields @@ Domain.Dst.var var)
        ~src:(Domain.Src.captured i) astate
    in
    let astate = Domain.bottom in
    let astate = List.foldi ~init:astate ~f:add_arg_flow formals in
    let astate = List.foldi ~init:astate ~f:add_cap_flow captured in
    astate
  in
  (* Analyse the procedure to get the invmap *)
  let invmap = Analyzer.exec_pdesc analysis_data ~initial:initial_astate proc_desc in
  let exit_node = CFG.exit_node cfg in
  let ret_var_path = VarPath.pvar (Procdesc.get_ret_var proc_desc) in
  (* Add Return edges *)
  let exit_astate =
    match Analyzer.InvariantMap.find_opt (PPNode.id exit_node) invmap with
    | None ->
        L.die InternalError "no post for exit_node?"
    | Some {AbstractInterpreter.State.post} ->
        post
  in
  let final_astate =
    Domain.add_flow ~shapes ~node:exit_node ~kind:Direct
      ~src:(Domain.Src.var_path ret_var_path)
      ~dst:Domain.Dst.return exit_astate
  in
  (* Collect the graph from all nodes *)
  let graph =
    let collect _nodeid {AbstractInterpreter.State.post} acc_partial_graphs =
      let post_partial_graph = Domain.partial_graph post in
      post_partial_graph :: acc_partial_graphs
    in
    Analyzer.InvariantMap.fold collect invmap
      [Domain.partial_graph initial_astate; Domain.partial_graph final_astate]
    |> PartialGraph.aggregate
  in
  let exit_has_unsupported_features = Domain.has_unsupported_features exit_astate in
  let summary = Summary.make shapes proc_desc exit_has_unsupported_features graph in
  if Config.lineage_json_report then Out.report_summary proc_desc summary ;
  L.debug Analysis Verbose "@[Lineage summary for %a:@;@[%a@]@]@;" Procname.pp
    (Procdesc.get_proc_name proc_desc)
    Summary.pp summary ;
  Some summary


let checker =
  LineageBase.skip_unwanted "Lineage" ~max_size:Config.lineage_max_cfg_size unskipped_checker
