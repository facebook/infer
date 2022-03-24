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
end

module LineageGraph = struct
  (** INV: Constants occur only as sources of flow. NOTE: Constants are "local" because type [data]
      associates them with a location, in a proc. *)
  type local =
    | ConstantAtom of string
    | ConstantInt of string
    | ConstantString of string
    | Variable of (Var.t[@sexp.opaque])
  [@@deriving compare, sexp]

  type data =
    | Local of ((local * PPNode.t)[@sexp.opaque])
    | Argument of int
    | ArgumentOf of int * (Procname.t[@sexp.opaque])
    | Captured of int
    | CapturedBy of int * (Procname.t[@sexp.opaque])
    | Return
    | ReturnOf of (Procname.t[@sexp.opaque])
    | Self
    | Function of (Procname.t[@sexp.opaque])
  [@@deriving compare, sexp]

  module FlowKind = struct
    module T = struct
      (** Order is important: see how [compare_flow_kind] is used. *)
      type t =
        | Direct  (** Immediate copy; e.g., assigment or passing an argument *)
        | Capture  (** [X=1, F=fun()->X end] has Capture flow from X to F *)
        | DynamicCall  (** [X=F(args)] has DynamicCall flow from F to X *)
        | Summary  (** Summarizes the effect of a procedure call *)
      [@@deriving compare, sexp, variants]
    end

    include T
    include Comparable.Make (T)
  end

  type flow_kind = FlowKind.t [@@deriving compare, sexp]

  let rank_of_flow_kind = FlowKind.Variants.to_rank

  type flow = {source: data; target: data; kind: flow_kind; node: (PPNode.t[@sexp.opaque])}
  [@@deriving compare, sexp]

  let flow ?(kind = FlowKind.Direct) ~node ~source ~target () = {source; target; kind; node}

  type t = flow list

  let pp_local fmt local =
    match local with
    | ConstantAtom atom_name ->
        Format.fprintf fmt "A(%s)" atom_name
    | ConstantInt digits ->
        Format.fprintf fmt "I(%s)" digits
    | ConstantString s ->
        Format.fprintf fmt "S(%s)" s
    | Variable variable ->
        Format.fprintf fmt "V(%a)" Var.pp variable


  let pp_data fmt data =
    match data with
    | Local (local, node) ->
        Format.fprintf fmt "%a@@%a" pp_local local PPNode.pp node
    | Argument index ->
        Format.fprintf fmt "arg%d" index
    | Captured index ->
        Format.fprintf fmt "cap%d" index
    | Return ->
        Format.fprintf fmt "ret"
    | CapturedBy (index, proc_name) ->
        Format.fprintf fmt "%a.cap%d" Procname.pp proc_name index
    | ArgumentOf (index, proc_name) ->
        Format.fprintf fmt "%a.arg%d" Procname.pp proc_name index
    | ReturnOf proc_name ->
        Format.fprintf fmt "%a.ret" Procname.pp proc_name
    | Function proc_name ->
        Format.fprintf fmt "%a.fun" Procname.pp proc_name
    | Self ->
        Format.fprintf fmt "self"


  let pp_flow_kind fmt kind =
    match (kind : flow_kind) with
    | Capture ->
        Format.fprintf fmt "Capture"
    | Direct ->
        Format.fprintf fmt "Direct"
    | DynamicCall ->
        Format.fprintf fmt "DynamicCall"
    | Summary ->
        Format.fprintf fmt "Summary"


  let pp_flow fmt {source; target; kind} =
    Format.fprintf fmt "@[<2>[%a@ ->@ %a@ (%a)]@]@;" pp_data source pp_data target pp_flow_kind kind


  let pp fmt flows =
    Format.fprintf fmt "@;@[<v 2>LineageGraph@;@[%a@]@]" (Format.pp_print_list pp_flow) flows


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
        { term_data: string [@key "name" (* T106560112 *)]
        ; term_type: term_type [@key "variable_type" (* T106560112 *)] }
      [@@deriving yojson_of]

      type _node = {id: node_id; state: state_id; term: term [@key "variable" (* T106560112 *)]}
      [@@deriving yojson_of]

      type node = {node: _node} [@@deriving yojson_of]

      type edge_type = Capture | Copy | Derive | DynamicCall

      let yojson_of_edge_type typ =
        match typ with
        | Capture ->
            `String "Capture"
        | Copy ->
            `String "Copy"
        | Derive ->
            `String "Derive"
        | DynamicCall ->
            `String "DynamicCall"


      type _edge = {source: node_id; target: node_id; edge_type: edge_type; location: location_id}
      [@@deriving yojson_of]

      type edge = {edge: _edge} [@@deriving yojson_of]

      type _function = {name: string; has_unsupported_features: bool} [@@deriving yojson_of]

      type function_ = {function_: _function [@key "function"]} [@@deriving yojson_of]

      type entity_type = Edge | Function | Location | Node | State
      [@@deriving compare, hash, sexp]
    end

    let channel_ref = ref None

    let channel () =
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

    (** Like [LineageGraph.data], but without the ability to refer to other procedures, which makes
        it "local". *)
    type data_local = Argument of int | Captured of int | Return | Normal of local | Function

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
        Sequence.memoize (run (gen (Random.State.make [|Config.simple_lineage_seed|])))


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

      let of_term {Json.term_data; term_type} : t =
        of_list [of_string term_data; of_term_type term_type]


      let of_kind (kind : flow_kind) : t = Z.of_int (rank_of_flow_kind kind)

      (** Converts the internal representation to an [int64], as used by the [Out] module. *)
      let out id : int64 =
        try Z.to_int64 id with Z.Overflow -> L.die InternalError "Hash does not fit in int64"
    end

    let term_of_data (data : data_local) : Json.term =
      let term_data =
        match data with
        | Argument index ->
            Printf.sprintf "$arg%d" index
        | Captured index ->
            Printf.sprintf "$cap%d" index
        | Return ->
            "$ret"
        | Normal (Variable x) ->
            Format.asprintf "%a" Var.pp x
        | Normal (ConstantAtom x) | Normal (ConstantInt x) | Normal (ConstantString x) ->
            x
        | Function ->
            "$fun"
      in
      let term_type : Json.term_type =
        match data with
        | Argument _ | Captured _ ->
            Argument
        | Return ->
            Return
        | Normal (Variable x) ->
            if Var.appears_in_source_code x then UserVariable else TemporaryVariable
        | Normal (ConstantAtom _) ->
            ConstantAtom
        | Normal (ConstantInt _) ->
            ConstantInt
        | Normal (ConstantString _) ->
            ConstantString
        | Function ->
            Function
      in
      {Json.term_data; term_type}


    module JsonCacheKey = struct
      module T = struct
        type t = Json.entity_type * Int64.t [@@deriving compare, hash, sexp]
      end

      include T
      include Hashable.Make (T)
    end

    let write_json_cache = JsonCacheKey.Hash_set.create ()

    let write_json =
      let really_write_json json =
        Yojson.Safe.to_channel (channel ()) json ;
        Out_channel.newline (channel ())
      in
      if Config.simple_lineage_dedup then ( fun category id json ->
        let key = (category, Id.out id) in
        if not (Hash_set.mem write_json_cache key) then (
          Hash_set.add write_json_cache key ;
          really_write_json json ) )
      else fun _category _id json -> really_write_json json


    let save_location ~write procname (state_local : state_local) : Id.t =
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
        write_json Location location_id
          (Json.yojson_of_location {location= {id= Id.out location_id; function_; file; line}}) ) ;
      location_id


    let save_state ~write procname (state : state_local) : Id.t =
      let location_id = save_location ~write procname state in
      if write then
        write_json State location_id
          (Json.yojson_of_state {state= {id= Id.out location_id; location= Id.out location_id}}) ;
      location_id


    let save_node proc_desc (data : data) =
      let save ?(write = true) procname state_local data_local =
        let state_id = save_state ~write procname state_local in
        let term = term_of_data data_local in
        let node_id = Id.of_list [state_id; Id.of_term term] in
        if write then
          write_json Node node_id
            (Json.yojson_of_node {node= {id= Id.out node_id; state= Id.out state_id; term}}) ;
        node_id
      in
      let procname = Procdesc.get_proc_name proc_desc in
      let start = Start (Procdesc.Node.get_loc (Procdesc.get_start_node proc_desc)) in
      let exit = Exit (Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc)) in
      match data with
      | Local (var, node) ->
          save procname (Normal node) (Normal var)
      | Argument index ->
          save procname start (Argument index)
      | ArgumentOf (index, callee_procname) ->
          save callee_procname (Start Location.dummy) (Argument index)
      | Captured index ->
          save procname start (Captured index)
      | CapturedBy (index, lambda_procname) ->
          save ~write:false lambda_procname (Start Location.dummy) (Captured index)
      | Return ->
          save procname exit Return
      | ReturnOf callee_procname ->
          save callee_procname (Exit Location.dummy) Return
      | Self ->
          save procname start Function
      | Function procname ->
          save ~write:false procname (Start Location.dummy) Function


    let save_edge proc_desc {source; target; kind; node} =
      let source_id = save_node proc_desc source in
      let target_id = save_node proc_desc target in
      let kind_id = Id.of_kind kind in
      let edge_id = Id.of_list [source_id; target_id; kind_id] in
      let edge_type =
        match kind with
        | Capture ->
            Json.Capture
        | Direct ->
            Json.Copy
        | DynamicCall ->
            Json.DynamicCall
        | Summary ->
            Json.Derive
      in
      let location_id =
        let procname = Procdesc.get_proc_name proc_desc in
        save_location ~write:true procname (Normal node)
      in
      write_json Edge edge_id
        (Json.yojson_of_edge
           { edge=
               { source= Id.out source_id
               ; target= Id.out target_id
               ; edge_type
               ; location= Id.out location_id } } ) ;
      edge_id


    let report_summary flows has_unsupported_features proc_desc =
      let procname = Procdesc.get_proc_name proc_desc in
      let fun_id = Id.of_procname procname in
      write_json Function fun_id
        (Json.yojson_of_function_
           {function_= {name= Procname.hashable_name procname; has_unsupported_features}} ) ;
      let _fun_id = save_node proc_desc Self in
      let record_flow flow = ignore (save_edge proc_desc flow) in
      List.iter ~f:record_flow flows ;
      Out_channel.flush (channel ()) ;
      Hash_set.clear write_json_cache
  end

  let report = Out.report_summary
end

(** Helper function. *)
let get_formals proc_desc : Var.t list =
  let f (pvar, _typ) = Var.of_pvar pvar in
  List.map ~f (Procdesc.get_pvar_formals proc_desc)


(** Helper function. *)
let get_captured proc_desc : Var.t list =
  let f {CapturedVar.pvar} = Var.of_pvar pvar in
  List.map ~f (Procdesc.get_captured proc_desc)


module Summary = struct
  (** TITO stands for "taint-in taint-out". In this context a tito argument is one that has a path
      to the return node, without going through call edges; more precisely, [i] is a tito argument
      if there is a path from [Argument i] to [Return] not going through [ArgumentOf _] nodes. *)
  type tito_arguments = IntSet.t

  type t = {graph: LineageGraph.t; tito_arguments: tito_arguments; has_unsupported_features: bool}

  let pp_tito_arguments fmt arguments =
    let pp_sep fmt () = Format.fprintf fmt ",@;" in
    Format.fprintf fmt "@;@[<2>TitoArguments@;%a@]"
      (Format.pp_print_list ~pp_sep Int.pp)
      (IntSet.elements arguments)


  let pp fmt {graph; tito_arguments} =
    Format.fprintf fmt "@;@[<2>LineageSummary@;%a%a@]" pp_tito_arguments tito_arguments
      LineageGraph.pp graph


  (** Make [LineageGraph.data] usable in Maps/Sets. *)
  module Vertex = struct
    module T = struct
      type t = LineageGraph.data [@@deriving compare, sexp]
    end

    include T
    include Comparable.Make (T)
  end

  (** Make [LineageGraph.flow] usable in Maps/Sets. *)
  module Edge = struct
    module T = struct
      type t = LineageGraph.flow [@@deriving compare, sexp]
    end

    include T
    include Comparable.Make (T)
  end

  let tito_arguments_of_graph graph =
    (* Construct an adjacency list representation of the reversed graph. *)
    let graph_rev =
      let add_flow g {LineageGraph.source; target} =
        match target with
        | ArgumentOf _ ->
            g (* skip call edges *)
        | _ ->
            Map.add_multi ~key:target ~data:source g
      in
      List.fold ~init:Vertex.Map.empty ~f:add_flow graph
    in
    (* Do a DFS, to see which arguments are reachable from return. *)
    let rec dfs seen node =
      if Set.mem seen node then seen
      else
        let seen = Set.add seen node in
        let parents = Map.find_multi graph_rev node in
        List.fold ~init:seen ~f:dfs parents
    in
    let reachable = dfs Vertex.Set.empty LineageGraph.Return in
    (* Collect reachable arguments. *)
    let tito_arguments =
      let add_node args (node : LineageGraph.data) =
        match node with Argument i -> IntSet.add i args | _ -> args
      in
      Set.fold reachable ~init:IntSet.empty ~f:add_node
    in
    tito_arguments


  (** A vertex is uninteresting if it is of the form [Local(x,_)], where x is a variable that does
      not appear in source or a formal parameter or the return variable. The returned graph has an
      edge X0->Xn of type T between interesting vertices X0 and Xn iff the input graph has a path
      X0->X1->...->Xn with X1,...,X(n-1) being uninteresting and T is the maximum of the types of
      the edges in the path. The location of the X0->Xn edge is the location of the first edge of
      type T in the path X0->X1->...->Xn. *)
  let remove_temporaries proc_desc graph =
    (* Build adjacency list representation, to make DFS easy. *)
    let children =
      let add_flow g {LineageGraph.source; target; kind; node} =
        Map.add_multi ~key:source ~data:(target, kind, node) g
      in
      List.fold ~init:Vertex.Map.empty ~f:add_flow graph
    in
    (* A check for vertex interestingness, used during the DFS that comes next. *)
    let special_variables =
      let formals = get_formals proc_desc in
      let ret_var = Var.of_pvar (Procdesc.get_ret_var proc_desc) in
      Var.Set.of_list (ret_var :: formals)
    in
    let is_interesting (data : LineageGraph.data) =
      match data with
      | Local (Variable var, _node) ->
          Var.appears_in_source_code var && not (Var.Set.mem var special_variables)
      | Argument _ | Captured _ | Return | CapturedBy _ | ArgumentOf _ | ReturnOf _ ->
          true
      | Local (ConstantAtom _, _) | Local (ConstantInt _, _) | Local (ConstantString _, _) ->
          true
      | Function _ | Self ->
          true
    in
    (* Do a DFS and produce edges. *)
    let rec dfs (seen, result_graph) ({LineageGraph.source; target; kind; node} as state) =
      if not (is_interesting source) then L.die InternalError "INV broken" ;
      if not (Set.mem seen state) then
        let seen = Set.add seen state in
        if is_interesting target then (seen, state :: result_graph)
        else
          let f (seen, result_graph) (target, edge_kind, edge_node) =
            let kind, node =
              if LineageGraph.compare_flow_kind kind edge_kind > 0 then (kind, node)
              else (edge_kind, edge_node)
            in
            dfs (seen, result_graph) (LineageGraph.flow ~kind ~node ~source ~target ())
          in
          List.fold ~init:(seen, result_graph) ~f (Map.find_multi children target)
      else (seen, result_graph)
    in
    let start_dfs (seen, result_graph) ({LineageGraph.source} as state) =
      if is_interesting source then dfs (seen, result_graph) state else (seen, result_graph)
    in
    let _seen, result_graph = List.fold ~init:(Edge.Set.empty, []) ~f:start_dfs graph in
    result_graph


  (** Given a graph, computes tito_arguments, and makes a summary. *)
  let make has_unsupported_features proc_desc graph =
    let graph =
      if Config.simple_lineage_keep_temporaries then graph else remove_temporaries proc_desc graph
    in
    let tito_arguments = tito_arguments_of_graph graph in
    {graph; tito_arguments; has_unsupported_features}


  let report {graph; has_unsupported_features} proc_desc =
    LineageGraph.report graph has_unsupported_features proc_desc
end

(** A summary is computed by taking the union of all partial summaries present in the final
    invariant map. Partial summaries are stored in abstract states only because it is convenient.
    But, they do not influence how abstract states are joined, widened, etc, which explains why
    below all functions having to do with the abstract domain are dummies. *)
module PartialSummary = struct
  type t = LineageGraph.t

  let pp = LineageGraph.pp

  let bottom = []

  let leq ~lhs:_ ~rhs:_ = true

  let join _ _ = bottom

  let widen ~prev:_ ~next ~num_iters:_ = next
end

module Domain = struct
  module Real = struct
    module LastWrites = AbstractDomain.FiniteMultiMap (Var) (PPNode)
    module UnsupportedFeatures = AbstractDomain.BooleanOr
    include AbstractDomain.Pair (LastWrites) (UnsupportedFeatures)
  end

  module Unit = PartialSummary
  include AbstractDomain.Pair (Real) (Unit)
end

module TransferFunctions = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = Summary.t InterproceduralAnalysis.t

  let get_written_var (instr : Sil.instr) : Var.t option =
    match instr with
    | Load {id} ->
        if Ident.is_none id then None else Some (Var.of_id id)
    | Store {e1= Lvar lhs} ->
        Some (Var.of_pvar lhs)
    | Store _ ->
        L.debug Analysis Verbose
          "SimpleLineage: The only lhs I can handle (now) for Store is Lvar@\n" ;
        None
    | Prune _ ->
        None
    | Call _ ->
        L.die InternalError "exec_instr should special-case this"
    | Metadata _ ->
        None


  (** Make [LineageGraph.local] usable in Maps/Sets. *)
  module Local = struct
    module T = struct
      type t = LineageGraph.local [@@deriving compare, sexp]
    end

    include T
    include Comparable.Make (T)
  end

  (** Return constants and free variables that occur in [e]. *)
  let free_locals_of_exp (e : Exp.t) : Local.Set.t =
    let rec gather locals (e : Exp.t) =
      match e with
      | Lvar pvar ->
          Local.Set.add locals (Variable (Var.of_pvar pvar))
      | Var id ->
          Local.Set.add locals (Variable (Var.of_id id))
      | Const (Cint x) ->
          Local.Set.add locals (ConstantInt (IntLit.to_string x))
      | Const (Cstr x) ->
          Local.Set.add locals (ConstantString x)
      | Const (Cfun _) | Const (Cfloat _) | Const (Cclass _) ->
          locals
      | Closure _ ->
          locals
      | UnOp (_, e1, _) | Exn e1 | Cast (_, e1) | Lfield (e1, _, _) ->
          gather locals e1
      | Sizeof {dynamic_length= Some e1} ->
          gather locals e1
      | Sizeof {dynamic_length= None} ->
          locals
      | BinOp (_, e1, e2) | Lindex (e1, e2) ->
          gather (gather locals e1) e2
    in
    gather Local.Set.empty e


  (** Return variables that are captured by the closures occurring in [e]. *)
  let captured_locals_of_exp (e : Exp.t) : Local.Set.t =
    let add locals {Exp.captured_vars} =
      List.fold captured_vars ~init:locals ~f:(fun locals (_exp, pvar, _typ, _mode) ->
          Local.Set.add locals (Variable (Var.of_pvar pvar)) )
    in
    Sequence.fold ~init:Local.Set.empty ~f:add (Exp.closures e)


  let lambdas_of_exp (e : Exp.t) : Procname.Set.t =
    let add procnames {Exp.name} = Procname.Set.add name procnames in
    Sequence.fold ~init:Procname.Set.empty ~f:add (Exp.closures e)


  let free_locals_of_exp_list (es : Exp.t list) : Local.Set.t =
    Local.Set.union_list (List.rev_map ~f:free_locals_of_exp es)


  type read_set =
    { free_locals: Local.Set.t (* constants and free variables *)
    ; captured_locals: Local.Set.t (* captured variables *)
    ; lambdas: Procname.Set.t (* names of closures *) }

  let get_read_set (instr : Sil.instr) : read_set =
    let of_exp e =
      { free_locals= free_locals_of_exp e
      ; captured_locals= captured_locals_of_exp e
      ; lambdas= lambdas_of_exp e }
    in
    match instr with
    | Load {e} ->
        of_exp e
    | Store {e2} ->
        of_exp e2
    | Prune (e, _location, _true, _kind) ->
        of_exp e
    | Call _ ->
        L.die InternalError "exec_instr should special-case this"
    | Metadata _ ->
        let dummy = Exp.bool false in
        of_exp dummy


  let procname_of_exp (e : Exp.t) : Procname.t option =
    match e with Closure {name} | Const (Cfun name) -> Some name | _ -> None


  let sources_of_local here last_writes (local : Local.t) : LineageGraph.data list =
    match local with
    | ConstantAtom _ | ConstantInt _ | ConstantString _ ->
        [LineageGraph.Local (local, here)]
    | Variable variable ->
        let source_nodes = Domain.Real.LastWrites.get_all variable last_writes in
        List.map ~f:(fun node -> LineageGraph.Local (local, node)) source_nodes


  (** For all closures in [instr], record capture flows. *)
  let add_cap_flows node (instr : Sil.instr) (astate : Domain.t) : Domain.t =
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
      let one_var index ((last_writes, has_unsupported_features), local_edges)
          (_exp, pvar, _typ, _mode) =
        let local : Local.t = Variable (Var.of_pvar pvar) in
        let source_list = sources_of_local node last_writes local in
        let add_one_flow local_edges source =
          LineageGraph.flow ~node ~source ~target:(CapturedBy (index, name)) () :: local_edges
        in
        let local_edges = List.fold source_list ~f:add_one_flow ~init:local_edges in
        ((last_writes, has_unsupported_features), local_edges)
      in
      List.foldi ~init:astate ~f:one_var captured_vars
    in
    List.fold ~init:astate ~f:one_exp (Sil.exps_of_instr instr)


  let add_arg_flows (call_node : PPNode.t) (callee_pname : Procname.t) (argument_list : Exp.t list)
      (astate : Domain.t) : Domain.t =
    let add_flows_all_to_arg index ((last_writes, has_unsupported_features), local_edges) arg =
      let read_set = free_locals_of_exp arg in
      let add_flows_local_to_arg local_edges (local : Local.t) =
        let sources = sources_of_local call_node last_writes local in
        let add_one_flow local_edges source =
          LineageGraph.flow ~node:call_node ~source ~target:(ArgumentOf (index, callee_pname)) ()
          :: local_edges
        in
        List.fold sources ~f:add_one_flow ~init:local_edges
      in
      let local_edges = Set.fold read_set ~init:local_edges ~f:add_flows_local_to_arg in
      ((last_writes, has_unsupported_features), local_edges)
    in
    List.foldi argument_list ~init:astate ~f:add_flows_all_to_arg


  let add_ret_flows (callee_pname : Procname.t) (ret_id : Ident.t) node (astate : Domain.t) :
      Domain.t =
    let last_writes, local_edges = astate in
    let local_edges =
      LineageGraph.flow ~node ~source:(ReturnOf callee_pname)
        ~target:(Local (Variable (Var.of_id ret_id), node))
        ()
      :: local_edges
    in
    (last_writes, local_edges)


  let update_write kind ((write_var, write_node) : Var.t * PPNode.t) (read_set : Local.Set.t)
      (((last_writes, has_unsupported_features), local_edges) : Domain.t) : Domain.t =
    let target = LineageGraph.Local (Variable write_var, write_node) in
    let add_read local_edges (read_local : Local.t) =
      let sources =
        match read_local with
        | ConstantAtom _ | ConstantInt _ | ConstantString _ ->
            [LineageGraph.Local (read_local, write_node)]
        | Variable read_var ->
            let source_nodes = Domain.Real.LastWrites.get_all read_var last_writes in
            List.map ~f:(fun node -> LineageGraph.Local (read_local, node)) source_nodes
      in
      let add_edge local_edges source =
        LineageGraph.flow ~node:write_node ~kind ~source ~target () :: local_edges
      in
      let local_edges = List.fold ~init:local_edges ~f:add_edge sources in
      local_edges
    in
    let local_edges = Set.fold read_set ~init:local_edges ~f:add_read in
    let last_writes = Domain.Real.LastWrites.remove_all write_var last_writes in
    let last_writes = Domain.Real.LastWrites.add write_var write_node last_writes in
    ((last_writes, has_unsupported_features), local_edges)


  let add_tito tito_arguments (argument_list : Exp.t list) (ret_id : Ident.t) node
      (astate : Domain.t) : Domain.t =
    let tito_locals =
      let tito_exps =
        List.filter_mapi
          ~f:(fun index arg -> if IntSet.mem index tito_arguments then Some arg else None)
          argument_list
      in
      free_locals_of_exp_list tito_exps
    in
    update_write LineageGraph.FlowKind.Summary (Var.of_id ret_id, node) tito_locals astate


  let add_tito_all (argument_list : Exp.t list) (ret_id : Ident.t) node (astate : Domain.t) :
      Domain.t =
    let all = IntSet.of_list (List.mapi argument_list ~f:(fun index _ -> index)) in
    add_tito all argument_list ret_id node astate


  let add_summary_flows (callee : (Procdesc.t * Summary.t) option) (argument_list : Exp.t list)
      (ret_id : Ident.t) node (astate : Domain.t) : Domain.t =
    match callee with
    | None ->
        add_tito_all argument_list ret_id node astate
    | Some (_callee_pdesc, {Summary.tito_arguments}) ->
        add_tito tito_arguments argument_list ret_id node astate


  let record_supported name (((last_writes, has_unsupported_features), local_edges) : Domain.t) :
      Domain.t =
    let has_unsupported_features =
      has_unsupported_features || Procname.is_erlang_unsupported name
    in
    ((last_writes, has_unsupported_features), local_edges)


  let custom_call_models =
    let make_atom astate node _analyze_dependency ret_id _procname (args : Exp.t list) =
      let atom_name =
        match args with
        | Const (Cstr atom_name) :: _ ->
            atom_name
        | _ ->
            L.die InternalError "Expecting first argument of 'make_atom' to be its name"
      in
      let sources = Local.Set.singleton (ConstantAtom atom_name) in
      update_write LineageGraph.FlowKind.Direct (Var.of_id ret_id, node) sources astate
    in
    let pairs = [(BuiltinDecl.__erlang_make_atom, make_atom)] in
    Stdlib.List.to_seq pairs |> Procname.Map.of_seq


  let generic_call_model astate node analyze_dependency ret_id procname args =
    astate |> record_supported procname |> add_arg_flows node procname args
    |> add_ret_flows procname ret_id node
    |> add_summary_flows (analyze_dependency procname) args ret_id node


  let exec_call (astate : Domain.t) node analyze_dependency ret_id fun_exp args : Domain.t =
    let callee_pname = procname_of_exp fun_exp in
    let args = List.map ~f:fst args in
    match callee_pname with
    | None ->
        let sources = free_locals_of_exp fun_exp in
        let astate =
          update_write LineageGraph.FlowKind.DynamicCall (Var.of_id ret_id, node) sources astate
        in
        add_tito_all args ret_id node astate
    | Some name ->
        let model =
          Procname.Map.find_opt name custom_call_models |> Option.value ~default:generic_call_model
        in
        model astate node analyze_dependency ret_id name args


  let add_lambda_edges (write_var, write_node) lambdas (real_state, local_edges) =
    let target = LineageGraph.Local (Variable write_var, write_node) in
    let add_one_lambda one_lambda local_edges =
      LineageGraph.flow ~node:write_node ~source:(Function one_lambda) ~target () :: local_edges
    in
    let local_edges = Procname.Set.fold add_one_lambda lambdas local_edges in
    (real_state, local_edges)


  let exec_noncall astate node instr =
    let written_var = get_written_var instr in
    let {free_locals; captured_locals; lambdas} = get_read_set instr in
    match written_var with
    | None ->
        astate
    | Some written_var ->
        astate
        |> update_write LineageGraph.FlowKind.Direct (written_var, node) free_locals
        |> update_write LineageGraph.FlowKind.Capture (written_var, node) captured_locals
        |> add_lambda_edges (written_var, node) lambdas


  let exec_instr astate {InterproceduralAnalysis.analyze_dependency} node instr_index
      (instr : Sil.instr) =
    if not (Int.equal instr_index 0) then
      L.die InternalError "SimpleLineage: INV broken: CFGs should be single instruction@\n" ;
    let astate = add_cap_flows node instr (fst astate, [ (* don't repeat edges*) ]) in
    match instr with
    | Call ((ret_id, _ret_typ), name, args, _location, _flags) ->
        exec_call astate node analyze_dependency ret_id name args
    | _ ->
        exec_noncall astate node instr


  let pp_session_name _node fmt = Format.pp_print_string fmt "SimpleLineage"
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let proc_size = Procdesc.size proc_desc in
  let too_big =
    Option.value_map ~default:false
      ~f:(fun limit -> proc_size > limit)
      Config.simple_lineage_max_cfg_size
  in
  if too_big then (
    L.user_warning "Skipped large (%d) procedure (%a)@." proc_size Procname.pp
      (Procdesc.get_proc_name proc_desc) ;
    None )
  else
    let cfg = CFG.from_pdesc proc_desc in
    let initial =
      let formals = get_formals proc_desc in
      let captured = get_captured proc_desc in
      let start_node = CFG.start_node cfg in
      let add_arg last_writes arg = Domain.Real.LastWrites.add arg start_node last_writes in
      let last_writes =
        List.fold ~init:Domain.Real.LastWrites.bottom ~f:add_arg (captured @ formals)
      in
      let local_edges =
        List.mapi
          ~f:(fun i var ->
            LineageGraph.flow ~node:start_node ~source:(Argument i)
              ~target:(Local (Variable var, start_node))
              () )
          formals
        @ List.mapi
            ~f:(fun i var ->
              LineageGraph.flow ~node:start_node ~source:(Captured i)
                ~target:(Local (Variable var, start_node))
                () )
            captured
      in
      let has_unsupported_features = false in
      ((last_writes, has_unsupported_features), local_edges)
    in
    let invmap = Analyzer.exec_pdesc analysis_data ~initial proc_desc in
    let (exit_last_writes, exit_has_unsupported_features), _ =
      match Analyzer.InvariantMap.find_opt (PPNode.id (CFG.exit_node cfg)) invmap with
      | None ->
          L.die InternalError "no post for exit_node?"
      | Some {AbstractInterpreter.State.post} ->
          post
    in
    let graph =
      let collect _nodeid {AbstractInterpreter.State.post} edges =
        let _last_writes, post_edges = post in
        post_edges :: edges
      in
      let ret_var = Var.of_pvar (Procdesc.get_ret_var proc_desc) in
      let mk_ret_edge ret_node =
        LineageGraph.flow ~node:ret_node
          ~source:(Local (Variable ret_var, ret_node))
          ~target:Return ()
      in
      List.map ~f:mk_ret_edge (Domain.Real.LastWrites.get_all ret_var exit_last_writes)
      @ (Analyzer.InvariantMap.fold collect invmap [snd initial] |> List.concat)
    in
    let summary = Summary.make exit_has_unsupported_features proc_desc graph in
    if Config.simple_lineage_json_report then Summary.report summary proc_desc ;
    Some summary
