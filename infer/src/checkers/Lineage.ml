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

module Shapes = LineageShape.Summary
module FieldLabel = LineageShape.FieldLabel
module FieldPath = LineageShape.FieldPath
module Cell = LineageShape.Cell

module VarPath : sig
  (** A variable path is a pair of a variable and a possibly empty list of subscripted fields. They
      are built from their in-program occurrences. They may semantically have sub-fields themselves:
      it is the job of the {!Cell} module to determine the final graph nodes constructed from paths. *)

  (** The type of variable paths: a variable and a possibly empty list of subscripted fields. *)
  type t = Var.t * FieldPath.t

  val var : Var.t -> t

  val sub_label : t -> FieldLabel.t -> t
  (** Subscript one sub-field from a variable path.*)

  val sub_path : t -> FieldPath.t -> t
  (** Subscript nested sub-fields from a variable path. *)

  val make : Var.t -> FieldPath.t -> t

  val pvar : Pvar.t -> t

  val ident : Ident.t -> t
end = struct
  type t = Var.t * FieldPath.t

  let var v = (v, [])

  let sub_path (var, field_path) subfields = (var, field_path @ subfields)

  let sub_label var_path label = sub_path var_path [label]

  let make var field_path = (var, field_path)

  let pvar pvar = var (Var.of_pvar pvar)

  let ident id = var (Var.of_id id)
end

module Local = struct
  module T = struct
    type t =
      | ConstantAtom of string
      | ConstantInt of string
      | ConstantString of string
      | Cell of (Cell.t[@sexp.opaque])
    [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)

  let pp fmt local =
    match local with
    | ConstantAtom atom_name ->
        Format.fprintf fmt "A(%s)" atom_name
    | ConstantInt digits ->
        Format.fprintf fmt "I(%s)" digits
    | ConstantString s ->
        Format.fprintf fmt "S(%s)" s
    | Cell cell ->
        Format.fprintf fmt "V(%a)" Cell.pp cell


  module Set = struct
    include Set

    (** Shortcut for adding a Cell-variant local *)
    let add_cell set cell = add set (Cell cell)
  end
end

module LineageGraph = struct
  (** INV: Constants occur only as sources of edges. NOTE: Constants are "local" because type [data]
      associates them with a location, in a proc. *)

  module V = struct
    module T = struct
      type t =
        | Local of ((Local.t * PPNode.t)[@sexp.opaque])
        | Argument of int * FieldPath.t
        | ArgumentOf of int * (Procname.t[@sexp.opaque])
        | Captured of int
        | CapturedBy of int * (Procname.t[@sexp.opaque])
        | Return of FieldPath.t
        | ReturnOf of (Procname.t[@sexp.opaque])
        | Self
        | Function of (Procname.t[@sexp.opaque])
      [@@deriving compare, equal, sexp]
    end

    include T
    include Comparable.Make (T)

    let is_abstract_cell = function Local (Cell cell, _node) -> Cell.is_abstract cell | _ -> false

    let pp fmt vertex =
      match vertex with
      | Local (local, node) ->
          Format.fprintf fmt "%a@@%a" Local.pp local PPNode.pp node
      | Argument (index, field_path) ->
          Format.fprintf fmt "arg%d%a" index FieldPath.pp field_path
      | Captured index ->
          Format.fprintf fmt "cap%d" index
      | Return field_path ->
          Format.fprintf fmt "ret%a" FieldPath.pp field_path
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
  end

  module E = struct
    module Kind = struct
      module T = struct
        (** - INV1. There is no direct edge from ReturnOf to ArgumentOf. In that case a Return edge
              is followed by a Call edge, with a Local in the middle (which may be a temporary
              variable).
            - INV2: There is no loop, because they don't mean anything. *)
        type t =
          | Direct  (** Immediate copy; e.g., assigment or passing an argument *)
          | Call of FieldPath.t  (** Target is ArgumentOf, field path is injected into arg *)
          | Return of FieldPath.t  (** Source is ReturnOf, field path is projected from return *)
          | Capture  (** [X=1, F=fun()->X end] has Capture edge from X to F *)
          | Builtin
              (** Edge coming from a suppressed builtin call, ultimately exported as a Copy *)
          | Summary of {shape_is_preserved: bool}  (** Summarizes the effect of a procedure call *)
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
        | Call field_path ->
            Format.fprintf fmt "Call%a" FieldPath.pp field_path
        | Return field_path ->
            Format.fprintf fmt "Return%a" FieldPath.pp field_path
        | Builtin ->
            Format.fprintf fmt "Builtin"
        | Summary {shape_is_preserved} ->
            Format.fprintf fmt "Summary" ;
            if not shape_is_preserved then Format.fprintf fmt "#"
        | DynamicCallFunction ->
            Format.fprintf fmt "DynamicCallFunction"
        | DynamicCallModule ->
            Format.fprintf fmt "DynamicCallModule"
    end

    type kind = Kind.t [@@deriving compare, equal, sexp]

    type t = {source: V.t; target: V.t; kind: kind; node: (PPNode.t[@sexp.opaque])}
    [@@deriving compare, equal, sexp]

    let pp fmt {source; target; kind; node} =
      Format.fprintf fmt "@[<2>[%a@ ->@ %a@ (%a@@%a)]@]@;" V.pp source V.pp target Kind.pp kind
        PPNode.pp node


    let preserves_shape {source; target; kind; _} =
      match kind with
      | Direct ->
          Fn.non V.is_abstract_cell source && Fn.non V.is_abstract_cell target
      | Builtin ->
          (* Builtins are considered as unknown functions for shape preservation purposes.
             TODO T154077173: does this introduce undesirable imprecision? *)
          false
      | Call _ ->
          (* Target is ArgumentOf *)
          Fn.non V.is_abstract_cell source
      | Return _ ->
          (* Source is ReturnOf *)
          Fn.non V.is_abstract_cell target
      | Summary {shape_is_preserved} ->
          shape_is_preserved
      | Capture ->
          true (* TODO T154077173: investigate if more precision is worthwile *)
      | DynamicCallFunction | DynamicCallModule ->
          true (* TODO T154077173: model as a call? *)
  end

  type vertex = V.t

  type edge = E.t

  type t = edge list

  let empty = []

  let is_empty = List.is_empty

  let count_vertices edges =
    let v_list = List.concat_map ~f:(function {E.source; target} -> [source; target]) edges in
    let v_set = V.Set.of_list v_list in
    Set.length v_set


  let pp fmt edges =
    Format.fprintf fmt "@[<v 2>LineageGraph: edges %d vertices %d@;@[%a@]@]" (List.length edges)
      (count_vertices edges) (Format.pp_print_list E.pp) edges


  let add_edge ~kind ~node ~source ~target graph =
    let added = {E.source; target; kind; node} :: graph in
    match ((kind : E.kind), V.equal target source, (target : V.t), (source : V.t)) with
    | Direct, true, _, _ ->
        graph (* skip Direct loops *)
    | Builtin, true, _, _ ->
        graph (* skip Builtin loops *)
    | Summary _, true, _, _ ->
        graph (* skip Summary loops*)
    | _, true, _, _ ->
        L.die InternalError "There shall be no fancy (%a) loops!" E.Kind.pp kind
    | Call _, _, ArgumentOf _, _ ->
        added
    | Call _, _, _, _ ->
        L.die InternalError "Call edges shall return ArgumentOf!"
    | Return _, _, _, ReturnOf _ ->
        added
    | Return _, _, _, _ ->
        L.die InternalError "Return edges shall come form ReturnOf!"
    | _ ->
        added


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

      type edge_type =
        | Call
        | Capture
        | Copy
        | Derive
        | DynamicCallFunction
        | DynamicCallModule
        | Return

      type edge_metadata =
        { inject: FieldPath.t [@default []] [@yojson_drop_default.equal]
        ; project: FieldPath.t [@default []] [@yojson_drop_default.equal] }
      [@@deriving yojson_of]

      (** Returns [Some {inject; project}] metadata if at least one of them is non empty *)
      let metadata_nonempty ~project ~inject =
        match (project, inject) with [], [] -> None | _ -> Some {inject; project}


      (** Returns Some injection metadata if the field path is non empty *)
      let metadata_nonempty_inject field_path = metadata_nonempty ~project:[] ~inject:field_path

      (** Returns Some injection metadata if the field path is non empty *)
      let metadata_nonempty_project field_path = metadata_nonempty ~inject:[] ~project:field_path

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
        ; edge_metadata: edge_metadata option [@yojson.option]
        ; location: location_id }
      [@@deriving yojson_of]

      type edge = {edge: _edge} [@@deriving yojson_of]

      type _function = {name: string; has_unsupported_features: bool} [@@deriving yojson_of]

      type function_ = {function_: _function [@key "function"]} [@@deriving yojson_of]

      type entity_type = Edge | Function | Location | Node | State
      [@@deriving compare, equal, hash, sexp]
    end

    let channel_ref = ref None

    let channel () =
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

    (** Like [LineageGraph.vertex], but :

        - Without the ability to refer to other procedures, which makes it "local".
        - With some information lost/summarised, such as fields of procedure arguments/return
          (although the Derive edges will be generated taking fields into account, we only output
          one node for each argument in the Json graph to denote function calls). *)
    type data_local = Argument of int | Captured of int | Return | Normal of Local.t | Function

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

      let of_term {Json.term_data; term_type} : t =
        of_list [of_string term_data; of_term_type term_type]


      let of_kind (kind : E.kind) : t = Z.of_int (E.Kind.to_rank kind)

      let of_field_path field_path = of_string (Fmt.to_to_string FieldPath.pp field_path)

      let of_edge_metadata ({inject; project} : Json.edge_metadata) =
        of_list [of_field_path inject; of_field_path project]


      let of_option of_elt option =
        match option with None -> of_list [] | Some elt -> of_list [of_elt elt]


      (** Converts the internal representation to an [int64], as used by the [Out] module. *)
      let out id : int64 =
        try Z.to_int64 id with Z.Overflow -> L.die InternalError "Hash does not fit in int64"
    end

    let term_of_data (data : data_local) : Json.term =
      let term_data =
        match data with
        | Argument index ->
            Format.asprintf "$arg%d" index
        | Captured index ->
            Format.asprintf "$cap%d" index
        | Return ->
            "$ret"
        | Normal (Cell cell) ->
            Format.asprintf "%a" Cell.pp cell
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
        | Normal (Cell cell) ->
            if Cell.var_appears_in_source_code cell then UserVariable else TemporaryVariable
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
        type t = Json.entity_type * Int64.t [@@deriving compare, equal, hash, sexp]
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
      if Config.lineage_dedup then ( fun category id json ->
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


    let save_vertex proc_desc (vertex : V.t) =
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
      match vertex with
      | Local (var, node) ->
          save procname (Normal node) (Normal var)
      | Argument (index, _field_path) ->
          (* We don't distinguish the fields of arguments when generating Argument nodes. See
             {!type:data_local}. *)
          save procname start (Argument index)
      | ArgumentOf (index, callee_procname) ->
          save callee_procname (Start Location.dummy) (Argument index)
      | Captured index ->
          save procname start (Captured index)
      | CapturedBy (index, lambda_procname) ->
          save ~write:false lambda_procname (Start Location.dummy) (Captured index)
      | Return _field_path ->
          (* We don't distinguish the fields of the returned value when generating Return nodes. See
             {!type:data_local}. *)
          save procname exit Return
      | ReturnOf callee_procname ->
          save callee_procname (Exit Location.dummy) Return
      | Self ->
          save procname start Function
      | Function procname ->
          save ~write:false procname (Start Location.dummy) Function


    let save_edge proc_desc {E.source; target; kind; node} =
      let source_id = save_vertex proc_desc source in
      let target_id = save_vertex proc_desc target in
      let kind_id = Id.of_kind kind in
      let location_id =
        let procname = Procdesc.get_proc_name proc_desc in
        save_location ~write:true procname (Normal node)
      in
      let edge_type =
        match kind with
        | Call _ ->
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
        | Return _ ->
            Json.Return
      in
      let edge_metadata =
        match kind with
        | Builtin | Capture | Summary _ | DynamicCallFunction | DynamicCallModule ->
            None
        | Call field_path ->
            Json.metadata_nonempty_inject field_path
        | Return field_path ->
            Json.metadata_nonempty_project field_path
        | Direct -> (
          (* As the [Return ret_path] nodes will all be merged in a single [Return], we add the
             [Injection ret_path] metadata to their incoming Copy edges to encode the information
             that they're flowing into a specific field path of the returned value.

             We similarly generate projection metadata from [Argument (index, path)] nodes, that will
             be merged into a summarising [Argument index] one. *)
          match (source, target) with
          | Argument (_index, arg_path), Return ret_path ->
              (* $arg -> $ret edges may happen in infer-generated procedures (that only get
                 non-source-occurring variables by definition) *)
              Json.metadata_nonempty ~project:arg_path ~inject:ret_path
          | Argument (_index, arg_path), _ (* Not Return *) ->
              Json.metadata_nonempty_project arg_path
          | _ (* Not Argument *), Return ret_path ->
              Json.metadata_nonempty_inject ret_path
          | _ ->
              None )
      in
      let metadata_id =
        (* Contrary to other ids which are computed from the source components, this one must be
           computed on the generated metadata since it doesn't exist as-is in the source. *)
        Id.of_option Id.of_edge_metadata edge_metadata
      in
      let edge_id = Id.of_list [source_id; target_id; kind_id; metadata_id; location_id] in
      write_json Edge edge_id
        (Json.yojson_of_edge
           { edge=
               { source= Id.out source_id
               ; target= Id.out target_id
               ; edge_type
               ; edge_metadata
               ; location= Id.out location_id } } ) ;
      edge_id


    let report_summary edges has_unsupported_features proc_desc =
      let procname = Procdesc.get_proc_name proc_desc in
      let fun_id = Id.of_procname procname in
      write_json Function fun_id
        (Json.yojson_of_function_
           {function_= {name= Procname.hashable_name procname; has_unsupported_features}} ) ;
      let _fun_id = save_vertex proc_desc Self in
      let record_edge edge = ignore (save_edge proc_desc edge) in
      List.iter ~f:record_edge edges ;
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


module Tito : sig
  (** TITO stands for "taint-in taint-out". In this context a tito flow is a data path from an
      argument field to a field of the return node, without going through call edges; more
      precisely, [i#foo#bar] is a tito path to [ret#baz] if there is a path from
      [Argument (i, \[foo, bar\])] to [Return baz] not going through [ArgumentOf _] nodes.

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
      abstraction, it is equivalent to having a path from every [Argument (i, \[\])] to
      [Return \[\]].*)

  val add :
       arg_index:int
    -> arg_field_path:FieldPath.t
    -> ret_field_path:FieldPath.t
    -> shape_is_preserved:bool
    -> t
    -> t

  val fold :
       t
    -> init:'init
    -> f:
         (   arg_index:int
          -> arg_field_path:FieldPath.t
          -> ret_field_path:FieldPath.t
          -> shape_is_preserved:bool
          -> 'init
          -> 'init )
    -> 'init
end = struct
  (* Utility pretty printers *)

  let pp_arg_index = Fmt.fmt "$arg%d"

  let pp_ret = Fmt.any "$ret"

  module ArgPathSet = struct
    module FieldPathSet = struct
      (* Sets of field paths, that shall be associated to an argument index *)

      module M = Set.Make_tree (FieldPath)
      include M

      let pp ~arg_index =
        (* Prints: $argN#foo#bar $argN#other#field *)
        let pp_field_path = Fmt.(const pp_arg_index arg_index ++ FieldPath.pp) in
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
          Fmt.pf fmt "=%a%a" pp_arg_index arg_index FieldPath.pp arg_field_path
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
      Fmt.(parens @@ pair ~sep:IFmt.colon_sp Fmt.(pp_ret ++ FieldPath.pp) Sources.pp)


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

  type t = {graph: LineageGraph.t; tito_arguments: tito_arguments; has_unsupported_features: bool}

  let pp_tito_arguments fmt arguments =
    Format.fprintf fmt "@[@[<2>TitoArguments:@ {@;@[%a@]@]@,}@]" Tito.pp arguments


  let pp fmt {graph; tito_arguments} =
    Format.fprintf fmt "@;@[<v2>LineageSummary.@;%a@;%a@]" pp_tito_arguments tito_arguments
      LineageGraph.pp graph


  let tito_arguments_of_graph graph return_field_paths =
    (* - Construct an adjacency list representation of the reversed graph.
       - Collect a set of nodes that break shape preservation, due to either being abstract cells, or
         being targets of edges that have this effect. *)
    let graph_rev, shape_mixing_nodes =
      let add_edge g {LineageGraph.E.source; target; kind} =
        match kind with
        | Call _ | Return _ ->
            g (* skip call/return edges *)
        | _ ->
            Map.add_multi ~key:target ~data:source g
      in
      let add_if_shape_mixing set ({LineageGraph.E.target; _} as edge) =
        if LineageGraph.E.preserves_shape edge then set else Set.add set target
      in
      let walk_edge (g, s) edge = (add_edge g edge, add_if_shape_mixing s edge) in
      List.fold ~init:(LineageGraph.V.Map.empty, LineageGraph.V.Set.empty) ~f:walk_edge graph
    in
    (* Do a DFS, to see which arguments are reachable from a return field path. *)
    let rec dfs (shape_is_preserved, seen) node =
      if Set.mem seen node then (shape_is_preserved, seen)
      else
        let seen = Set.add seen node in
        let parents = Map.find_multi graph_rev node in
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
      dfs (true, LineageGraph.V.Set.empty) (LineageGraph.V.Return return_field_path)
    in
    (* Collect reachable arguments from a Return field path. *)
    let collect_tito tito ret_field_path =
      let shape_is_preserved, reachable = collect_reachable ret_field_path in
      Set.fold reachable ~init:tito ~f:(fun acc (node : LineageGraph.vertex) ->
          match node with
          | Argument (arg_index, arg_field_path) ->
              Tito.add ~arg_index ~arg_field_path ~ret_field_path ~shape_is_preserved acc
          | _ ->
              acc )
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
    (* Build adjacency list representation, to make DFS easy. *)
    let parents, children =
      let add_edge (parents, children) ({LineageGraph.E.source; target} as edge) =
        (Map.add_multi ~key:target ~data:edge parents, Map.add_multi ~key:source ~data:edge children)
      in
      List.fold ~init:LineageGraph.V.Map.(empty, empty) ~f:add_edge graph
    in
    (* A check for vertex interestingness, used in the graph simplification that follows. *)
    let special_variables =
      let formals = get_formals proc_desc in
      let ret_var = Var.of_pvar (Procdesc.get_ret_var proc_desc) in
      Var.Set.of_list (ret_var :: formals)
    in
    let is_interesting_vertex (vertex : LineageGraph.vertex) =
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
    let is_interesting_edge ({kind; source; target} : LineageGraph.edge) =
      match kind with
      | Direct -> (
        match (source, target) with
        | Argument (_, _ :: _), _ | _, Return (_ :: _) ->
            (* The edge will have non-empty injection/projection metadata. *)
            true
        | _ ->
            false )
      | Builtin ->
          (* Suppressed builtins are considered as direct edges for being simplification
             candidates. *)
          false
      | Call _ | Return _ | Capture | Summary _ | DynamicCallFunction | DynamicCallModule ->
          true
    in
    let merge_edges {LineageGraph.E.kind= kind_ab; source= source_ab; target= _; node= node_ab}
        {LineageGraph.E.kind= kind_bc; source= _; target= target_bc; node= node_bc} :
        LineageGraph.edge =
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
      {LineageGraph.E.kind; node; source= source_ab; target= target_bc}
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
      LineageGraph.V.Set.of_list
        (List.concat_map ~f:(function {LineageGraph.E.source; target} -> [source; target]) graph)
    in
    let todo = Set.filter ~f:(Fn.non is_interesting_vertex) todo in
    let remove_edge (parents, children) ({LineageGraph.E.source; target} as edge) =
      let rm map key =
        let edge_list = Map.find_multi map key in
        let edge_list =
          List.filter ~f:(fun elem -> not (LineageGraph.E.equal elem edge)) edge_list
        in
        Map.set map ~key ~data:edge_list
      in
      (rm parents target, rm children source)
    in
    let rec simplify todo parents children =
      match Set.choose todo with
      | None ->
          (parents, children)
      | Some vertex ->
          if is_interesting_vertex vertex then L.die InternalError "Shouldn't happen" ;
          let todo = Set.remove todo vertex in
          let before = Map.find_multi parents vertex in
          let after = Map.find_multi children vertex in
          let before_interesting = List.exists ~f:is_interesting_edge before in
          let after_interesting = List.exists ~f:is_interesting_edge after in
          let short list = match list with [] | [_] -> true | _ -> false in
          let before_short = short before in
          let after_short = short after in
          if (before_short || after_short) && not (before_interesting && after_interesting) then
            let pairs = List.cartesian_product before after in
            (* (A) remove old edges *)
            let parents, children = List.fold ~init:(parents, children) ~f:remove_edge before in
            let parents, children = List.fold ~init:(parents, children) ~f:remove_edge after in
            let do_pair (todo, (parents, children))
                ((edge_ab : LineageGraph.edge), (edge_bc : LineageGraph.edge)) =
              let keep = merge_edges edge_ab edge_bc in
              if LineageGraph.V.equal keep.source keep.target then
                L.die InternalError "OOPS: I don't work with loops." ;
              (* (B) add new edges *)
              let parents = Map.add_multi ~key:keep.target ~data:keep parents in
              let children = Map.add_multi ~key:keep.source ~data:keep children in
              (* The following two lines insert at most 1 vertex into [todo] for each [edge_ab]/
               * [edge_bc] edge that gets removed from the graph, in step (A) above. This is where
               * the earlier claim that at most m insertions happen in the main loop. However,
               * this claim is invalidated if one of the [edge_ab]/[edge_bc] edges gets re-added
               * to the graph in step (B) above, which may happen if there is an (isolated) loop
               * in the graph. *)
              let todo =
                if is_interesting_vertex keep.source then todo else Set.add todo keep.source
              in
              let todo =
                if is_interesting_vertex keep.target then todo else Set.add todo keep.target
              in
              (todo, (parents, children))
            in
            let todo, (parents, children) =
              List.fold ~init:(todo, (parents, children)) ~f:do_pair pairs
            in
            simplify todo parents children
          else simplify todo parents children
    in
    let parents, _children = simplify todo parents children in
    (* Simplified graph: from adjacency list to edge list representation.*)
    List.concat (Map.data parents)


  (** Given a graph, computes tito_arguments, and makes a summary. *)
  let make has_unsupported_features proc_desc graph return_field_paths =
    (* We need to compute the Tito arguments before reducing the graph to make use of information
       hold by temporary nodes (eg. if they're abstract or not). *)
    let tito_arguments = tito_arguments_of_graph graph return_field_paths in
    let graph =
      if Config.lineage_keep_temporaries then graph else remove_temporaries proc_desc graph
    in
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

  let bottom = LineageGraph.empty

  let is_bottom = LineageGraph.is_empty

  let leq ~lhs:_ ~rhs:_ = true

  let join _ _ = bottom

  let widen ~prev:_ ~next ~num_iters:_ = next
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
    (** Constructors for data that can be used a source node of edges in the flow graph.

        Each function corresponds to a variant of the {!LineageGraph.vertex} type.

        This module does not provide constructors for [Local]-typed sources (eg. variables), as
        [Domain] itself implements functions that directly take these values as sources and fetches
        the corresponding nodes accordingly. *)

    type t

    val function_ : Procname.t -> t

    val argument : int -> FieldPath.t -> t

    val captured : int -> t

    val return_of : Procname.t -> t
  end

  module Dst : sig
    (** Constructors for data that can be used as destination node of edges in the flow graph.

        Each function corresponds to a variant of the {!LineageGraph.vertex} type.

        As for the {!Src} module, we don't provide [Local] destinations here (that is, [Cell] ones),
        as they should be processed differently. This is done by using the [add_write_...] family of
        functions of the {!Domain} module itself. *)

    type t

    val captured_by : int -> Procname.t -> t

    val argument_of : int -> Procname.t -> t

    val return : FieldPath.t -> t
  end

  val get_lineage_partial_graph : t -> LineageGraph.edge list
  (** Extract the edges accumulated in an abstract state. *)

  val clear_graph : t -> t
  (** Forget the collected edges. One can use this when going to analyse another instruction as the
      finalisation of the procedure analysis will go through all nodes to collect the complete edges
      set. *)

  val has_unsupported_features : t -> bool
  (** Check if the abstract state recorded unsupported Erlang features. *)

  val record_supported : Procname.t -> t -> t
  (** If the given procedure is an unsupported Erlang feature, record that in the abstract state. *)

  val add_flow_from_local_set :
    node:PPNode.t -> kind:LineageGraph.E.Kind.t -> src:(Local.t, _) Set.t -> dst:Dst.t -> t -> t

  (** {2 Add flow to non-variable nodes} *)

  (** The [add_flow_...] functions can be used to record a taint flow from various types of sources
      to destinations that are not written variables. To record flow to a variable, use the
      corresponding [add_write_...] function instead. *)

  val add_flow_from_path :
       shapes:Shapes.t
    -> node:PPNode.t
    -> kind:LineageGraph.E.Kind.t
    -> src:VarPath.t
    -> dst:Dst.t
    -> t
    -> t

  val add_flow_from_path_f :
       shapes:Shapes.t
    -> node:PPNode.t
    -> kind_f:(FieldPath.t -> LineageGraph.E.Kind.t)
    -> src:VarPath.t
    -> dst_f:(FieldPath.t -> Dst.t)
    -> t
    -> t
  (** [add_flow_from_path_f] allows recording flow whose kind and destination can be different for
      every cell under the source variable path. This can be used for instance to record injecting
      [Call] edges where the edge holds the field information.

      [kind_f] and [dst_f] will be passed the full field path of each relevant cell, including
      fields already present in the [src] path. *)

  (** {2 Add flow to non-variable nodes} *)

  (** The [add_write_...] functions can be used to record a taint flow from various types of sources
      to written variable destinations. To record flow to another type of node, use the
      corresponding [add_flow_...] function instead. *)

  val add_write :
       shapes:Shapes.t
    -> node:PPNode.t
    -> kind:LineageGraph.E.Kind.t
    -> src:Src.t
    -> dst:VarPath.t
    -> t
    -> t

  val add_write_f :
       shapes:Shapes.t
    -> node:PPNode.t
    -> kind_f:(FieldPath.t -> LineageGraph.E.Kind.t)
    -> src_f:(FieldPath.t -> Src.t)
    -> dst:VarPath.t
    -> t
    -> t
  (** [add_write_f] allows recording flow whose kind and source can be different for every cell
      under the destination variable path. This can be used for instance to record projecting
      [Return] edges where the edge holds the field information.

      [kind_f] and [src_f] will be passed the full field path of each relevant cell, including
      fields already present in the [dst] path. *)

  val add_write_from_local :
       shapes:Shapes.t
    -> node:PPNode.t
    -> kind:LineageGraph.E.Kind.t
    -> src:Local.t
    -> dst:VarPath.t
    -> t
    -> t

  val add_write_from_local_set :
       shapes:Shapes.t
    -> node:PPNode.t
    -> kind:LineageGraph.E.Kind.t
    -> src:(Local.t, _) Set.t
    -> dst:VarPath.t
    -> t
    -> t

  val add_write_parallel :
       shapes:Shapes.t
    -> node:PPNode.t
    -> kind:LineageGraph.E.Kind.t
    -> src:VarPath.t
    -> dst:VarPath.t
    -> ?exclude:(src_field_path:FieldPath.t -> dst_field_path:FieldPath.t -> bool)
    -> t
    -> t
  (** Add flow from every cell under the source variable path to the cell with the same field path
      under the destination variable path. See {!Shapes.fold_cell_pairs}.

      If the [exclude] predicate is set, flows for which the predicates holds (on the flow source
      and destination paths) won't be added. *)

  val add_write_product :
       shapes:Shapes.t
    -> node:PPNode.t
    -> kind:LineageGraph.E.Kind.t
    -> src:VarPath.t
    -> dst:VarPath.t
    -> t
    -> t
  (** Add flow from every cell under the source variable path to every cell under the destination
      variable path. *)
end = struct
  module Real = struct
    module LastWrites = AbstractDomain.FiniteMultiMap (Cell) (PPNode)
    module UnsupportedFeatures = AbstractDomain.BooleanOr
    include AbstractDomain.PairWithBottom (LastWrites) (UnsupportedFeatures)
  end

  module Unit = PartialSummary
  include AbstractDomain.PairWithBottom (Real) (Unit)

  let get_lineage_partial_graph (_, graph) = graph

  let clear_graph (real, _graph) = (real, [])

  let has_unsupported_features ((_, has_unsupported_features), _) = has_unsupported_features

  let record_supported name (((last_writes, has_unsupported_features), local_graph) : t) : t =
    let has_unsupported_features =
      has_unsupported_features || Procname.is_erlang_unsupported name
    in
    ((last_writes, has_unsupported_features), local_graph)


  module Src = struct
    type t = LineageGraph.vertex

    let function_ procname : t = Function procname

    let argument i field_path : t = Argument (i, field_path)

    let captured i : t = Captured i

    let return_of procname : t = ReturnOf procname

    module Private = struct
      (** A module grouping functions that should only be used inside Domain and not exported. *)

      (** Fold over the sources of a given local. *)
      let fold_local ~f ~init node ((last_writes, _), _) (local : Local.t) =
        match local with
        | ConstantAtom _ | ConstantInt _ | ConstantString _ ->
            f init (LineageGraph.V.Local (local, node))
        | Cell cell ->
            let source_nodes = Real.LastWrites.get_all cell last_writes in
            List.fold
              ~f:(fun acc node -> f acc (LineageGraph.V.Local (local, node)))
              ~init source_nodes
    end
  end

  module Dst = struct
    type t = LineageGraph.vertex

    let captured_by i proc_name : t = CapturedBy (i, proc_name)

    let argument_of i callee_pname : t = ArgumentOf (i, callee_pname)

    let return field_path : t = Return field_path

    module Private = struct
      (** A module grouping functions that should only be used inside Domain and not exported. *)

      let cell node cell : t = Local (Cell cell, node)
    end
  end

  let update_write ~node ~var_path ((last_writes, has_unsupported_features), graph) =
    let last_writes = Real.LastWrites.set_to_single_value var_path node last_writes in
    ((last_writes, has_unsupported_features), graph)


  let add_edge ~node ~kind ~src ~dst ((last_writes, has_unsupported_features), graph) : t =
    let graph = LineageGraph.add_edge ~node ~kind ~source:src ~target:dst graph in
    ((last_writes, has_unsupported_features), graph)


  let add_flow_from_local ~node ~kind ~src ~dst astate : t =
    Src.Private.fold_local
      ~f:(fun acc_astate one_source -> add_edge ~node ~kind ~src:one_source ~dst acc_astate)
      ~init:astate node astate src


  let add_flow_from_local_set ~node ~kind ~src ~dst astate =
    Set.fold
      ~f:(fun acc_astate one_local -> add_flow_from_local ~node ~kind ~src:one_local ~dst acc_astate)
      ~init:astate src


  let add_flow_from_path ~shapes ~node ~kind ~src ~dst astate =
    Shapes.fold_cells
      ~f:(fun acc src_cell -> add_flow_from_local ~node ~kind ~src:(Cell src_cell) ~dst acc)
      ~init:astate shapes src


  let add_flow_from_path_f ~shapes ~node ~kind_f ~src ~dst_f astate =
    Shapes.fold_cells
      ~f:(fun acc src_cell ->
        let source_field_path = Cell.field_path src_cell in
        add_flow_from_local ~node ~kind:(kind_f source_field_path) ~src:(Cell src_cell)
          ~dst:(dst_f source_field_path) acc )
      ~init:astate shapes src


  let add_cell_write ~node ~kind ~src ~dst astate =
    astate
    |> add_edge ~node ~kind ~src ~dst:(Dst.Private.cell node dst)
    |> update_write ~node ~var_path:dst


  let add_cell_write_from_local ~node ~kind ~src ~dst astate =
    astate
    |> add_flow_from_local ~node ~kind ~src ~dst:(Dst.Private.cell node dst)
    |> update_write ~node ~var_path:dst


  let add_cell_write_from_local_set ~node ~kind ~src ~dst astate =
    astate
    |> add_flow_from_local_set ~node ~kind ~src ~dst:(Dst.Private.cell node dst)
    |> update_write ~node ~var_path:dst


  (* Update all the cells under a path, as obtained from the shapes information. *)
  let add_write ~shapes ~node ~kind ~src ~dst astate =
    Shapes.fold_cells
      ~f:(fun acc_astate dst_cell -> add_cell_write ~node ~kind ~src ~dst:dst_cell acc_astate)
      ~init:astate shapes dst


  let add_write_f ~shapes ~node ~kind_f ~src_f ~dst astate =
    Shapes.fold_cells
      ~f:(fun acc_astate dst_cell ->
        let dst_field_path = Cell.field_path dst_cell in
        add_cell_write ~node ~kind:(kind_f dst_field_path) ~src:(src_f dst_field_path) ~dst:dst_cell
          acc_astate )
      ~init:astate shapes dst


  (* Update all the cells under a path, as obtained from the shapes information. *)
  let add_write_from_local ~shapes ~node ~kind ~src ~dst astate =
    Shapes.fold_cells
      ~f:(fun acc_astate dst_cell ->
        add_cell_write_from_local ~node ~kind ~src ~dst:dst_cell acc_astate )
      ~init:astate shapes dst


  (* Update all the cells under a path, as obtained from the shapes information. *)
  let add_write_from_local_set ~shapes ~node ~kind ~src ~dst astate =
    Shapes.fold_cells
      ~f:(fun acc_astate dst_cell ->
        add_cell_write_from_local_set ~node ~kind ~src ~dst:dst_cell acc_astate )
      ~init:astate shapes dst


  (* Update all the cells under a destination path, as obtained from the shapes information, as being
     written in parallel from the corresponding cells under a source path. *)
  let add_write_parallel ~shapes ~node ~kind ~src ~dst
      ?(exclude = fun ~src_field_path:_ ~dst_field_path:_ -> false) astate =
    Shapes.fold_cell_pairs
      ~f:(fun acc_astate src_cell dst_cell ->
        if
          exclude ~src_field_path:(Cell.field_path src_cell)
            ~dst_field_path:(Cell.field_path dst_cell)
        then acc_astate
        else add_cell_write_from_local ~node ~kind ~src:(Cell src_cell) ~dst:dst_cell acc_astate )
      ~init:astate shapes src dst


  let add_write_product ~shapes ~node ~kind ~src ~dst astate =
    Shapes.fold_cells
      ~f:(fun acc_astate src_cell ->
        add_write_from_local ~shapes ~node ~kind ~src:(Cell src_cell) ~dst acc_astate )
      ~init:astate shapes src
end

module TransferFunctions = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = Shapes.t * Summary.t InterproceduralAnalysis.t

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


  (** Return constants and free cells that occur in [e]. *)
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
        Domain.add_flow_from_path ~shapes ~node ~kind:Direct ~src:(VarPath.pvar pvar)
          ~dst:(Domain.Dst.captured_by index name)
          astate
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
          Domain.add_flow_from_local_set ~node:call_node ~kind:(Call []) ~src:read_set
            ~dst:(Domain.Dst.argument_of index callee_pname)
            astate
      | Some actual_arg_var ->
          (* The concrete argument is a single var: we collect all its cells and have
             them flow onto the corresponding field path of the formal parameter. *)
          let actual_arg_var_path = VarPath.var actual_arg_var in
          Domain.add_flow_from_path_f ~shapes ~node:call_node
            ~kind_f:(fun arg_field_path -> Call arg_field_path)
            ~src:actual_arg_var_path
            ~dst_f:(fun _ -> Domain.Dst.argument_of index callee_pname)
            astate
    in
    List.foldi argument_list ~init:astate ~f:add_one_arg_flows


  (* Add Return flow from the special ReturnOf nodes to the destination variable of a call *)
  let add_ret_flows shapes node (callee_pname : Procname.t) (ret_id : Ident.t) (astate : Domain.t) :
      Domain.t =
    Domain.add_write_f ~shapes ~node
      ~kind_f:(fun field_path -> Return field_path)
      ~src_f:(Fn.const @@ Domain.Src.return_of callee_pname)
      ~dst:(VarPath.ident ret_id) astate


  let add_lambda_edges shapes node written_var_path lambdas astate =
    let add_one_lambda one_lambda astate =
      Domain.add_write ~shapes ~node ~kind:Direct ~dst:written_var_path
        ~src:(Domain.Src.function_ one_lambda) astate
    in
    Procname.Set.fold add_one_lambda lambdas astate


  let exec_assignment shapes node dst_path src_exp astate =
    match exp_as_single_var_path src_exp with
    | Some src_path ->
        (* Simple assignment of the form [dst := src_path]: we copy the fields of [src_path] into
           the corresponding fields of [dst]. *)
        Domain.add_write_parallel ~shapes ~node ~kind:Direct ~src:src_path ~dst:dst_path astate
    | None ->
        (* Complex assignment of any other form: we copy every cell under the source to (every cell
           under) the destination, and also process the potential captured indices and lambdas. *)
        let {free_locals; captured_locals; lambdas} = read_set_of_exp shapes src_exp in
        astate
        |> Domain.add_write_from_local_set ~shapes ~node ~kind:Direct ~dst:dst_path ~src:free_locals
        |> Domain.add_write_from_local_set ~shapes ~node ~kind:Capture ~dst:dst_path
             ~src:captured_locals
        |> add_lambda_edges shapes node dst_path lambdas


  (* Add Summary (or Direct if this is a suppressed builtin call) edges from the concrete arguments
     to the concrete destination variable of a call, as specified by the tito_arguments summary information *)
  let add_tito (shapes : Shapes.t) node kind_f (tito : Tito.t) (argument_list : Exp.t list)
      (ret_id : Ident.t) (astate : Domain.t) : Domain.t =
    let add_one_tito_flow ~arg_index ~arg_field_path ~ret_field_path ~shape_is_preserved astate =
      let arg_expr = List.nth_exn argument_list arg_index in
      let ret_path = VarPath.make (Var.of_id ret_id) ret_field_path in
      match exp_as_single_var_path arg_expr with
      | None ->
          warn_on_complex_arg arg_expr ;
          let kind = kind_f ~shape_is_preserved:false in
          Domain.add_write_from_local_set ~shapes ~node ~kind ~dst:ret_path
            ~src:(free_locals_of_exp shapes arg_expr)
            astate
      | Some arg_path ->
          let add_write eta_args =
            if shape_is_preserved then Domain.add_write_parallel eta_args
            else Domain.add_write_product eta_args
          in
          let kind = kind_f ~shape_is_preserved in
          add_write ~shapes ~node ~kind ~dst:ret_path
            ~src:(VarPath.sub_path arg_path arg_field_path)
            astate
    in
    Tito.fold tito ~init:astate ~f:add_one_tito_flow


  (* Add all the possible Summary/Direct (see add_tito) call edges from arguments to destination for
     when no summary is available. *)
  let add_tito_all (shapes : Shapes.t) node kind_f (argument_list : Exp.t list) (ret_id : Ident.t)
      (astate : Domain.t) : Domain.t =
    let arity = List.length argument_list in
    let tito_full = Tito.full ~arity in
    add_tito shapes node kind_f tito_full argument_list ret_id astate


  (* Add the relevant Summary/Direct call edges from concrete arguments to the destination, depending
     on the presence of a summary. *)
  let add_summary_flows shapes node kind_f (callee : Summary.t option) (argument_list : Exp.t list)
      (ret_id : Ident.t) (astate : Domain.t) : Domain.t =
    match callee with
    | None ->
        add_tito_all shapes node kind_f argument_list ret_id astate
    | Some {Summary.tito_arguments} ->
        add_tito shapes node kind_f tito_arguments argument_list ret_id astate


  let generic_call_model shapes node analyze_dependency ret_id procname args astate =
    let rm_builtin = (not Config.lineage_include_builtins) && BuiltinDecl.is_declared procname in
    let if_not_builtin transform state = if rm_builtin then state else transform state in
    let kind_f ~shape_is_preserved : LineageGraph.E.kind =
      if rm_builtin then Builtin else Summary {shape_is_preserved}
    in
    astate |> Domain.record_supported procname
    |> if_not_builtin (add_arg_flows shapes node procname args)
    |> if_not_builtin (add_ret_flows shapes node procname ret_id)
    |> add_summary_flows shapes node kind_f (analyze_dependency procname) args ret_id


  module CustomModel = struct
    let call_unqualified shapes node analyze_dependency ret_id procname args astate =
      match args with
      | fun_ :: _ ->
          astate
          |> generic_call_model shapes node analyze_dependency ret_id procname args
          |> Domain.add_write_from_local_set ~shapes ~node ~kind:DynamicCallFunction
               ~dst:(VarPath.ident ret_id) ~src:(free_locals_of_exp shapes fun_)
      | _ ->
          L.die InternalError "Expecting at least one argument for '__erlang_call_unqualified'"


    let call_qualified shapes node analyze_dependency ret_id procname args astate =
      match args with
      | module_ :: fun_ :: _ ->
          astate
          |> generic_call_model shapes node analyze_dependency ret_id procname args
          |> Domain.add_write_from_local_set ~shapes ~node ~kind:DynamicCallFunction
               ~dst:(VarPath.ident ret_id) ~src:(free_locals_of_exp shapes fun_)
          |> Domain.add_write_from_local_set ~shapes ~node ~kind:DynamicCallModule
               ~dst:(VarPath.ident ret_id)
               ~src:(free_locals_of_exp shapes module_)
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
      Domain.add_write_from_local ~shapes ~node ~kind:Direct ~dst:(VarPath.ident ret_id)
        ~src:(ConstantAtom atom_name) astate


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
          Domain.add_write_parallel ~shapes ~node ~kind:Direct
            ~src:(VarPath.sub_label map_path field_label)
            ~dst:dst_path astate_acc )
        ~fallback:(Domain.add_write_product ~shapes ~node ~kind:Direct ~src:map_path ~dst:dst_path)
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
          let atom_ok = Local.ConstantAtom "ok" in
          let atom_error = Local.ConstantAtom "error" in
          let dst_tuple_path index =
            VarPath.make (Var.of_id ret_id) [FieldLabel.tuple_elem_zero_based ~size:2 ~index]
          in
          astate
          (* key is present => {ok, Value} *)
          |> Domain.add_write_from_local ~shapes ~node ~kind:Direct ~src:atom_ok
               ~dst:(dst_tuple_path 0)
          |> maps_get shapes node (dst_tuple_path 1) ~key_exp ~map_exp
          (* key is absent => error *)
          |> Domain.add_write_from_local ~shapes ~node ~kind:Direct ~src:atom_error
               ~dst:(VarPath.ident ret_id)
      | _ ->
          L.die InternalError "`maps:find` expects three arguments"


    let maps_new =
      (* The generic call model with zero parameter will simply add a flow from maps:new$ret to
         ret_id. We could also consider doing nothing and simply return the abstract state, which
         would amount to considering maps:new as a zero-argument builtin and would generate no flow
         at all. *)
      generic_call_model


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
          (* First copy the argument map into the returned map, except the new key. *)
          let exclude_new_key ~src_field_path ~dst_field_path =
            Shapes.fold_field_labels
              ~f:(fun acc field_label ->
                (* Since abstraction may truncate either the copied path or the returned one, we
                   check that both are separate from the newly put key. *)
                acc
                || [%equal: FieldLabel.t option] (List.hd src_field_path) (Some field_label)
                || [%equal: FieldLabel.t option] (List.hd dst_field_path) (Some field_label) )
              ~fallback:(Fn.const false) ~init:false shapes key_path
          in
          let astate =
            Domain.add_write_parallel ~shapes ~node ~kind:Direct ~src:map_path ~dst:ret_path
              ~exclude:exclude_new_key astate
          in
          (* Then have the put value flow into the put-key-indexed cells. *)
          Shapes.fold_field_labels
            ~f:(fun astate_acc field_label ->
              Domain.add_write_parallel ~shapes ~node ~kind:Direct ~src:value_path
                ~dst:(VarPath.sub_label ret_path field_label)
                astate_acc )
            ~fallback:
              (Domain.add_write_product ~shapes ~node ~kind:Direct ~src:value_path ~dst:ret_path)
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
              Domain.add_write_parallel ~shapes ~node ~kind:Direct ~src:value_path
                ~dst:(VarPath.sub_label ret_path field_label)
                astate_acc )
            ~fallback:
              (Domain.add_write_product ~shapes ~node ~kind:Direct ~src:value_path ~dst:ret_path)
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
        ; (BuiltinDecl.__erlang_make_map, make_map)
        ; (Procname.make_erlang ~module_name:"maps" ~function_name:"new" ~arity:0, maps_new)
        ; (Procname.make_erlang ~module_name:"maps" ~function_name:"get" ~arity:2, maps_get_2)
        ; (Procname.make_erlang ~module_name:"maps" ~function_name:"get" ~arity:3, maps_get_3)
        ; (Procname.make_erlang ~module_name:"maps" ~function_name:"find" ~arity:2, maps_find)
        ; (Procname.make_erlang ~module_name:"maps" ~function_name:"put" ~arity:3, maps_put)
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
    let astate = Domain.clear_graph astate (* Don't repeat edges *) in
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

let unskipped_checker ({InterproceduralAnalysis.proc_desc} as analysis) shapes_opt =
  let shapes =
    match shapes_opt with
    | Some shapes ->
        shapes
    | None ->
        L.die InternalError "Failed to compute shape information for %a" Procname.pp
          (Procdesc.get_proc_name proc_desc)
  in
  let analysis_data = (shapes, analysis) in
  let cfg = CFG.from_pdesc proc_desc in
  (* Build the initial abstract state *)
  let initial_astate =
    let formals = get_formals proc_desc in
    let captured = get_captured proc_desc in
    let start_node = CFG.start_node cfg in
    let add_arg_flow i astate arg_var =
      TransferFunctions.Domain.add_write_f ~shapes ~node:start_node
        ~kind_f:(Fn.const LineageGraph.E.Kind.Direct)
        ~dst:(VarPath.var arg_var) ~src_f:(Domain.Src.argument i) astate
    in
    let add_cap_flow i astate var =
      TransferFunctions.Domain.add_write ~shapes ~node:start_node ~kind:Direct
        ~dst:(VarPath.var var) ~src:(Domain.Src.captured i) astate
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
    Domain.add_flow_from_path_f ~shapes ~node:exit_node
      ~kind_f:(Fn.const LineageGraph.E.Kind.Direct)
      ~src:ret_var_path ~dst_f:Domain.Dst.return exit_astate
  in
  (* Collect the graph from all nodes *)
  let graph =
    let collect _nodeid {AbstractInterpreter.State.post} edges =
      let post_edges = Domain.get_lineage_partial_graph post in
      post_edges :: edges
    in
    Analyzer.InvariantMap.fold collect invmap
      [ Domain.get_lineage_partial_graph initial_astate
      ; Domain.get_lineage_partial_graph final_astate ]
    |> List.concat
  in
  (* Collect the return fields to finish the summary *)
  let known_ret_field_paths =
    Shapes.fold_cells shapes ret_var_path ~init:[] ~f:(fun acc cell -> Cell.field_path cell :: acc)
  in
  let exit_has_unsupported_features = Domain.has_unsupported_features exit_astate in
  let summary = Summary.make exit_has_unsupported_features proc_desc graph known_ret_field_paths in
  if Config.lineage_json_report then Summary.report summary proc_desc ;
  L.debug Analysis Verbose "@[Lineage summary for %a:@;@[%a@]@]@;" Procname.pp
    (Procdesc.get_proc_name proc_desc)
    Summary.pp summary ;
  Some summary


let checker = LineageUtils.skip_unwanted unskipped_checker
