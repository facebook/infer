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

module Fields = struct
  (** A module to help manipulating lists of (nested) fields. *)

  (** We define a type alias to be able use our own json exporting function *)
  type fieldname = Fieldname.t [@@deriving compare, equal, sexp]

  let yojson_of_fieldname fieldname =
    (* The ppx-generated [yojson_of] is unnecessarily complex for our purposes (it's a record that
       involves "class_name", which incidentally seems to always be "_".) *)
    `String (Fieldname.to_string fieldname)


  (* The list is to be understood in "syntactic order": [["a"; "b"]] represents the field part of
     [X#a#b].*)
  type t = fieldname list [@@deriving compare, equal, sexp, yojson_of]

  let pp = Fmt.(list ~sep:nop (any "#" ++ Fieldname.pp))
end

module VariableIndex : sig
  (** A [VariableIndex] is a variable and a possibly empty list of subscripted fields. *)

  (** The type {!t} has a phantom parameter, which is used to statically encode the information that
      some variable indexes are "terminal", which means that no subfield of them will be considered
      by the analysis (either because they have none, or that would lead to too deep or too wide
      field sequences).

      Only terminal indices can be used to build the lineage graph. *)

  type terminal

  (** For indices that are not known to be terminal *)
  type transient

  type _ t

  val var : Var.t -> transient t

  val subfield : _ t -> Fields.t -> transient t
  (** Sub-field of an index. *)

  val get_var : _ t -> Var.t

  val get_fields : terminal t -> Fields.t

  val make : Var.t -> Fields.t -> transient t

  val pvar : Pvar.t -> transient t

  val ident : Ident.t -> transient t

  val pp : Format.formatter -> _ t -> unit

  val var_appears_in_source_code : _ t -> bool

  val fold_terminal :
    SimpleShape.Summary.t -> _ t -> init:'accum -> f:('accum -> terminal t -> 'accum) -> 'accum
  (** Given an index, fold the [f] function over all the terminal indices that can be obtained as
      "sub fields" of the index. *)

  val concat_map_terminal : SimpleShape.Summary.t -> _ t -> f:(terminal t -> 'a list) -> 'a list
  (** Given an index, accumulates the results of the [f] function computed over all the terminal
      indices that can be obtained as "sub fields" of the index. *)

  val fold_terminal_pairs :
       SimpleShape.Summary.t
    -> _ t
    -> _ t
    -> init:'accum
    -> f:('accum -> terminal t -> terminal t -> 'accum)
    -> 'accum
  (** Given two indices that must have the same type, fold the [f] function over all the pairs of
      terminal indices that can be obtained as "sub fields" of the indices. [f] will always be
      called on corresponding sub-indices: see {!SimpleShape.Summary.fold_terminal_fields_2}. *)

  module Terminal : sig
    (** Utility module for standard functor calls *)

    type nonrec t = terminal t [@@deriving compare, equal]

    val pp : Format.formatter -> t -> unit
  end
end = struct
  type terminal

  type transient

  type _ t = Var.t * Fields.t [@@deriving compare, equal]

  let var v = (v, [])

  let subfield (var, fields) subfields = (var, fields @ subfields)

  let make var fields = (var, fields)

  let get_var (v, _) = v

  let get_fields (_, fields) = fields

  let pvar pvar = var (Var.of_pvar pvar)

  let ident id = var (Var.of_id id)

  let pp fmt (var, fields) = Format.fprintf fmt "%a%a" Var.pp var Fields.pp fields

  let var_appears_in_source_code (var, _) = Var.appears_in_source_code var

  let max_depth = Config.simple_lineage_field_depth

  let max_width = Option.value ~default:Int.max_value Config.simple_lineage_field_width

  let prevent_cycles = Config.simple_lineage_prevent_cycles

  let fold_terminal shapes (var, fields) ~init ~f =
    SimpleShape.Summary.fold_terminal_fields shapes (var, fields) ~max_width ~max_depth
      ~prevent_cycles ~init ~f:(fun acc fields -> f acc (make var fields))


  let concat_map_terminal shapes (var, fields) ~f =
    SimpleShape.Summary.concat_map_terminal_fields shapes (var, fields) ~max_width ~max_depth
      ~prevent_cycles ~f:(fun fields -> f (make var fields))


  let fold_terminal_pairs shapes (var1, fields1) (var2, fields2) ~init ~f =
    SimpleShape.Summary.fold_terminal_fields_2 shapes (var1, fields1) (var2, fields2) ~max_width
      ~max_depth ~prevent_cycles ~init ~f:(fun acc fields1 fields2 ->
        f acc (make var1 fields1) (make var2 fields2) )


  module Terminal = struct
    type nonrec t = terminal t

    (* Ignore the phantom type variable in comparisons *)

    let compare = compare [%compare: _]

    let equal = equal [%equal: _]

    let pp = pp
  end
end

module LineageGraph = struct
  (** INV: Constants occur only as sources of flow. NOTE: Constants are "local" because type [data]
      associates them with a location, in a proc. *)
  type local =
    | ConstantAtom of string
    | ConstantInt of string
    | ConstantString of string
    | VariableIndex of (VariableIndex.Terminal.t[@sexp.opaque])
  [@@deriving compare, equal, sexp]

  type data =
    | Local of ((local * PPNode.t)[@sexp.opaque])
    | Argument of int * Fields.t
    | ArgumentOf of int * (Procname.t[@sexp.opaque])
    | Captured of int
    | CapturedBy of int * (Procname.t[@sexp.opaque])
    | Return
    | ReturnOf of (Procname.t[@sexp.opaque])
    | Self
    | Function of (Procname.t[@sexp.opaque])
  [@@deriving compare, equal, sexp]

  module FlowKind = struct
    module T = struct
      (** - INV1. There is no direct flow from ReturnOf to ArgumentOf. In that case a Return flow is
            followed by a Call flow, with a Local in the middle (which may be a temporary variable).
          - INV2: There is no loop, because they don't mean anything. *)
      type t =
        | Direct  (** Immediate copy; e.g., assigment or passing an argument *)
        | Inject of Fields.t
        | Project of Fields.t
        | Call of Fields.t  (** Target is ArgumentOf, fields are injected into arg *)
        | Return of Fields.t  (** Source is ReturnOf, fields are projected from return *)
        | Capture  (** [X=1, F=fun()->X end] has Capture flow from X to F *)
        | Summary  (** Summarizes the effect of a procedure call *)
        | DynamicCallFunction
        | DynamicCallModule
      [@@deriving compare, equal, sexp, variants]
    end

    include T
    include Comparable.Make (T)
  end

  type flow_kind = FlowKind.t [@@deriving compare, equal, sexp]

  let rank_of_flow_kind = FlowKind.Variants.to_rank

  type flow = {source: data; target: data; kind: flow_kind; node: (PPNode.t[@sexp.opaque])}
  [@@deriving compare, equal, sexp]

  type t = flow list

  (** Make [data] usable in Maps/Sets. *)
  module Vertex = struct
    module T = struct
      type t = data [@@deriving compare, equal, sexp]
    end

    include T
    include Comparable.Make (T)
  end

  let pp_local fmt local =
    match local with
    | ConstantAtom atom_name ->
        Format.fprintf fmt "A(%s)" atom_name
    | ConstantInt digits ->
        Format.fprintf fmt "I(%s)" digits
    | ConstantString s ->
        Format.fprintf fmt "S(%s)" s
    | VariableIndex variable ->
        Format.fprintf fmt "V(%a)" VariableIndex.pp variable


  let pp_data fmt data =
    match data with
    | Local (local, node) ->
        Format.fprintf fmt "%a@@%a" pp_local local PPNode.pp node
    | Argument (index, fields) ->
        Format.fprintf fmt "arg%d%a" index Fields.pp fields
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
    | Call fields ->
        Format.fprintf fmt "Call%a" Fields.pp fields
    | Inject fields ->
        Format.fprintf fmt "Inject%a" Fields.pp fields
    | Project fields ->
        Format.fprintf fmt "Project%a" Fields.pp fields
    | Return fields ->
        Format.fprintf fmt "Return%a" Fields.pp fields
    | Summary ->
        Format.fprintf fmt "Summary"
    | DynamicCallFunction ->
        Format.fprintf fmt "DynamicCallFunction"
    | DynamicCallModule ->
        Format.fprintf fmt "DynamicCallModule"


  let pp_flow fmt {source; target; kind; node} =
    Format.fprintf fmt "@[<2>[%a@ ->@ %a@ (%a@@%a)]@]@;" pp_data source pp_data target pp_flow_kind
      kind PPNode.pp node


  let vertices flows =
    let data = List.concat_map ~f:(function {source; target} -> [source; target]) flows in
    Vertex.Set.of_list data


  let count_data flows = Set.length (vertices flows)

  let pp fmt flows =
    Format.fprintf fmt "@;@[<v 2>LineageGraph flows %d data %d@;@[%a@]@]" (List.length flows)
      (count_data flows) (Format.pp_print_list pp_flow) flows


  let add_flow ~kind ~node ~source ~target graph =
    let added = {source; target; kind; node} :: graph in
    match ((kind : FlowKind.t), equal_data target source, target, source) with
    | Direct, true, _, _ ->
        graph (* skip Direct loops *)
    | Summary, true, _, _ ->
        graph (* skip Summary loops*)
    | _, true, _, _ ->
        L.die InternalError "There shall be no fancy (%a) loops!" pp_flow_kind kind
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
        { inject: Fields.t [@default []] [@yojson_drop_default.equal]
        ; project: Fields.t [@default []] [@yojson_drop_default.equal] }
      [@@deriving yojson_of]

      let metadata_inject fields = {inject= fields; project= []}

      let metadata_project fields = {inject= []; project= fields}

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

    (** Like [LineageGraph.data], but :

        - Without the ability to refer to other procedures, which makes it "local".
        - With some information lost/summarised, such as fields of procedure arguments (although the
          Derive edges will be generated taking fields into account, we only output one node for
          each argument in the Json graph to denote function calls). *)
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
            Format.asprintf "$arg%d" index
        | Captured index ->
            Format.asprintf "$cap%d" index
        | Return ->
            "$ret"
        | Normal (VariableIndex x) ->
            Format.asprintf "%a" VariableIndex.pp x
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
        | Normal (VariableIndex x) ->
            if VariableIndex.var_appears_in_source_code x then UserVariable else TemporaryVariable
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
      | Argument (index, _fields) ->
          (* We don't distinguish the fields of arguments when generating Argument nodes. See
             {!type:data_local}. *)
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
      let location_id =
        let procname = Procdesc.get_proc_name proc_desc in
        save_location ~write:true procname (Normal node)
      in
      let edge_id = Id.of_list [source_id; target_id; kind_id; location_id] in
      let edge_type =
        match kind with
        | Call _ ->
            Json.Call
        | Capture ->
            Json.Capture
        | Direct ->
            Json.Copy
        | Inject _ ->
            Json.Copy
        | Project _ ->
            Json.Copy
        | Summary ->
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
        | Direct | Capture | Summary | DynamicCallFunction | DynamicCallModule ->
            None
        | Call [] | Return [] ->
            (* Don't generate "trivial" metadata *)
            None
        | Call fields | Inject fields ->
            Some (Json.metadata_inject fields)
        | Return fields | Project fields ->
            Some (Json.metadata_project fields)
      in
      write_json Edge edge_id
        (Json.yojson_of_edge
           { edge=
               { source= Id.out source_id
               ; target= Id.out target_id
               ; edge_type
               ; edge_metadata
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


module Tito = struct
  (** TITO stands for "taint-in taint-out". In this context a tito argument is one that has a path
      to the return node, without going through call edges; more precisely, [i#foo#bar] is a tito
      argument if there is a path from [Argument (i, \[foo, bar\])] to [Return] not going through
      [ArgumentOf _] nodes. *)

  module FieldSet = struct
    include Caml.Set.Make (Fields)

    let pp = Fmt.iter iter Fields.pp
  end

  module IntMap = struct
    include Caml.Map.Make (Int)

    let pp pp_data =
      let sep = Fmt.any ":@ " in
      Fmt.iter_bindings ~sep:Fmt.comma iter Fmt.(pair ~sep int pp_data)
  end

  (** A [Tito.t] is a map from arguments indexes [i] to the set of [fields] field sequences such
      that [i#fields] is a Tito argument. Note: for any sequence of [fields], if [i#fields] is a
      Tito argument, then every [i#fields#foo] subfield of should be considered as also being one
      (even if not explicitly present in the map). *)
  type t = FieldSet.t IntMap.t
  (* Note: making this an array could improve the performance (arguments indexes are small and
     contiguous). *)

  let pp = IntMap.pp FieldSet.pp

  let empty = IntMap.empty

  let get i arguments = IntMap.find_opt i arguments |> Option.value ~default:FieldSet.empty

  let add i fields arguments =
    IntMap.update i
      (function
        | None ->
            Some (FieldSet.singleton fields)
        | Some fieldset ->
            Some (FieldSet.add fields fieldset) )
      arguments


  (** Returns a TITO map holding every field of every parameter of a function having the given arity *)
  let all_arguments arity =
    IntMap.of_seq @@ Seq.init arity (fun arg_index -> (arg_index, FieldSet.singleton []))
end

module Summary = struct
  type tito_arguments = Tito.t

  type t = {graph: LineageGraph.t; tito_arguments: tito_arguments; has_unsupported_features: bool}

  let pp_tito_arguments fmt arguments =
    Format.fprintf fmt "@;@[<2>TitoArguments@;%a@]" Tito.pp arguments


  let pp fmt {graph; tito_arguments} =
    Format.fprintf fmt "@;@[<2>LineageSummary@;%a%a@]" pp_tito_arguments tito_arguments
      LineageGraph.pp graph


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
      List.fold ~init:LineageGraph.Vertex.Map.empty ~f:add_flow graph
    in
    (* Do a DFS, to see which arguments are reachable from return. *)
    let rec dfs seen node =
      if Set.mem seen node then seen
      else
        let seen = Set.add seen node in
        let parents = Map.find_multi graph_rev node in
        List.fold ~init:seen ~f:dfs parents
    in
    let reachable = dfs LineageGraph.Vertex.Set.empty LineageGraph.Return in
    (* Collect reachable arguments. *)
    let tito_arguments =
      let add_node args (node : LineageGraph.data) =
        match node with Argument (i, fields) -> Tito.add i fields args | _ -> args
      in
      Set.fold reachable ~init:Tito.empty ~f:add_node
    in
    tito_arguments


  (** Reduces the size of the graph by possibly some data and some flows. Consider the following
      pattern: A -1-> B -2-> C. (Here, A, B and C are data; 1 and 2 are flows.) The basic trick is
      to transform such patterns into A -1-> C when: (i) B is "uninteresting" and (ii) 2 is
      "uninteresting". Data is uninteresting when it corresponds to a temporary variable introduced
      by the frontend; Flows are uninteresting when they are of kind [Direct].

      The basic trick is slightly generalized below: B may have 1 flow on one side, but 0..oo on the
      other side. This allows considerably more reduction in size.

      Note0: Because 0 is allowed in the "general" trick, (a) it is possible that interesting data
      is removed from the graph (but this happens only for data that is not connected in lineage to
      other interesting data); and (b) it is possible that the number of data nodes is reduced more
      than the number of flows (otherwise, each transform step would reduce both the number of data
      nodes and the number of flows by exactly 1, so the reduction would be equal to the number of
      transform steps applied).

      Note1: The implementation assumes no loops; otherwise, it may not terminate. (This is easy to
      change, but there should be no loops in lineage anyway.)

      Note2: The algorithm guarantees that the number of flows does not increase, and that the
      number of data nodes does not increase. *)
  let remove_temporaries proc_desc graph =
    (* Build adjacency list representation, to make DFS easy. *)
    let parents, children =
      let add_flow (parents, children) ({LineageGraph.source; target} as flow) =
        (Map.add_multi ~key:target ~data:flow parents, Map.add_multi ~key:source ~data:flow children)
      in
      List.fold ~init:LineageGraph.Vertex.Map.(empty, empty) ~f:add_flow graph
    in
    (* A check for vertex interestingness, used in the graph simplification that follows. *)
    let special_variables =
      let formals = get_formals proc_desc in
      let ret_var = Var.of_pvar (Procdesc.get_ret_var proc_desc) in
      Var.Set.of_list (ret_var :: formals)
    in
    let is_interesting_data (data : LineageGraph.data) =
      match data with
      | Local (VariableIndex var_idx, _node) ->
          VariableIndex.var_appears_in_source_code var_idx
          && not (Var.Set.mem (VariableIndex.get_var var_idx) special_variables)
      | Argument _ | Captured _ | Return | CapturedBy _ | ArgumentOf _ | ReturnOf _ ->
          true
      | Local (ConstantAtom _, _) | Local (ConstantInt _, _) | Local (ConstantString _, _) ->
          true
      | Function _ | Self ->
          true
    in
    let is_interesting_flow ({LineageGraph.kind} : LineageGraph.flow) =
      match kind with
      | Direct ->
          false
      | Call _
      | Return _
      | Capture
      | Summary
      | DynamicCallFunction
      | DynamicCallModule
      | Inject _
      | Project _ ->
          true
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
      LineageGraph.Vertex.Set.of_list
        (List.concat_map ~f:(function {LineageGraph.source; target} -> [source; target]) graph)
    in
    let todo = Set.filter ~f:(Fn.non is_interesting_data) todo in
    let remove_flow (parents, children) ({LineageGraph.source; target} as flow) =
      let rm map key =
        let flow_list = Map.find_multi map key in
        let flow_list =
          List.filter ~f:(fun elem -> not (LineageGraph.equal_flow elem flow)) flow_list
        in
        Map.set map ~key ~data:flow_list
      in
      (rm parents target, rm children source)
    in
    let rec simplify todo parents children =
      match Set.choose todo with
      | None ->
          (parents, children)
      | Some data ->
          if is_interesting_data data then L.die InternalError "Shouldn't happen" ;
          let todo = Set.remove todo data in
          let before = Map.find_multi parents data in
          let after = Map.find_multi children data in
          let before_interesting = List.exists ~f:is_interesting_flow before in
          let after_interesting = List.exists ~f:is_interesting_flow after in
          let short list = match list with [] | [_] -> true | _ -> false in
          let before_short = short before in
          let after_short = short after in
          if (before_short || after_short) && not (before_interesting && after_interesting) then
            let pairs = List.cartesian_product before after in
            (* (A) remove old edges *)
            let parents, children = List.fold ~init:(parents, children) ~f:remove_flow before in
            let parents, children = List.fold ~init:(parents, children) ~f:remove_flow after in
            let do_pair (todo, (parents, children))
                ((flow_ab : LineageGraph.flow), (flow_bc : LineageGraph.flow)) =
              let keep = if is_interesting_flow flow_bc then flow_bc else flow_ab in
              let keep : LineageGraph.flow =
                {keep with LineageGraph.source= flow_ab.source; target= flow_bc.target}
              in
              if LineageGraph.equal_data keep.source keep.target then
                L.die InternalError "OOPS: I don't work with loops." ;
              (* (B) add new edges *)
              let parents = Map.add_multi ~key:keep.target ~data:keep parents in
              let children = Map.add_multi ~key:keep.source ~data:keep children in
              (* The following two lines insert at most 1 vertex into [todo] for each [flow_ab]/
               * [flow_bc] edge that gets removed from the graph, in step (A) above. This is where
               * the earlier claim that at most m insertions happen in the main loop. However,
               * this claim is invalidated if one of the [flow_ab]/[flow_bc] edges gets re-added
               * to the graph in step (B) above, which may happen if there is an (isolated) loop
               * in the graph. *)
              let todo =
                if is_interesting_data keep.source then todo else Set.add todo keep.source
              in
              let todo =
                if is_interesting_data keep.target then todo else Set.add todo keep.target
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
    module LastWrites = AbstractDomain.FiniteMultiMap (VariableIndex.Terminal) (PPNode)
    module UnsupportedFeatures = AbstractDomain.BooleanOr
    include AbstractDomain.Pair (LastWrites) (UnsupportedFeatures)
  end

  module Unit = PartialSummary
  include AbstractDomain.Pair (Real) (Unit)
end

module TransferFunctions = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = SimpleShape.Summary.t * Summary.t InterproceduralAnalysis.t

  module Local = struct
    module T = struct
      type t = LineageGraph.local [@@deriving compare, equal, sexp]
    end

    include T

    (** Make [LineageGraph.local] usable in Maps/Sets. *)
    include Comparable.Make (T)

    module Set = struct
      include Set

      (** Shortcut for adding a VariableIndex-variant local *)
      let add_variable_index set index = add set (VariableIndex index)
    end
  end

  (** If an expression is made of a single variable, return it *)
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


  (** If an expression is made of a single variable index, return it *)
  let exp_as_single_var_index (e : Exp.t) : _ VariableIndex.t option =
    let rec aux fields_acc = function
      | Exp.Lvar pvar ->
          Some (VariableIndex.make (Var.of_pvar pvar) fields_acc)
      | Exp.Var id ->
          Some (VariableIndex.make (Var.of_id id) fields_acc)
      | Exp.Lfield (e, fieldname, _) ->
          aux (fieldname :: fields_acc) e
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


  (** Return the terminal free indices that can be derived from an index *)
  let terminal_free_locals_from_index shapes index =
    VariableIndex.fold_terminal shapes index ~f:Local.Set.add_variable_index ~init:Local.Set.empty


  (** Return constants and free terminal indices that occur in [e]. *)
  let rec free_locals_of_exp shapes (e : Exp.t) : Local.Set.t =
    match e with
    | Lvar pvar ->
        terminal_free_locals_from_index shapes (VariableIndex.pvar pvar)
    | Var id ->
        terminal_free_locals_from_index shapes (VariableIndex.ident id)
    | Lfield _ -> (
      (* We only allow (sequences of) fields to be "applied" to a single variable, yielding a single index  *)
      match exp_as_single_var_index e with
      | None ->
          (* Debugging hint: if you run into this assertion, the easiest is likely to change the
             frontend to introduce a temporary variable. *)
          L.die InternalError "I don't support fields sequences from non-variables expressions"
      | Some index ->
          terminal_free_locals_from_index shapes index )
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
          Local.Set.union locals (terminal_free_locals_from_index shapes (VariableIndex.pvar pvar)) )
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


  let sources_of_local here last_writes (local : Local.t) : LineageGraph.data list =
    match local with
    | ConstantAtom _ | ConstantInt _ | ConstantString _ ->
        [LineageGraph.Local (local, here)]
    | VariableIndex var_idx ->
        let source_nodes = Domain.Real.LastWrites.get_all var_idx last_writes in
        List.map ~f:(fun node -> LineageGraph.Local (local, node)) source_nodes


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
      let one_var index ((last_writes, has_unsupported_features), local_edges)
          (_exp, pvar, _typ, _mode) =
        let source_list =
          VariableIndex.concat_map_terminal shapes (VariableIndex.pvar pvar) ~f:(fun index ->
              sources_of_local node last_writes (VariableIndex index) )
        in
        let add_one_flow local_edges source =
          LineageGraph.add_flow ~kind:Direct ~node ~source
            ~target:(CapturedBy (index, name))
            local_edges
        in
        let local_edges = List.fold source_list ~f:add_one_flow ~init:local_edges in
        ((last_writes, has_unsupported_features), local_edges)
      in
      List.foldi ~init:astate ~f:one_var captured_vars
    in
    List.fold ~init:astate ~f:one_exp (Sil.exps_of_instr instr)


  let warn_on_complex_arg arg_exp =
    L.debug Analysis Verbose
      "SimpleLineage: the analysis assumes that the frontend uses only single-var expressions as \
       actual arguments, otherwise it will lose precision (found actual argument `%a` instead).@;"
      Exp.pp arg_exp


  (* Add Call flow from the concrete arguments to the special ArgumentOf nodes *)
  let add_arg_flows shapes (call_node : PPNode.t) (callee_pname : Procname.t)
      (argument_list : Exp.t list) (astate : Domain.t) : Domain.t =
    let add_one_arg_flows index ((last_writes, has_unsupported_features), local_edges) arg =
      let add_one_source_edge local_edges source ~injected_arg_field =
        LineageGraph.add_flow ~kind:(Call injected_arg_field) ~node:call_node ~source
          ~target:(ArgumentOf (index, callee_pname))
          local_edges
      in
      let add_one_local_flows local_edges (local : Local.t) ~injected_arg_field =
        let sources = sources_of_local call_node last_writes local in
        List.fold sources ~f:(add_one_source_edge ~injected_arg_field) ~init:local_edges
      in
      let local_edges =
        match exp_as_single_var arg with
        | None ->
            (* The Erlang frontend probably doesn't generate non-single-var arguments. If that
               happens however, we may simply collect all the locals from the actual argument and
               have them flow on the full formal parameter (that is inject into the empty list of
               nested fields) *)
            warn_on_complex_arg arg ;
            let read_set = free_locals_of_exp shapes arg in
            Set.fold read_set ~init:local_edges ~f:(add_one_local_flows ~injected_arg_field:[])
        | Some arg_var ->
            (* The concrete argument is a single var: we collect all its terminal fields and have
               them flow onto the corresponding field of the formal parameter. *)
            let arg_as_index = VariableIndex.var arg_var in
            VariableIndex.fold_terminal shapes arg_as_index
              ~f:(fun acc arg_terminal ->
                add_one_local_flows acc (VariableIndex arg_terminal)
                  ~injected_arg_field:(VariableIndex.get_fields arg_terminal) )
              ~init:local_edges
      in
      ((last_writes, has_unsupported_features), local_edges)
    in
    List.foldi argument_list ~init:astate ~f:add_one_arg_flows


  (* Add Return flow from the special ReturnOf nodes to the destination variable of a call *)
  let add_ret_flows shapes node (callee_pname : Procname.t) (ret_id : Ident.t) (astate : Domain.t) :
      Domain.t =
    let last_writes, local_edges = astate in
    let add_one_return_edge local_edges ret_terminal_index =
      (* Add flow from the formal return to a terminal subfield of the actual ret_id: project the
         return into the corresponding fields. *)
      let target_field = VariableIndex.get_fields ret_terminal_index in
      LineageGraph.add_flow ~kind:(Return target_field) ~node ~source:(ReturnOf callee_pname)
        ~target:(Local (VariableIndex ret_terminal_index, node))
        local_edges
    in
    let local_edges =
      VariableIndex.fold_terminal ~f:add_one_return_edge ~init:local_edges shapes
        (VariableIndex.ident ret_id)
    in
    (last_writes, local_edges)


  let add_lambda_edges shapes node write_var_index lambdas (real_state, local_edges) =
    let add_one_lambda one_lambda local_edges =
      VariableIndex.fold_terminal
        ~f:(fun acc target ->
          LineageGraph.add_flow ~kind:Direct ~node ~source:(Function one_lambda)
            ~target:(Local (VariableIndex target, node))
            acc )
        ~init:local_edges shapes write_var_index
    in
    let local_edges = Procname.Set.fold add_one_lambda lambdas local_edges in
    (real_state, local_edges)


  (* Add a write for a single variable index -- this should usually not be used directly as it will
     not take any shapes into account. *)
  let update_one_write node kind (write_var : VariableIndex.Terminal.t) (read_set : Local.Set.t)
      (((last_writes, has_unsupported_features), local_edges) : Domain.t) : Domain.t =
    let target = LineageGraph.Local (VariableIndex write_var, node) in
    let add_read local_edges (read_local : Local.t) =
      let sources = sources_of_local node last_writes read_local in
      let add_edge local_edges source =
        LineageGraph.add_flow ~node ~kind ~source ~target local_edges
      in
      let local_edges = List.fold ~init:local_edges ~f:add_edge sources in
      local_edges
    in
    let local_edges = Set.fold read_set ~init:local_edges ~f:add_read in
    let last_writes = Domain.Real.LastWrites.remove_all write_var last_writes in
    let last_writes = Domain.Real.LastWrites.add write_var node last_writes in
    ((last_writes, has_unsupported_features), local_edges)


  (* Update all the terminal fields of an index, as obtained from the shapes information. *)
  let update_write shapes node kind dst_index read_set astate =
    VariableIndex.fold_terminal
      ~f:(fun acc_astate terminal -> update_one_write node kind terminal read_set acc_astate)
      ~init:astate shapes dst_index


  (* Update all the terminal fields of an destination index, as obtained from the shapes information,
     as being written in parallel from the corresponding terminal fields of a source index. *)
  let update_write_parallel shapes node kind dst_index src_index astate =
    VariableIndex.fold_terminal_pairs
      ~f:(fun acc_astate dst_terminal src_terminal ->
        update_one_write node kind dst_terminal
          (Local.Set.singleton (VariableIndex src_terminal))
          acc_astate )
      ~init:astate shapes dst_index src_index


  let exec_assignment shapes node dst_index src_exp astate =
    match exp_as_single_var_index src_exp with
    | Some src_index ->
        (* Simple assignment of the form [dst := src_index]: we copy the fields of [src_index] into
           the corresponding fields of [dst]. *)
        update_write_parallel shapes node Direct dst_index src_index astate
    | None ->
        (* Complex assignment of any other form: we copy every terminal field from the source to
           (every terminal field of) the destination, and also process the potential captured indices
           and lambdas. *)
        let {free_locals; captured_locals; lambdas} = read_set_of_exp shapes src_exp in
        astate
        |> update_write shapes node LineageGraph.FlowKind.Direct dst_index free_locals
        |> update_write shapes node LineageGraph.FlowKind.Capture dst_index captured_locals
        |> add_lambda_edges shapes node dst_index lambdas


  (* Add Summary (or Direct if this is a suppressed builtin call) edges from the concrete parameters
     to the concrete destination variable of a call, as specified by the tito_arguments summary information *)
  let add_tito (shapes : SimpleShape.Summary.t) node (kind : LineageGraph.FlowKind.t)
      (tito_arguments : Tito.t) (argument_list : Exp.t list) (ret_id : Ident.t) (astate : Domain.t)
      : Domain.t =
    let collect_one_index var_index tito_subfields acc =
      VariableIndex.fold_terminal shapes
        (VariableIndex.subfield var_index tito_subfields)
        ~f:Local.Set.add_variable_index ~init:acc
    in
    let tito_locals =
      List.foldi ~init:Local.Set.empty
        ~f:(fun index acc arg ->
          match exp_as_single_var_index arg with
          | None ->
              (* The Erlang frontend probably doesn't generate arguments that are not put in
                 intermediate variables first, but in any case we can just add edges from all
                 occurring locals and soundly not use the TITO information. *)
              warn_on_complex_arg arg ;
              Local.Set.union acc (free_locals_of_exp shapes arg)
          | Some var_index ->
              Tito.FieldSet.fold (collect_one_index var_index) (Tito.get index tito_arguments) acc
          )
        argument_list
    in
    update_write shapes node kind (VariableIndex.ident ret_id) tito_locals astate


  (* Add all the possible Summary/Direct (see add_tito) call edges from arguments to destination for
     when no summary is available. *)
  let add_tito_all (shapes : SimpleShape.Summary.t) node (kind : LineageGraph.FlowKind.t)
      (argument_list : Exp.t list) (ret_id : Ident.t) (astate : Domain.t) : Domain.t =
    let arity = List.length argument_list in
    let tito_all_arguments = Tito.all_arguments arity in
    add_tito shapes node kind tito_all_arguments argument_list ret_id astate


  (* Add the relevant Summary/Direct call edges from concrete arguments to the destination, depending
     on the presence of a summary. *)
  let add_summary_flows shapes node (kind : LineageGraph.FlowKind.t) (callee : Summary.t option)
      (argument_list : Exp.t list) (ret_id : Ident.t) (astate : Domain.t) : Domain.t =
    match callee with
    | None ->
        add_tito_all shapes node kind argument_list ret_id astate
    | Some {Summary.tito_arguments} ->
        add_tito shapes node kind tito_arguments argument_list ret_id astate


  let record_supported name (((last_writes, has_unsupported_features), local_edges) : Domain.t) :
      Domain.t =
    let has_unsupported_features =
      has_unsupported_features || Procname.is_erlang_unsupported name
    in
    ((last_writes, has_unsupported_features), local_edges)


  let generic_call_model shapes node analyze_dependency ret_id procname args astate =
    let rm_builtin =
      (not Config.simple_lineage_include_builtins) && BuiltinDecl.is_declared procname
    in
    let if_not_builtin transform state = if rm_builtin then state else transform state in
    let summary_type : LineageGraph.FlowKind.t = if rm_builtin then Direct else Summary in
    astate |> record_supported procname
    |> if_not_builtin (add_arg_flows shapes node procname args)
    |> if_not_builtin (add_ret_flows shapes node procname ret_id)
    |> add_summary_flows shapes node summary_type (analyze_dependency procname) args ret_id


  module CustomModel = struct
    let call_unqualified shapes node analyze_dependency ret_id procname args astate =
      match args with
      | fun_ :: _ ->
          astate
          |> generic_call_model shapes node analyze_dependency ret_id procname args
          |> update_write shapes node DynamicCallFunction (VariableIndex.ident ret_id)
               (free_locals_of_exp shapes fun_)
      | _ ->
          L.die InternalError "Expecting at least one argument for '__erlang_call_unqualified'"


    let call_qualified shapes node analyze_dependency ret_id procname args astate =
      match args with
      | module_ :: fun_ :: _ ->
          astate
          |> generic_call_model shapes node analyze_dependency ret_id procname args
          |> update_write shapes node DynamicCallFunction (VariableIndex.ident ret_id)
               (free_locals_of_exp shapes fun_)
          |> update_write shapes node DynamicCallModule (VariableIndex.ident ret_id)
               (free_locals_of_exp shapes module_)
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
      let sources = Local.Set.singleton (ConstantAtom atom_name) in
      update_write shapes node LineageGraph.FlowKind.Direct (VariableIndex.ident ret_id) sources
        astate


    let make_tuple shapes node _analyze_dependency ret_id _procname (args : Exp.t list) astate =
      let size = List.length args in
      let tuple_type = ErlangTypeName.Tuple size in
      let field_names = ErlangTypeName.tuple_field_names size in
      let fieldname name = Fieldname.make (ErlangType tuple_type) name in
      let ret_field name = VariableIndex.make (Var.of_id ret_id) [fieldname name] in
      List.fold2_exn
        ~f:(fun astate field_name arg ->
          exec_assignment shapes node (ret_field field_name) arg astate )
        ~init:astate field_names args


    let custom_call_models =
      let apply arity =
        Procname.make_erlang ~module_name:ErlangTypeName.erlang_namespace ~function_name:"apply"
          ~arity
      in
      let pairs =
        [ (BuiltinDecl.__erlang_make_atom, make_atom)
        ; (BuiltinDecl.__erlang_make_tuple, make_tuple)
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
      L.die InternalError "SimpleLineage: INV broken: CFGs should be single instruction@\n" ;
    let astate = add_cap_flows shapes node instr (fst astate, [ (* don't repeat edges*) ]) in
    match instr with
    | Load {id; e; _} ->
        if Ident.is_none id then astate
        else exec_assignment shapes node (VariableIndex.ident id) e astate
    | Store {e1= Lvar lhs; e2; _} ->
        exec_assignment shapes node (VariableIndex.pvar lhs) e2 astate
    | Store _ ->
        L.debug Analysis Verbose
          "SimpleLineage: The only lhs I can handle (now) for Store is Lvar@\n" ;
        astate
    | Call ((ret_id, _ret_typ), name, args, _location, _flags) ->
        exec_call shapes node analyze_dependency ret_id name args astate
    | Sil.Prune (_, _, _, _) | Sil.Metadata _ ->
        astate


  let pp_session_name _node fmt = Format.pp_print_string fmt "SimpleLineage"
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

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
  let initial =
    let formals = get_formals proc_desc in
    let captured = get_captured proc_desc in
    let start_node = CFG.start_node cfg in
    let add_arg_index last_writes arg_index =
      Domain.Real.LastWrites.add arg_index start_node last_writes
    in
    let add_arg last_writes var =
      VariableIndex.fold_terminal ~f:add_arg_index ~init:last_writes shapes (VariableIndex.var var)
    in
    let last_writes =
      List.fold ~init:Domain.Real.LastWrites.bottom ~f:add_arg (captured @ formals)
    in
    let local_edges =
      let add_proc_start_flow ~source ~kind local_edges var_index =
        LineageGraph.add_flow ~kind ~node:start_node ~source
          ~target:(Local (VariableIndex var_index, start_node))
          local_edges
      in
      let arg_edge_kind arg_fields : LineageGraph.FlowKind.t =
        (* Record that edges from fields of a formal parameter will represent a Projection of those
           fields from the eventual summarised Argument node *)
        match arg_fields with [] -> Direct | subfields -> Project subfields
      in
      let add_arg_flow i local_edges arg_var =
        VariableIndex.fold_terminal
          ~f:(fun acc idx ->
            let fields = VariableIndex.get_fields idx in
            add_proc_start_flow ~kind:(arg_edge_kind fields) ~source:(Argument (i, fields)) acc idx
            )
          ~init:local_edges shapes (VariableIndex.var arg_var)
      in
      let add_cap_flow i local_edges var =
        VariableIndex.fold_terminal
          ~f:(add_proc_start_flow ~kind:Direct ~source:(Captured i))
          ~init:local_edges shapes (VariableIndex.var var)
      in
      let local_edges = [] in
      let local_edges = List.foldi ~init:local_edges ~f:add_arg_flow formals in
      let local_edges = List.foldi ~init:local_edges ~f:add_cap_flow captured in
      local_edges
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
    let ret_var = VariableIndex.pvar (Procdesc.get_ret_var proc_desc) in
    let ret_edge_kind into_ret_index : LineageGraph.FlowKind.t =
      (* Record that edges into fields of the formal return will represent an Injection of those
         fields into the eventual summarised Return node *)
      match VariableIndex.get_fields into_ret_index with
      | [] ->
          Direct
      | subfields ->
          Inject subfields
    in
    let add_one_ret_index_edge local_edges ret_index ret_node =
      LineageGraph.add_flow ~kind:(ret_edge_kind ret_index) ~node:ret_node
        ~source:(Local (VariableIndex ret_index, ret_node))
        ~target:Return local_edges
    in
    let add_ret_index_edges local_edges ret_index =
      List.fold ~init:local_edges
        ~f:(fun acc source -> add_one_ret_index_edge acc ret_index source)
        (Domain.Real.LastWrites.get_all ret_index exit_last_writes)
    in
    let graph = Analyzer.InvariantMap.fold collect invmap [snd initial] |> List.concat in
    let graph = VariableIndex.fold_terminal ~init:graph ~f:add_ret_index_edges shapes ret_var in
    graph
  in
  let summary = Summary.make exit_has_unsupported_features proc_desc graph in
  if Config.simple_lineage_json_report then Summary.report summary proc_desc ;
  Some summary


let checker = SimpleLineageUtils.skip_unwanted unskipped_checker
