(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module ConfigName = FbGKInteraction.ConfigName

module Branch = struct
  type t = True | False | Top

  let pp f = function
    | True ->
        F.pp_print_string f "true branch"
    | False ->
        F.pp_print_string f "false branch"
    | Top ->
        AbstractDomain.TopLiftedUtils.pp_top f


  let leq ~lhs ~rhs =
    match (lhs, rhs) with True, True | False, False | _, Top -> true | _, _ -> false


  let join x y = match (x, y) with True, True -> True | False, False -> False | _, _ -> Top

  let widen ~prev ~next ~num_iters:_ = join prev next

  let top = Top

  let is_top = function Top -> true | True | False -> false

  let neg = function True -> False | False -> True | Top -> Top
end

module ConfigChecks = AbstractDomain.SafeInvertedMap (ConfigName) (Branch)

module Field = struct
  include Fieldname

  let compare = Fieldname.compare_name
end

module Fields = AbstractDomain.FiniteSet (Field)

module FieldChecks = struct
  include AbstractDomain.SafeInvertedMap (Field) (Branch)

  let get_fields field_map =
    fold
      (fun field branch acc ->
        assert (not (Branch.is_top branch)) ;
        Fields.add field acc )
      field_map Fields.empty
end

module UncheckedCallee = struct
  type callee_name = Procname.t

  let compare_callee_name = Procname.compare_name

  type t =
    { callee: callee_name
    ; is_known_expensive: bool
    ; location: Location.t [@compare.ignore]
    ; call_type: call_type [@compare.ignore] }
  [@@deriving compare]

  and call_type = Direct | Indirect of t

  let pp_common ~with_location f {callee; location; call_type} =
    F.fprintf f "%a is %scalled" Procname.pp callee
      (match call_type with Direct -> "" | Indirect _ -> "indirectly ") ;
    if with_location then F.fprintf f " at %a" Location.pp location


  let pp f x = pp_common ~with_location:true f x

  let pp_without_location f x = pp_common ~with_location:false f x

  let pp_without_location_list f unchecked_callees =
    IList.pp_print_list ~max:Config.config_impact_max_callees_to_print
      ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
      (fun f {callee} -> Procname.pp f callee)
      f unchecked_callees


  let make ~is_known_expensive callee location =
    {callee; is_known_expensive; location; call_type= Direct}


  let is_known_expensive {is_known_expensive} = is_known_expensive

  let replace_location_by_call location x = {x with location; call_type= Indirect x}

  let rec make_err_trace ({location} as x) =
    let desc = F.asprintf "%a" pp_without_location x in
    let trace_elem = Errlog.make_trace_element 0 location desc [] in
    match x.call_type with Direct -> [trace_elem] | Indirect x -> trace_elem :: make_err_trace x
end

module UncheckedCallees = struct
  include AbstractDomain.FiniteSet (UncheckedCallee)

  let replace_location_by_call location x =
    map (UncheckedCallee.replace_location_by_call location) x


  let encode astate = Marshal.to_string astate [] |> Base64.encode_exn

  let decode enc_str = Marshal.from_string (Base64.decode_exn enc_str) 0

  let pp_without_location f x = UncheckedCallee.pp_without_location_list f (elements x)

  let has_known_expensive_callee x = exists (fun {is_known_expensive} -> is_known_expensive) x

  let filter_known_expensive x = filter (fun {is_known_expensive} -> is_known_expensive) x
end

module UncheckedCalleesCond = struct
  include AbstractDomain.Map (Fields) (UncheckedCallees)

  let weak_update fields callees fields_map =
    update fields
      (function None -> Some callees | Some prev -> Some (UncheckedCallees.union prev callees))
      fields_map


  let replace_location_by_call location fields_map =
    map (UncheckedCallees.replace_location_by_call location) fields_map


  let filter_known_expensive fields_map =
    fold
      (fun fields callees acc ->
        let expensive_callees = UncheckedCallees.filter_known_expensive callees in
        if UncheckedCallees.is_empty expensive_callees then acc
        else add fields expensive_callees acc )
      fields_map empty
end

module Loc = struct
  type t = Ident of Ident.t | Pvar of Pvar.t [@@deriving compare]

  let pp f = function Ident id -> Ident.pp f id | Pvar pvar -> Pvar.pp Pp.text f pvar

  let of_id id = Ident id

  let of_pvar pvar = Pvar pvar
end

module ConfigLifted = AbstractDomain.Flat (ConfigName)

module Val = struct
  type t = {config: ConfigLifted.t; fields: Fields.t}

  let pp f {config; fields} =
    F.fprintf f "@[@[config:@,%a@]@ @[fields:@,%a@]@]" ConfigLifted.pp config Fields.pp fields


  let leq ~lhs ~rhs =
    ConfigLifted.leq ~lhs:lhs.config ~rhs:rhs.config && Fields.leq ~lhs:lhs.fields ~rhs:rhs.fields


  let join x y = {config= ConfigLifted.join x.config y.config; fields= Fields.join x.fields y.fields}

  let widen ~prev ~next ~num_iters =
    { config= ConfigLifted.widen ~prev:prev.config ~next:next.config ~num_iters
    ; fields= Fields.widen ~prev:prev.fields ~next:next.fields ~num_iters }


  let bottom = {config= ConfigLifted.bottom; fields= Fields.bottom}

  let of_config config = {bottom with config= ConfigLifted.v config}

  let of_field fn = {bottom with fields= Fields.singleton fn}
end

module Mem = struct
  include AbstractDomain.Map (Loc) (Val)

  let lookup loc mem = find_opt loc mem |> Option.value ~default:Val.bottom
end

module Summary = struct
  type t =
    { unchecked_callees: UncheckedCallees.t  (** Set of unchecked callees *)
    ; unchecked_callees_cond: UncheckedCalleesCond.t
          (** Sets of unchecked callees that are called conditionally by object fields *)
    ; has_call_stmt: bool  (** True if a function includes a call statement *)
    ; config_fields: Fields.t
          (** Intra-procedurally collected fields that may have config values *) }

  let pp f {unchecked_callees; unchecked_callees_cond; has_call_stmt; config_fields} =
    F.fprintf f
      "@[@[unchecked callees:@,\
       %a@],@ @[unchecked callees cond:@,\
       %a@],@ @[has_call_stmt:%b@],@ @[config fields:%a@]@]" UncheckedCallees.pp unchecked_callees
      UncheckedCalleesCond.pp unchecked_callees_cond has_call_stmt Fields.pp config_fields


  let get_config_fields {config_fields} = config_fields

  let get_unchecked_callees {unchecked_callees} = unchecked_callees

  let instantiate_unchecked_callees_cond ~all_config_fields
      ({unchecked_callees; unchecked_callees_cond} as x) =
    let unchecked_callees =
      UncheckedCalleesCond.fold
        (fun fields callees acc ->
          if Fields.is_empty (Fields.inter all_config_fields fields) then
            UncheckedCallees.union acc callees
          else acc )
        unchecked_callees_cond unchecked_callees
    in
    {x with unchecked_callees; unchecked_callees_cond= UncheckedCalleesCond.bottom}


  let has_known_expensive_callee {unchecked_callees} =
    UncheckedCallees.has_known_expensive_callee unchecked_callees
end

module Dom = struct
  type t =
    { config_checks: ConfigChecks.t
    ; field_checks: FieldChecks.t
    ; unchecked_callees: UncheckedCallees.t
    ; unchecked_callees_cond: UncheckedCalleesCond.t
    ; mem: Mem.t
    ; config_fields: Fields.t }

  let pp f
      {config_checks; field_checks; unchecked_callees; unchecked_callees_cond; mem; config_fields} =
    F.fprintf f
      "@[@[config checks:@,\
       %a@]@ @[field checks:@,\
       %a@]@ @[unchecked callees:@,\
       %a@]@ @[unchecked callees cond:@,\
       %a@]@ @[mem:@,\
       %a@]@ @[config fields:@,\
       %a@]@]"
      ConfigChecks.pp config_checks FieldChecks.pp field_checks UncheckedCallees.pp
      unchecked_callees UncheckedCalleesCond.pp unchecked_callees_cond Mem.pp mem Fields.pp
      config_fields


  let leq ~lhs ~rhs =
    ConfigChecks.leq ~lhs:lhs.config_checks ~rhs:rhs.config_checks
    && FieldChecks.leq ~lhs:lhs.field_checks ~rhs:rhs.field_checks
    && UncheckedCallees.leq ~lhs:lhs.unchecked_callees ~rhs:rhs.unchecked_callees
    && UncheckedCalleesCond.leq ~lhs:lhs.unchecked_callees_cond ~rhs:rhs.unchecked_callees_cond
    && Mem.leq ~lhs:lhs.mem ~rhs:rhs.mem
    && Fields.leq ~lhs:lhs.config_fields ~rhs:rhs.config_fields


  let join x y =
    { config_checks= ConfigChecks.join x.config_checks y.config_checks
    ; field_checks= FieldChecks.join x.field_checks y.field_checks
    ; unchecked_callees= UncheckedCallees.join x.unchecked_callees y.unchecked_callees
    ; unchecked_callees_cond=
        UncheckedCalleesCond.join x.unchecked_callees_cond y.unchecked_callees_cond
    ; mem= Mem.join x.mem y.mem
    ; config_fields= Fields.join x.config_fields y.config_fields }


  let widen ~prev ~next ~num_iters =
    { config_checks= ConfigChecks.widen ~prev:prev.config_checks ~next:next.config_checks ~num_iters
    ; field_checks= FieldChecks.widen ~prev:prev.field_checks ~next:next.field_checks ~num_iters
    ; unchecked_callees=
        UncheckedCallees.widen ~prev:prev.unchecked_callees ~next:next.unchecked_callees ~num_iters
    ; unchecked_callees_cond=
        UncheckedCalleesCond.widen ~prev:prev.unchecked_callees_cond
          ~next:next.unchecked_callees_cond ~num_iters
    ; mem= Mem.widen ~prev:prev.mem ~next:next.mem ~num_iters
    ; config_fields= Fields.widen ~prev:prev.config_fields ~next:next.config_fields ~num_iters }


  let to_summary ~has_call_stmt {unchecked_callees; unchecked_callees_cond; config_fields} =
    {Summary.unchecked_callees; unchecked_callees_cond; has_call_stmt; config_fields}


  let init =
    { config_checks= ConfigChecks.top
    ; field_checks= FieldChecks.top
    ; unchecked_callees= UncheckedCallees.bottom
    ; unchecked_callees_cond= UncheckedCalleesCond.bottom
    ; mem= Mem.bottom
    ; config_fields= Fields.bottom }


  let add_mem loc v ({mem} as astate) = {astate with mem= Mem.add loc v mem}

  let copy_mem ~tgt ~src ({mem} as astate) = add_mem tgt (Mem.lookup src mem) astate

  let call_config_check ret config astate = add_mem (Loc.of_id ret) (Val.of_config config) astate

  let load_config id pvar astate = copy_mem ~tgt:(Loc.of_id id) ~src:(Loc.of_pvar pvar) astate

  let load_field id fn astate = add_mem (Loc.of_id id) (Val.of_field fn) astate

  let store_config pvar id astate = copy_mem ~tgt:(Loc.of_pvar pvar) ~src:(Loc.of_id id) astate

  let store_field fn id ({mem; config_fields} as astate) =
    let {Val.config} = Mem.lookup (Loc.of_id id) mem in
    if ConfigLifted.is_bottom config then astate
    else {astate with config_fields= Fields.add fn config_fields}


  let boolean_value id_tgt id_src astate =
    copy_mem ~tgt:(Loc.of_id id_tgt) ~src:(Loc.of_id id_src) astate


  let neg_branch res = Option.map ~f:(fun (config, branch) -> (config, Branch.neg branch)) res

  let rec get_config_check_prune e mem =
    match (e : Exp.t) with
    | Var id -> (
        let {Val.config; fields} = Mem.lookup (Loc.of_id id) mem in
        match ConfigLifted.get config with
        | Some config ->
            Some (`Unconditional config, Branch.True)
        | None ->
            Option.some_if (not (Fields.is_empty fields)) (`Conditional fields, Branch.True) )
    | UnOp (LNot, e, _) ->
        get_config_check_prune e mem |> neg_branch
    | BinOp ((Eq | Ne), Const _, Const _) ->
        None
    | (BinOp (Eq, e, (Const _ as const)) | BinOp (Eq, (Const _ as const), e)) when Exp.is_zero const
      ->
        get_config_check_prune e mem |> neg_branch
    | (BinOp (Ne, e, (Const _ as const)) | BinOp (Ne, (Const _ as const), e)) when Exp.is_zero const
      ->
        get_config_check_prune e mem
    | _ ->
        None


  let prune e ({config_checks; field_checks; mem} as astate) =
    get_config_check_prune e mem
    |> Option.value_map ~default:astate ~f:(fun (config, branch) ->
           match config with
           | `Unconditional config ->
               {astate with config_checks= ConfigChecks.add config branch config_checks}
           | `Conditional fields ->
               { astate with
                 field_checks=
                   Fields.fold
                     (fun field acc -> FieldChecks.add field branch acc)
                     fields field_checks } )


  type known_expensiveness = KnownCheap | KnownExpensive

  let get_expensiveness_model =
    let dispatch : (Tenv.t, known_expensiveness, unit) ProcnameDispatcher.Call.dispatcher =
      let open ProcnameDispatcher.Call in
      make_dispatcher
        [ +BuiltinDecl.(match_builtin __cast) <>--> KnownCheap
        ; +PatternMatch.Java.implements_google "common.base.Preconditions"
          &:: "checkArgument" $ any_arg $+ any_arg $+...$--> KnownExpensive
        ; +PatternMatch.Java.implements_google "common.base.Preconditions"
          &:: "checkElementIndex" $ any_arg $+ any_arg $+ any_arg $+...$--> KnownExpensive
        ; +PatternMatch.Java.implements_google "common.base.Preconditions"
          &:: "checkNotNull" $ any_arg $+ any_arg $+...$--> KnownExpensive
        ; +PatternMatch.Java.implements_google "common.base.Preconditions"
          &:: "checkPositionIndex" $ any_arg $+ any_arg $+ any_arg $+...$--> KnownExpensive
        ; +PatternMatch.Java.implements_google "common.base.Preconditions"
          &:: "checkState" $ any_arg $+ any_arg $+...$--> KnownExpensive
        ; +PatternMatch.Java.implements_lang "String" &:: "concat" &--> KnownExpensive
        ; +PatternMatch.Java.implements_lang "StringBuilder" &:: "append" &--> KnownExpensive ]
    in
    fun tenv pname args ->
      let args =
        List.map args ~f:(fun (exp, typ) ->
            ProcnameDispatcher.Call.FuncArg.{exp; typ; arg_payload= ()} )
      in
      dispatch tenv pname args


  let call tenv analyze_dependency ~is_cheap_call callee args location
      ({config_checks; field_checks; unchecked_callees; unchecked_callees_cond} as astate) =
    let join_unchecked_callees new_unchecked_callees new_unchecked_callees_cond =
      if FieldChecks.is_top field_checks then
        { astate with
          unchecked_callees= UncheckedCallees.join unchecked_callees new_unchecked_callees
        ; unchecked_callees_cond=
            UncheckedCalleesCond.join unchecked_callees_cond new_unchecked_callees_cond }
      else
        let fields_to_add = FieldChecks.get_fields field_checks in
        let unchecked_callees_cond =
          UncheckedCalleesCond.weak_update fields_to_add new_unchecked_callees
            unchecked_callees_cond
        in
        let unchecked_callees_cond =
          UncheckedCalleesCond.fold
            (fun fields callees acc ->
              UncheckedCalleesCond.weak_update (Fields.union fields fields_to_add) callees acc )
            new_unchecked_callees_cond unchecked_callees_cond
        in
        {astate with unchecked_callees_cond}
    in
    if ConfigChecks.is_top config_checks then
      let (callee_summary : Summary.t option) =
        match analyze_dependency callee with
        | None ->
            None
        | Some (_, (_, analysis_data, _)) ->
            analysis_data
      in
      let expensiveness_model = get_expensiveness_model tenv callee args in
      let has_expensive_callee =
        Option.exists callee_summary ~f:Summary.has_known_expensive_callee
      in
      match expensiveness_model with
      | None when is_cheap_call && not has_expensive_callee ->
          (* If callee is cheap by heuristics, ignore it. *)
          astate
      | Some KnownCheap ->
          (* If callee is known cheap by model, ignore it. *)
          astate
      | Some KnownExpensive ->
          (* If callee is known expensive by model, add callee's name. *)
          join_unchecked_callees
            (UncheckedCallees.singleton
               (UncheckedCallee.make ~is_known_expensive:true callee location))
            UncheckedCalleesCond.empty
      | None -> (
        match callee_summary with
        | Some
            { Summary.unchecked_callees= callee_summary
            ; unchecked_callees_cond= callee_summary_cond
            ; has_call_stmt }
          when has_call_stmt ->
            let callee_summary, callee_summary_cond =
              if is_cheap_call then
                (* In this case, the callee is cheap by the heuristics, but its summary includes
                   some known expensive callees. Thus, it filters the known expensive callees
                   only. *)
                ( UncheckedCallees.filter_known_expensive callee_summary
                , UncheckedCalleesCond.filter_known_expensive callee_summary_cond )
              else (callee_summary, callee_summary_cond)
            in
            (* If callee's summary is not leaf, use it. *)
            join_unchecked_callees
              (UncheckedCallees.replace_location_by_call location callee_summary)
              (UncheckedCalleesCond.replace_location_by_call location callee_summary_cond)
        | None when Procname.is_objc_init callee ->
            (* If callee is unknown ObjC initializer, ignore it. *)
            astate
        | _ ->
            (* Otherwise, add callee's name. *)
            join_unchecked_callees
              (UncheckedCallees.singleton
                 (UncheckedCallee.make ~is_known_expensive:false callee location))
              UncheckedCalleesCond.empty )
    else astate
end

type analysis_data =
  { interproc:
      (BufferOverrunAnalysisSummary.t option * Summary.t option * CostDomain.summary option)
      InterproceduralAnalysis.t
  ; get_is_cheap_call: CostInstantiate.Call.t -> bool }

module TransferFunctions = struct
  module CFG = ProcCfg.Normal
  module Domain = Dom

  type nonrec analysis_data = analysis_data

  let is_java_boolean_value_method pname =
    Procname.get_class_name pname |> Option.exists ~f:(String.equal "java.lang.Boolean")
    && Procname.get_method pname |> String.equal "booleanValue"


  let is_cheap_system_method =
    let cheap_system_methods =
      String.Set.of_list
        [ "clearProperty"
        ; "console"
        ; "currentTimeMillis"
        ; "exit"
        ; "getEnv"
        ; "getLogger"
        ; "getProperties"
        ; "getProperty"
        ; "getSecurityManager"
        ; "identityHashCode​"
        ; "inheritedChannel"
        ; "lineSeparator"
        ; "mapLibraryName​"
        ; "nanoTime"
        ; "setErr"
        ; "setIn"
        ; "setOut"
        ; "setProperties"
        ; "setProperty"
        ; "setSecurityManager" ]
    in
    fun _ method_name -> String.Set.mem cheap_system_methods method_name


  let is_known_cheap_method =
    let dispatch : (Tenv.t, unit, unit) ProcnameDispatcher.ProcName.dispatcher =
      let open ProcnameDispatcher.ProcName in
      make_dispatcher
        [ +PatternMatch.Java.implements_collection &:: "size" <>--> ()
        ; +PatternMatch.Java.implements_math &::.*--> ()
        ; +PatternMatch.Java.implements_number &::.*--> ()
        ; +PatternMatch.Java.implements_system &::+ is_cheap_system_method &--> () ]
    in
    fun tenv pname -> dispatch tenv pname |> Option.is_some


  let exec_instr astate {interproc= {tenv; analyze_dependency}; get_is_cheap_call} node idx instr =
    match (instr : Sil.instr) with
    | Load {id; e= Lvar pvar} ->
        Dom.load_config id pvar astate
    | Load {id; e= Lfield (_, fn, _)} ->
        Dom.load_field id fn astate
    | Store {e1= Lvar pvar; e2= Var id} ->
        Dom.store_config pvar id astate
    | Store {e1= Lfield (_, fn, _); e2= Var id} ->
        Dom.store_field fn id astate
    | Call ((ret, _), Const (Cfun callee), [(Var id, _)], _, _)
      when is_java_boolean_value_method callee ->
        Dom.boolean_value ret id astate
    | Call (_, Const (Cfun callee), _, _, _) when is_known_cheap_method tenv callee ->
        astate
    | Call (((ret_id, _) as ret), Const (Cfun callee), args, location, _) -> (
      match FbGKInteraction.get_config_check tenv callee args with
      | Some (`Config config) ->
          Dom.call_config_check ret_id config astate
      | Some (`Exp _) ->
          astate
      | None ->
          (* normal function calls *)
          let call =
            CostInstantiate.Call.
              {loc= location; pname= callee; node= CFG.Node.to_instr idx node; args; ret}
          in
          let is_cheap_call = get_is_cheap_call call in
          Dom.call tenv analyze_dependency ~is_cheap_call callee args location astate )
    | Prune (e, _, _, _) ->
        Dom.prune e astate
    | _ ->
        astate


  let pp_session_name node fmt =
    F.fprintf fmt "Config impact function calls %a" CFG.Node.pp_id (CFG.Node.id node)
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let has_call_stmt proc_desc =
  let exception FoundCall in
  try
    Procdesc.iter_instrs (fun _node -> function Call _ -> raise FoundCall | _ -> ()) proc_desc ;
    false
  with FoundCall -> true


let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let get_is_cheap_call = CostInstantiate.get_is_cheap_call analysis_data in
  let analysis_data = {interproc= analysis_data; get_is_cheap_call} in
  Option.map (Analyzer.compute_post analysis_data ~initial:Dom.init proc_desc) ~f:(fun astate ->
      let has_call_stmt = has_call_stmt proc_desc in
      Dom.to_summary ~has_call_stmt astate )
