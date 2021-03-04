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

module UncheckedCallee = struct
  type t =
    { callee: Procname.t
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
end

module Loc = struct
  type t = Ident of Ident.t | Pvar of Pvar.t [@@deriving compare]

  let pp f = function Ident id -> Ident.pp f id | Pvar pvar -> Pvar.pp Pp.text f pvar

  let of_id id = Ident id

  let of_pvar pvar = Pvar pvar
end

module ConfigLifted = AbstractDomain.Flat (ConfigName)

module Val = struct
  type t = {config: ConfigLifted.t}

  let pp f {config} = F.fprintf f "@[config:@,%a@]" ConfigLifted.pp config

  let leq ~lhs ~rhs = ConfigLifted.leq ~lhs:lhs.config ~rhs:rhs.config

  let join x y = {config= ConfigLifted.join x.config y.config}

  let widen ~prev ~next ~num_iters =
    {config= ConfigLifted.widen ~prev:prev.config ~next:next.config ~num_iters}


  let bottom = {config= ConfigLifted.bottom}

  let of_config config = {config= ConfigLifted.v config}

  let get_config_opt {config} = ConfigLifted.get config
end

module Mem = struct
  include AbstractDomain.Map (Loc) (Val)

  let lookup loc mem = find_opt loc mem |> Option.value ~default:Val.bottom
end

module Summary = struct
  type t = {unchecked_callees: UncheckedCallees.t; has_call_stmt: bool}

  let pp f {unchecked_callees; has_call_stmt} =
    F.fprintf f "@[unchecked callees:@,%a,has_call_stmt:%b@]" UncheckedCallees.pp unchecked_callees
      has_call_stmt


  let get_unchecked_callees {unchecked_callees} = unchecked_callees
end

module Dom = struct
  type t = {config_checks: ConfigChecks.t; unchecked_callees: UncheckedCallees.t; mem: Mem.t}

  let pp f {config_checks; unchecked_callees; mem} =
    F.fprintf f "@[@[config checks:@,%a@]@ @[unchecked callees:@,%a@]@ @[mem:%,%a@]@]"
      ConfigChecks.pp config_checks UncheckedCallees.pp unchecked_callees Mem.pp mem


  let leq ~lhs ~rhs =
    ConfigChecks.leq ~lhs:lhs.config_checks ~rhs:rhs.config_checks
    && UncheckedCallees.leq ~lhs:lhs.unchecked_callees ~rhs:rhs.unchecked_callees
    && Mem.leq ~lhs:lhs.mem ~rhs:rhs.mem


  let join x y =
    { config_checks= ConfigChecks.join x.config_checks y.config_checks
    ; unchecked_callees= UncheckedCallees.join x.unchecked_callees y.unchecked_callees
    ; mem= Mem.join x.mem y.mem }


  let widen ~prev ~next ~num_iters =
    { config_checks= ConfigChecks.widen ~prev:prev.config_checks ~next:next.config_checks ~num_iters
    ; unchecked_callees=
        UncheckedCallees.widen ~prev:prev.unchecked_callees ~next:next.unchecked_callees ~num_iters
    ; mem= Mem.widen ~prev:prev.mem ~next:next.mem ~num_iters }


  let to_summary has_call_stmt {unchecked_callees} = {Summary.unchecked_callees; has_call_stmt}

  let init =
    {config_checks= ConfigChecks.top; unchecked_callees= UncheckedCallees.bottom; mem= Mem.bottom}


  let add_mem loc v ({mem} as astate) = {astate with mem= Mem.add loc v mem}

  let copy_mem ~tgt ~src ({mem} as astate) = add_mem tgt (Mem.lookup src mem) astate

  let call_config_check ret config astate = add_mem (Loc.of_id ret) (Val.of_config config) astate

  let load_config id pvar astate = copy_mem ~tgt:(Loc.of_id id) ~src:(Loc.of_pvar pvar) astate

  let store_config pvar id astate = copy_mem ~tgt:(Loc.of_pvar pvar) ~src:(Loc.of_id id) astate

  let boolean_value id_tgt id_src astate =
    copy_mem ~tgt:(Loc.of_id id_tgt) ~src:(Loc.of_id id_src) astate


  let neg_branch res = Option.map ~f:(fun (config, branch) -> (config, Branch.neg branch)) res

  let rec get_config_check_prune e mem =
    match (e : Exp.t) with
    | Var id ->
        Mem.lookup (Loc.of_id id) mem
        |> Val.get_config_opt
        |> Option.map ~f:(fun config -> (config, Branch.True))
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


  let prune e ({config_checks; mem} as astate) =
    get_config_check_prune e mem
    |> Option.value_map ~default:astate ~f:(fun (config, branch) ->
           {astate with config_checks= ConfigChecks.add config branch config_checks} )


  let call analyze_dependency callee location ({config_checks; unchecked_callees} as astate) =
    if ConfigChecks.is_top config_checks then
      let unchecked_callees =
        match analyze_dependency callee with
        | Some (_, {Summary.unchecked_callees= callee_summary; has_call_stmt}) when has_call_stmt ->
            (* If callee's summary is not leaf, use it. *)
            UncheckedCallees.replace_location_by_call location callee_summary
            |> UncheckedCallees.join unchecked_callees
        | _ ->
            (* Otherwise, add callee's name. *)
            UncheckedCallees.add {callee; location; call_type= Direct} unchecked_callees
      in
      {astate with unchecked_callees}
    else astate
end

module TransferFunctions = struct
  module CFG = ProcCfg.Normal
  module Domain = Dom

  type analysis_data = Summary.t InterproceduralAnalysis.t

  let is_java_boolean_value_method pname =
    Procname.get_class_name pname |> Option.exists ~f:(String.equal "java.lang.Boolean")
    && Procname.get_method pname |> String.equal "booleanValue"


  let exec_instr astate {InterproceduralAnalysis.tenv; analyze_dependency} _node instr =
    match (instr : Sil.instr) with
    | Load {id; e= Lvar pvar} ->
        Dom.load_config id pvar astate
    | Store {e1= Lvar pvar; e2= Var id} ->
        Dom.store_config pvar id astate
    | Call ((ret, _), Const (Cfun callee), [(Var id, _)], _, _)
      when is_java_boolean_value_method callee ->
        Dom.boolean_value ret id astate
    | Call ((ret, _), Const (Cfun callee), args, location, _) -> (
      match FbGKInteraction.get_config_check tenv callee args with
      | Some (`Config config) ->
          Dom.call_config_check ret config astate
      | Some (`Exp _) ->
          astate
      | None ->
          (* normal function calls *)
          Dom.call analyze_dependency callee location astate )
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
  Option.map (Analyzer.compute_post analysis_data ~initial:Dom.init proc_desc) ~f:(fun astate ->
      let has_call_stmt = has_call_stmt proc_desc in
      Dom.to_summary has_call_stmt astate )
