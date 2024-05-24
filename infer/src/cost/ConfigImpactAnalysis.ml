(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module ConfigName = FbPulseConfigName

type mode = Jsonconfigimpact_t.config_impact_mode [@@deriving equal]

let pp_mode f mode = F.pp_print_string f (Jsonconfigimpact_j.string_of_config_impact_mode mode)

let is_in_strict_mode_paths file =
  SourceFile.is_matching Config.config_impact_strict_mode_paths file


let is_in_test_paths file = SourceFile.is_matching Config.config_impact_test_paths file

let mode =
  if Config.config_impact_strict_mode then `Strict
  else
    match SourceFile.read_config_changed_files () with
    | None ->
        (* NOTE: ConfigImpact analysis assumes that non-empty changed files are always given. The
           next condition check is only for checker's tests. *)
        if not (List.is_empty Config.config_impact_strict_mode_paths) then `Strict else `Normal
    | Some changed_files ->
        if SourceFile.Set.exists is_in_strict_mode_paths changed_files then `Strict else `Normal


module Branch = struct
  type t = True | False | Lt of Const.t | Gt of Const.t | Le of Const.t | Ge of Const.t | Top
  [@@deriving equal]

  let pp f = function
    | True ->
        F.pp_print_string f "true branch"
    | False ->
        F.pp_print_string f "false branch"
    | Lt c ->
        F.fprintf f "(<%a) branch" (Const.pp Pp.text) c
    | Gt c ->
        F.fprintf f "(>%a) branch" (Const.pp Pp.text) c
    | Le c ->
        F.fprintf f "(<=%a) branch" (Const.pp Pp.text) c
    | Ge c ->
        F.fprintf f "(>=%a) branch" (Const.pp Pp.text) c
    | Top ->
        AbstractDomain.TopLiftedUtils.pp_top f


  let leq ~lhs ~rhs = match rhs with Top -> true | _ -> equal lhs rhs

  let join x y = if equal x y then x else Top

  let widen ~prev ~next ~num_iters:_ = join prev next

  let top = Top

  let is_top = function Top -> true | True | False | Lt _ | Gt _ | Le _ | Ge _ -> false
end

module Field = struct
  include Fieldname

  let compare = Fieldname.compare_name
end

module Param = struct
  type t = Pvar.t [@@deriving compare]

  let pp = Pvar.pp Pp.text
end

(** Placeholder for class fields or function parameters that gated some code. They may or may not
    have config values later. *)
module LatentConfig = struct
  type t = LatentField of Field.t | LatentParam of Param.t [@@deriving compare]

  let pp f = function
    | LatentField field ->
        Field.pp f field
    | LatentParam param ->
        Param.pp f param
end

module LatentConfigs = struct
  include AbstractDomain.FiniteSet (LatentConfig)

  let is_field latent_configs =
    match is_singleton_or_more latent_configs with
    | Singleton (LatentField _) ->
        true
    | Singleton (LatentParam _) | Empty | More ->
        false
end

module MakeConditionChecks (Key : PrettyPrintable.PrintableOrderedType) = struct
  include AbstractDomain.SafeInvertedMap (Key) (Branch)

  let add_all x ~into = fold (fun k v acc -> add k v acc) x into
end

module ConfigChecks = MakeConditionChecks (ConfigName)

module LatentConfigChecks = struct
  include MakeConditionChecks (LatentConfig)

  let get_configs x =
    let add_latent_config config branch acc =
      assert (not (Branch.is_top branch)) ;
      LatentConfigs.add config acc
    in
    fold add_latent_config x LatentConfigs.empty
end

module ConditionChecks = struct
  include AbstractDomain.PairWithTop (ConfigChecks) (LatentConfigChecks)

  let add_config config branch (config_checks, latent_config_checks) =
    (ConfigChecks.add config branch config_checks, latent_config_checks)


  let add_latent_config config branch (config_checks, latent_config_checks) =
    (config_checks, LatentConfigChecks.add config branch latent_config_checks)


  let add_all (config_checks, latent_config_checks) ~into =
    ( ConfigChecks.add_all config_checks ~into:(fst into)
    , LatentConfigChecks.add_all latent_config_checks ~into:(snd into) )
end

module UncheckedCallee = struct
  type callee_name = Procname.t

  let compare_callee_name = Procname.compare_name

  type callee = Callee of callee_name | InstanceOf of Typ.t [@@deriving compare]

  type t =
    { callee: callee
    ; is_known_expensive: bool
    ; location: Location.t [@compare.ignore]
    ; call_type: call_type [@compare.ignore] }
  [@@deriving compare]

  and call_type = Direct | Indirect of {unchecked: t; via: Procname.t}

  let pp_callee f = function
    | Callee pname ->
        Procname.pp f pname
    | InstanceOf {desc= Tstruct (JavaClass java_class_name)} ->
        F.fprintf f "instanceof(%s)" (JavaClassName.classname java_class_name)
    | InstanceOf {desc} ->
        F.fprintf f "instanceof(%a)" (Typ.pp_desc Pp.text) desc


  let pp_common ~with_location f {callee; location; call_type} =
    let pp_call_type f () =
      match call_type with
      | Direct ->
          ()
      | Indirect {via} ->
          F.fprintf f " indirectly via %a" Procname.pp via
    in
    F.fprintf f "%a is called%a" pp_callee callee pp_call_type () ;
    if with_location then F.fprintf f " at %a" Location.pp location


  let pp f x = pp_common ~with_location:true f x

  let pp_without_location f x = pp_common ~with_location:false f x

  let pp_without_location_list f unchecked_callees =
    IList.pp_print_list ~max:Config.config_impact_max_callees_to_print
      ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
      (fun f {callee} -> Format.fprintf f "`%a`" pp_callee callee)
      f unchecked_callees


  let make ~is_known_expensive ~callee location =
    {callee; is_known_expensive; location; call_type= Direct}


  let is_known_expensive {is_known_expensive} = is_known_expensive

  let replace_location_by_call ~via location x =
    {x with location; call_type= Indirect {unchecked= x; via}}


  let rec make_err_trace ({location} as x) =
    let desc = F.asprintf "%a" pp_without_location x in
    let trace_elem = Errlog.make_trace_element 0 location desc [] in
    match x.call_type with
    | Direct ->
        [trace_elem]
    | Indirect {unchecked} ->
        trace_elem :: make_err_trace unchecked
end

module UncheckedCallees = struct
  include AbstractDomain.FiniteSet (UncheckedCallee)

  let replace_location_by_call ~via location x =
    map (UncheckedCallee.replace_location_by_call ~via location) x


  let encode astate = Marshal.to_string astate [] |> Base64.encode_exn

  let decode enc_str = Marshal.from_string (Base64.decode_exn enc_str) 0

  let pp_without_location f x = UncheckedCallee.pp_without_location_list f (elements x)

  let has_known_expensive_callee x = exists (fun {is_known_expensive} -> is_known_expensive) x

  let filter_known_expensive x = filter (fun {is_known_expensive} -> is_known_expensive) x
end

module UncheckedCalleesCond = struct
  include AbstractDomain.Map (LatentConfigs) (UncheckedCallees)

  let weak_update cond callees conds_map =
    update cond
      (function None -> Some callees | Some prev -> Some (UncheckedCallees.union prev callees))
      conds_map


  let replace_location_by_call ~via location conds_map =
    map (UncheckedCallees.replace_location_by_call ~via location) conds_map


  let has_known_expensive_callee conds_map =
    exists (fun _ callees -> UncheckedCallees.has_known_expensive_callee callees) conds_map


  let filter_known_expensive conds_map =
    filter_map
      (fun _ callees ->
        let expensive_callees = UncheckedCallees.filter_known_expensive callees in
        Option.some_if (not (UncheckedCallees.is_empty expensive_callees)) expensive_callees )
      conds_map
end

module Loc = struct
  type t = Ident of Ident.t | Pvar of Pvar.t [@@deriving compare]

  let pp f = function Ident id -> Ident.pp f id | Pvar pvar -> Pvar.pp Pp.text f pvar

  let of_id id = Ident id

  let of_pvar pvar = Pvar pvar
end

module ConfigLifted = AbstractDomain.Flat (ConfigName)

module TempBool = struct
  type t =
    | Bot  (** bottom *)
    | True of ConditionChecks.t  (** config checks when the temporary boolean is true *)
    | False of ConditionChecks.t  (** config checks when the temporary boolean is false *)
    | Joined of {true_: ConditionChecks.t; false_: ConditionChecks.t}
        (** config checks when the temporary boolean is true or false *)

  let pp =
    let pp_helper f (b, condition_checks) =
      F.fprintf f "@[%b when %a checked@]" b ConditionChecks.pp condition_checks
    in
    fun f -> function
      | Bot ->
          AbstractDomain.BottomLiftedUtils.pp_bottom f
      | True checks ->
          pp_helper f (true, checks)
      | False checks ->
          pp_helper f (false, checks)
      | Joined {true_; false_} ->
          F.fprintf f "@[%a@ %a@]" pp_helper (true, true_) pp_helper (false, false_)


  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | Bot, _ ->
        true
    | _, Bot ->
        false
    | True c1, True c2
    | False c1, False c2
    | True c1, Joined {true_= c2}
    | False c1, Joined {false_= c2} ->
        ConditionChecks.leq ~lhs:c1 ~rhs:c2
    | Joined {true_= t1; false_= f1}, Joined {true_= t2; false_= f2} ->
        ConditionChecks.leq ~lhs:t1 ~rhs:t2 && ConditionChecks.leq ~lhs:f1 ~rhs:f2
    | _, _ ->
        false


  let join x y =
    match (x, y) with
    | Bot, v | v, Bot ->
        v
    | True c1, True c2 ->
        True (ConditionChecks.join c1 c2)
    | False c1, False c2 ->
        False (ConditionChecks.join c1 c2)
    | True c1, False c2 | False c2, True c1 ->
        Joined {true_= c1; false_= c2}
    | True c1, Joined {true_= c2; false_} | Joined {true_= c1; false_}, True c2 ->
        Joined {true_= ConditionChecks.join c1 c2; false_}
    | False c1, Joined {true_; false_= c2} | Joined {true_; false_= c1}, False c2 ->
        Joined {true_; false_= ConditionChecks.join c1 c2}
    | Joined {true_= t1; false_= f1}, Joined {true_= t2; false_= f2} ->
        Joined {true_= ConditionChecks.join t1 t2; false_= ConditionChecks.join f1 f2}


  let widen ~prev ~next ~num_iters =
    match (prev, next) with
    | Bot, v | v, Bot ->
        v
    | True c1, True c2 ->
        True (ConditionChecks.widen ~prev:c1 ~next:c2 ~num_iters)
    | False c1, False c2 ->
        False (ConditionChecks.widen ~prev:c1 ~next:c2 ~num_iters)
    | True c1, False c2 | False c2, True c1 ->
        Joined {true_= c1; false_= c2}
    | True c1, Joined {true_= c2; false_} | Joined {true_= c1; false_}, True c2 ->
        Joined {true_= ConditionChecks.widen ~prev:c1 ~next:c2 ~num_iters; false_}
    | False c1, Joined {true_; false_= c2} | Joined {true_; false_= c1}, False c2 ->
        Joined {true_; false_= ConditionChecks.widen ~prev:c1 ~next:c2 ~num_iters}
    | Joined {true_= t1; false_= f1}, Joined {true_= t2; false_= f2} ->
        Joined
          { true_= ConditionChecks.widen ~prev:t1 ~next:t2 ~num_iters
          ; false_= ConditionChecks.widen ~prev:f1 ~next:f2 ~num_iters }


  let bottom = Bot

  let is_bottom = function Bot -> true | True _ | False _ | Joined _ -> false

  let make ~is_true config_checks = if is_true then True config_checks else False config_checks

  let get_config_checks_true = function
    | True config_checks | Joined {true_= config_checks} ->
        Some config_checks
    | _ ->
        None


  let get_config_checks_false = function
    | False config_checks | Joined {false_= config_checks} ->
        Some config_checks
    | _ ->
        None


  let get_config_checks ~is_true x =
    if is_true then get_config_checks_true x else get_config_checks_false x
end

module Val = struct
  type t = {config: ConfigLifted.t; latent_configs: LatentConfigs.t; temp_bool: TempBool.t}

  let pp f {config; latent_configs; temp_bool} =
    F.fprintf f "@[@[config:@,%a@]@ @[latent_configs:@,%a@]@ @[temp_bool:@,%a@]@]" ConfigLifted.pp
      config LatentConfigs.pp latent_configs TempBool.pp temp_bool


  let leq ~lhs ~rhs =
    ConfigLifted.leq ~lhs:lhs.config ~rhs:rhs.config
    && LatentConfigs.leq ~lhs:lhs.latent_configs ~rhs:rhs.latent_configs
    && TempBool.leq ~lhs:lhs.temp_bool ~rhs:rhs.temp_bool


  let join x y =
    { config= ConfigLifted.join x.config y.config
    ; latent_configs= LatentConfigs.join x.latent_configs y.latent_configs
    ; temp_bool= TempBool.join x.temp_bool y.temp_bool }


  let widen ~prev ~next ~num_iters =
    { config= ConfigLifted.widen ~prev:prev.config ~next:next.config ~num_iters
    ; latent_configs=
        LatentConfigs.widen ~prev:prev.latent_configs ~next:next.latent_configs ~num_iters
    ; temp_bool= TempBool.widen ~prev:prev.temp_bool ~next:next.temp_bool ~num_iters }


  let bottom =
    {config= ConfigLifted.bottom; latent_configs= LatentConfigs.bottom; temp_bool= TempBool.bottom}


  let of_config config = {bottom with config= ConfigLifted.v config}

  let of_field fn = {bottom with latent_configs= LatentConfigs.singleton (LatentField fn)}

  let of_param pvar = {bottom with latent_configs= LatentConfigs.singleton (LatentParam pvar)}

  let of_temp_bool ~is_true config_checks =
    {bottom with temp_bool= TempBool.make ~is_true config_checks}


  let is_field {config; latent_configs; temp_bool} =
    ConfigLifted.is_bottom config
    && LatentConfigs.is_field latent_configs
    && TempBool.is_bottom temp_bool
end

module Mem = struct
  include AbstractDomain.Map (Loc) (Val)

  let lookup loc mem = find_opt loc mem |> Option.value ~default:Val.bottom
end

module ClassGateConditions = struct
  module Conditions = AbstractDomain.FiniteSet (LatentConfigs)

  type t = Conditional of Conditions.t | Top

  let pp f = function
    | Conditional conds when Conditions.is_empty conds ->
        F.pp_print_string f "gated"
    | Conditional conds ->
        Conditions.pp f conds
    | Top ->
        F.pp_print_string f "ungated"


  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | _, Top ->
        true
    | Top, _ ->
        false
    | Conditional lhs, Conditional rhs ->
        Conditions.leq ~lhs ~rhs


  let join x y =
    match (x, y) with
    | _, Top | Top, _ ->
        Top
    | Conditional x, Conditional y ->
        Conditional (Conditions.join x y)


  let widen ~prev ~next ~num_iters:_ = join prev next

  let empty = Conditional Conditions.empty

  let singleton latent_configs = Conditional (Conditions.singleton latent_configs)

  let add latent_configs = function
    | Conditional conds ->
        Conditional (Conditions.add latent_configs conds)
    | Top ->
        Top


  let is_gated_condition ~configs cond =
    LatentConfigs.exists (fun latent_config -> LatentConfigs.mem latent_config configs) cond


  let is_gated configs = function
    | Conditional conds ->
        Conditions.for_all (is_gated_condition ~configs) conds
    | Top ->
        false
end

module GatedClasses = struct
  include AbstractDomain.Map (Typ.Name) (ClassGateConditions)

  let add_cond typ fields m =
    update typ
      (function
        | None ->
            Some (ClassGateConditions.singleton fields)
        | Some gate_conditions ->
            Some (ClassGateConditions.add fields gate_conditions) )
      m


  let add_gated typ m =
    update typ
      (function
        | None -> Some ClassGateConditions.empty | Some _ as gate_conditions -> gate_conditions )
      m
end

module LatentConfigAlias = struct
  include AbstractDomain.FiniteMultiMap (LatentConfig) (LatentConfig)

  let empty = bottom

  let add_all src tgt x = LatentConfigs.fold (fun src acc -> add src tgt acc) src x

  let union x y = fold (fun key field acc -> add key field acc) y x

  let get_all src x = find_all src x
end

module Summary = struct
  type t =
    { unchecked_callees: UncheckedCallees.t  (** Set of unchecked callees *)
    ; unchecked_callees_cond: UncheckedCalleesCond.t
          (** Sets of unchecked callees that are called conditionally by object fields *)
    ; has_call_stmt: bool  (** True if a function includes a call statement *)
    ; configs: LatentConfigs.t
          (** Intra-procedurally collected fields and function parameters that may have config
              values *)
    ; gated_classes: GatedClasses.t  (** Intra-procedurally collected gated classes *)
    ; latent_config_alias: LatentConfigAlias.t  (** Aliases between latent configs *)
    ; ret: Val.t  (** Return value of the procedure *) }

  let pp f
      { unchecked_callees
      ; unchecked_callees_cond
      ; has_call_stmt
      ; configs
      ; gated_classes
      ; latent_config_alias
      ; ret } =
    F.fprintf f
      "@[@[unchecked callees:@,\
       %a@],@ @[unchecked callees cond:@,\
       %a@],@ @[has_call_stmt:%b@],@ @[configs:%a@],@ @[gated classes:%a@],@ @[latent config \
       alias:%a@],@ @[ret:%a@]@]"
      UncheckedCallees.pp unchecked_callees UncheckedCalleesCond.pp unchecked_callees_cond
      has_call_stmt LatentConfigs.pp configs GatedClasses.pp gated_classes LatentConfigAlias.pp
      latent_config_alias Val.pp ret


  let get_configs {configs} = configs

  let get_gated_classes {gated_classes} = gated_classes

  let get_unchecked_callees {unchecked_callees} = unchecked_callees

  let get_latent_config_alias {latent_config_alias} = latent_config_alias

  let instantiate_unchecked_callees_cond ~all_configs
      ({unchecked_callees; unchecked_callees_cond} as x) =
    let unchecked_callees =
      UncheckedCalleesCond.fold
        (fun latent_configs callees acc ->
          if LatentConfigs.is_empty (LatentConfigs.inter all_configs latent_configs) then
            UncheckedCallees.union acc callees
          else acc )
        unchecked_callees_cond unchecked_callees
    in
    {x with unchecked_callees; unchecked_callees_cond= UncheckedCalleesCond.bottom}


  let has_known_expensive_callee {unchecked_callees; unchecked_callees_cond} =
    UncheckedCallees.has_known_expensive_callee unchecked_callees
    || UncheckedCalleesCond.has_known_expensive_callee unchecked_callees_cond
end

module Dom = struct
  type t =
    { condition_checks: ConditionChecks.t
    ; unchecked_callees: UncheckedCallees.t
    ; unchecked_callees_cond: UncheckedCalleesCond.t
    ; mem: Mem.t
    ; configs: LatentConfigs.t
    ; gated_classes: GatedClasses.t
    ; latent_config_alias: LatentConfigAlias.t }

  let pp f
      { condition_checks
      ; unchecked_callees
      ; unchecked_callees_cond
      ; mem
      ; configs
      ; gated_classes
      ; latent_config_alias } =
    F.fprintf f
      "@[@[condition checks:@,\
       %a@]@ @[unchecked callees:@,\
       %a@]@ @[unchecked callees cond:@,\
       %a@]@ @[mem:@,\
       %a@]@ @[configs:@,\
       %a@]@ @[gated classes:@,\
       %a@]@ @[latent config alias:@,\
       %a@]@]"
      ConditionChecks.pp condition_checks UncheckedCallees.pp unchecked_callees
      UncheckedCalleesCond.pp unchecked_callees_cond Mem.pp mem LatentConfigs.pp configs
      GatedClasses.pp gated_classes LatentConfigAlias.pp latent_config_alias


  let leq ~lhs ~rhs =
    ConditionChecks.leq ~lhs:lhs.condition_checks ~rhs:rhs.condition_checks
    && UncheckedCallees.leq ~lhs:lhs.unchecked_callees ~rhs:rhs.unchecked_callees
    && UncheckedCalleesCond.leq ~lhs:lhs.unchecked_callees_cond ~rhs:rhs.unchecked_callees_cond
    && Mem.leq ~lhs:lhs.mem ~rhs:rhs.mem
    && LatentConfigs.leq ~lhs:lhs.configs ~rhs:rhs.configs
    && GatedClasses.leq ~lhs:lhs.gated_classes ~rhs:rhs.gated_classes
    && LatentConfigAlias.leq ~lhs:lhs.latent_config_alias ~rhs:rhs.latent_config_alias


  let join x y =
    { condition_checks= ConditionChecks.join x.condition_checks y.condition_checks
    ; unchecked_callees= UncheckedCallees.join x.unchecked_callees y.unchecked_callees
    ; unchecked_callees_cond=
        UncheckedCalleesCond.join x.unchecked_callees_cond y.unchecked_callees_cond
    ; mem= Mem.join x.mem y.mem
    ; configs= LatentConfigs.join x.configs y.configs
    ; gated_classes= GatedClasses.join x.gated_classes y.gated_classes
    ; latent_config_alias= LatentConfigAlias.join x.latent_config_alias y.latent_config_alias }


  let widen ~prev ~next ~num_iters =
    { condition_checks=
        ConditionChecks.widen ~prev:prev.condition_checks ~next:next.condition_checks ~num_iters
    ; unchecked_callees=
        UncheckedCallees.widen ~prev:prev.unchecked_callees ~next:next.unchecked_callees ~num_iters
    ; unchecked_callees_cond=
        UncheckedCalleesCond.widen ~prev:prev.unchecked_callees_cond
          ~next:next.unchecked_callees_cond ~num_iters
    ; mem= Mem.widen ~prev:prev.mem ~next:next.mem ~num_iters
    ; configs= LatentConfigs.widen ~prev:prev.configs ~next:next.configs ~num_iters
    ; gated_classes= GatedClasses.widen ~prev:prev.gated_classes ~next:next.gated_classes ~num_iters
    ; latent_config_alias=
        LatentConfigAlias.widen ~prev:prev.latent_config_alias ~next:next.latent_config_alias
          ~num_iters }


  let to_summary pname ~has_call_stmt
      {unchecked_callees; unchecked_callees_cond; configs; gated_classes; latent_config_alias; mem}
      =
    let ret = Mem.lookup (Loc.of_pvar (Pvar.get_ret_pvar pname)) mem in
    { Summary.unchecked_callees
    ; unchecked_callees_cond
    ; has_call_stmt
    ; configs
    ; gated_classes
    ; latent_config_alias
    ; ret }


  let init =
    { condition_checks= ConditionChecks.top
    ; unchecked_callees= UncheckedCallees.bottom
    ; unchecked_callees_cond= UncheckedCalleesCond.bottom
    ; mem= Mem.bottom
    ; configs= LatentConfigs.bottom
    ; gated_classes= GatedClasses.bottom
    ; latent_config_alias= LatentConfigAlias.bottom }


  let add_mem loc v ({mem} as astate) = {astate with mem= Mem.add loc v mem}

  let copy_mem ~tgt ~src ({mem} as astate) = add_mem tgt (Mem.lookup src mem) astate

  let call_config_check ret config astate = add_mem (Loc.of_id ret) (Val.of_config config) astate

  let load_config id pvar astate = copy_mem ~tgt:(Loc.of_id id) ~src:(Loc.of_pvar pvar) astate

  let load_field id fn astate = add_mem (Loc.of_id id) (Val.of_field fn) astate

  let load_param id pvar astate = add_mem (Loc.of_id id) (Val.of_param pvar) astate

  let store_config pvar id astate = copy_mem ~tgt:(Loc.of_pvar pvar) ~src:(Loc.of_id id) astate

  let assign_to_latent_config latent_config id ({mem; configs; latent_config_alias} as astate) =
    let {Val.config; latent_configs} = Mem.lookup (Loc.of_id id) mem in
    if ConfigLifted.is_bottom config then
      { astate with
        latent_config_alias=
          LatentConfigAlias.add_all latent_configs latent_config latent_config_alias }
    else {astate with configs= LatentConfigs.add latent_config configs}


  let store_field fn id astate = assign_to_latent_config (LatentField fn) id astate

  let copy_value id_tgt id_src astate =
    copy_mem ~tgt:(Loc.of_id id_tgt) ~src:(Loc.of_id id_src) astate


  let apply_cmp_branch cmp_branch res =
    Option.bind res ~f:(function
      | `Unconditional (config, Branch.True) ->
          Some (`Unconditional (config, cmp_branch))
      | `Conditional (conds, Branch.True) ->
          Some (`Conditional (conds, cmp_branch))
      | _ ->
          None )


  let rec get_config_check_prune ~is_true_branch e mem =
    match (e : Exp.t) with
    | Var id -> (
        let {Val.config; latent_configs; temp_bool} = Mem.lookup (Loc.of_id id) mem in
        let branch = if is_true_branch then Branch.True else Branch.False in
        match ConfigLifted.get config with
        | Some config ->
            Some (`Unconditional (config, branch))
        | None when not (LatentConfigs.is_empty latent_configs) ->
            Some (`Conditional (latent_configs, branch))
        | None ->
            (* Note: If the [id] is not a config value, field, or parameter, address it as a
               temporary variable. *)
            TempBool.get_config_checks ~is_true:is_true_branch temp_bool
            |> Option.map ~f:(fun config_checks -> `TempBool config_checks) )
    | UnOp (LNot, e, _) ->
        get_config_check_prune ~is_true_branch:(not is_true_branch) e mem
    | BinOp ((Eq | Ne | Lt | Gt | Le | Ge), Const _, Const _) ->
        None
    | (BinOp (Eq, e, (Const _ as const)) | BinOp (Eq, (Const _ as const), e)) when Exp.is_zero const
      ->
        get_config_check_prune ~is_true_branch:(not is_true_branch) e mem
    | (BinOp (Ne, e, (Const _ as const)) | BinOp (Ne, (Const _ as const), e)) when Exp.is_zero const
      ->
        get_config_check_prune ~is_true_branch e mem
    | BinOp (Lt, e, Const c) | BinOp (Gt, Const c, e) ->
        get_config_check_prune ~is_true_branch:true e mem
        |> apply_cmp_branch (if is_true_branch then Branch.Lt c else Branch.Ge c)
    | BinOp (Gt, e, Const c) | BinOp (Lt, Const c, e) ->
        get_config_check_prune ~is_true_branch:true e mem
        |> apply_cmp_branch (if is_true_branch then Branch.Gt c else Branch.Le c)
    | BinOp (Le, e, Const c) | BinOp (Ge, Const c, e) ->
        get_config_check_prune ~is_true_branch:true e mem
        |> apply_cmp_branch (if is_true_branch then Branch.Le c else Branch.Gt c)
    | BinOp (Ge, e, Const c) | BinOp (Le, Const c, e) ->
        get_config_check_prune ~is_true_branch:true e mem
        |> apply_cmp_branch (if is_true_branch then Branch.Ge c else Branch.Lt c)
    | _ ->
        None


  let prune e ({condition_checks; mem} as astate) =
    get_config_check_prune ~is_true_branch:true e mem
    |> Option.value_map ~default:astate ~f:(function
         | `Unconditional (config, branch) ->
             { astate with
               condition_checks= ConditionChecks.add_config config branch condition_checks }
         | `TempBool condition_checks' ->
             { astate with
               condition_checks= ConditionChecks.add_all condition_checks' ~into:condition_checks }
         | `Conditional (conds, branch) ->
             { astate with
               condition_checks=
                 LatentConfigs.fold
                   (fun config acc -> ConditionChecks.add_latent_config config branch acc)
                   conds condition_checks } )


  type known_expensiveness = FbGKInteraction.known_expensiveness = KnownCheap | KnownExpensive

  let get_expensiveness_model =
    let dispatch : (Tenv.t, known_expensiveness, unit) ProcnameDispatcher.Call.dispatcher =
      let open ProcnameDispatcher.Call in
      let dispatcher =
        make_dispatcher
          [ +BuiltinDecl.(match_builtin __cast) <>--> KnownCheap
          ; +BuiltinDecl.(match_builtin __java_throw) <>--> KnownCheap
          ; +PatternMatch.Java.implements_android "content.SharedPreferences"
            &:: "edit" &--> KnownCheap
          ; +PatternMatch.Java.implements_android "content.SharedPreferences"
            &::+ (fun _ method_name -> String.is_prefix method_name ~prefix:"get")
            &--> KnownExpensive
          ; +PatternMatch.Java.implements_arrays &:: "sort" $ any_arg $+...$--> KnownExpensive
          ; +PatternMatch.Java.implements_collections &:: "sort" $ any_arg $+...$--> KnownExpensive
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
          ; +PatternMatch.Java.implements_lang "StringBuilder" &:: "append" &--> KnownExpensive
          ; +PatternMatch.Java.implements_list &:: "sort" $ any_arg $+...$--> KnownExpensive
          ; +PatternMatch.Java.implements_regex "Pattern" &:: "compile" &--> KnownExpensive
          ; +PatternMatch.Java.implements_regex "Pattern" &:: "matcher" &--> KnownExpensive
          ; +PatternMatch.Java.implements_kotlin_intrinsics &::.*--> KnownCheap
          ; -"UICKeyChainStore" &::+ startsWith "dataForKey:" &--> KnownExpensive ]
      in
      merge_dispatchers dispatcher FbGKInteraction.ExpensivenessModel.dispatcher
    in
    fun tenv pname args ->
      let args =
        List.map args ~f:(fun (exp, typ) ->
            ProcnameDispatcher.Call.FuncArg.{exp; typ; arg_payload= ()} )
      in
      dispatch tenv pname args


  let is_config_typ =
    let number_types =
      ["java.lang.Boolean"; "java.lang.Double"; "java.lang.Long"; "java.lang.Number"]
    in
    fun {Typ.desc} ->
      match desc with
      | Tint _ | Tfloat _ ->
          true
      | Tptr ({desc= Tstruct name}, _) ->
          let s = Typ.Name.name name in
          List.exists number_types ~f:(String.equal s)
      | Tvoid | Tfun | TVar _ | Tarray _ | Tstruct _ | Tptr _ ->
          false


  let is_config_setter_typ ~is_static ret_typ args =
    Typ.is_void ret_typ
    &&
    match (args, is_static) with
    | ([(_, arg_typ)] | [_; (_, arg_typ)]), None
    | [(_, arg_typ)], Some true
    | [_; (_, arg_typ)], Some false ->
        is_config_typ arg_typ
    | _ ->
        false


  let is_config_getter_typ ~is_static ret_typ args =
    is_config_typ ret_typ
    &&
    match (args, is_static) with
    | ([] | [_]), None | [], Some true | [_], Some false ->
        true
    | _ ->
        false


  let is_NS_method pname =
    Procname.get_class_name pname |> Option.exists ~f:(String.is_prefix ~prefix:"NS")


  let is_config_setter_getter ~is_static ret_typ pname args =
    let method_name = Procname.get_method pname in
    (is_config_setter_typ ~is_static ret_typ args && String.is_prefix method_name ~prefix:"set")
    || is_config_getter_typ ~is_static ret_typ args
       && ( String.is_prefix method_name ~prefix:"get"
          || Language.curr_language_is Clang
             && (not (is_NS_method pname))
             && String.is_suffix method_name ~suffix:"Value" )


  let is_kotlin_getter pname args =
    match (Attributes.load pname, args) with
    | Some {loc= {file}}, [_] ->
        (not (SourceFile.is_invalid file))
        && SourceFile.has_extension ~ext:Config.kotlin_source_extension file
        && String.is_prefix (Procname.get_method pname) ~prefix:"get"
    | _, _ ->
        false


  let update_gated_callees ~callee args
      ({condition_checks= config_checks, latent_config_checks; gated_classes} as astate) =
    let update_gated_class_constructor typ_name =
      if ConfigChecks.exists (fun _ -> function True -> true | _ -> false) config_checks then
        (* Gated by true gate condition *)
        {astate with gated_classes= GatedClasses.add_gated typ_name gated_classes}
      else
        let cond =
          LatentConfigChecks.fold
            (fun latent_config branch acc ->
              match branch with True -> LatentConfigs.add latent_config acc | _ -> acc )
            latent_config_checks LatentConfigs.empty
        in
        if LatentConfigs.is_empty cond then
          (* Ungated by any condition *)
          {astate with gated_classes= GatedClasses.add typ_name Top gated_classes}
        else
          (* Gated by latent configs *)
          {astate with gated_classes= GatedClasses.add_cond typ_name cond gated_classes}
    in
    let typ_name =
      match args with
      | [(Exp.Sizeof {typ= {desc= Tstruct typ_name}}, _)]
        when Procname.equal callee BuiltinDecl.__objc_alloc_no_fail ->
          Some typ_name
      | _ ->
          if Procname.is_constructor callee then Procname.get_class_type_name callee else None
    in
    Option.value_map typ_name ~default:astate ~f:update_gated_class_constructor


  let update_latent_params formals args astate =
    Option.value_map formals ~default:astate ~f:(fun formals ->
        match
          List.fold2 formals args ~init:astate ~f:(fun acc formal arg ->
              match (formal, arg) with
              | (pvar, _), (Exp.Var id, _) ->
                  assign_to_latent_config (LatentParam pvar) id acc
              | _, _ ->
                  acc )
        with
        | Ok astate ->
            astate
        | Unequal_lengths ->
            astate )


  let call tenv analyze_dependency ret_typ ~callee formals args location
      ( { condition_checks= config_checks, latent_config_checks
        ; unchecked_callees
        ; unchecked_callees_cond } as astate ) =
    let join_unchecked_callees new_unchecked_callees new_unchecked_callees_cond =
      if LatentConfigChecks.is_top latent_config_checks then
        { astate with
          unchecked_callees= UncheckedCallees.join unchecked_callees new_unchecked_callees
        ; unchecked_callees_cond=
            UncheckedCalleesCond.join unchecked_callees_cond new_unchecked_callees_cond }
      else
        let cond_to_add = LatentConfigChecks.get_configs latent_config_checks in
        let unchecked_callees_cond =
          UncheckedCalleesCond.weak_update cond_to_add new_unchecked_callees unchecked_callees_cond
        in
        let unchecked_callees_cond =
          UncheckedCalleesCond.fold
            (fun cond callees acc ->
              UncheckedCalleesCond.weak_update (LatentConfigs.union cond cond_to_add) callees acc )
            new_unchecked_callees_cond unchecked_callees_cond
        in
        {astate with unchecked_callees_cond}
    in
    let join_callee_summary callee_summary callee_summary_cond =
      join_unchecked_callees
        (UncheckedCallees.replace_location_by_call ~via:callee location callee_summary)
        (UncheckedCalleesCond.replace_location_by_call location ~via:callee callee_summary_cond)
    in
    let add_callee_name ~is_known_expensive =
      let callee =
        match args with
        | _ :: (Exp.Sizeof {typ}, _) :: _ when Procname.equal callee BuiltinDecl.__instanceof ->
            UncheckedCallee.InstanceOf typ
        | _ ->
            UncheckedCallee.Callee callee
      in
      join_unchecked_callees
        (UncheckedCallees.singleton (UncheckedCallee.make ~is_known_expensive ~callee location))
        UncheckedCalleesCond.empty
    in
    let astate =
      if ConfigChecks.is_top config_checks then
        let callee_summary = analyze_dependency callee in
        let expensiveness_model = get_expensiveness_model tenv callee args in
        let has_expensive_callee =
          callee_summary |> Result.ok |> Option.exists ~f:Summary.has_known_expensive_callee
        in
        (* We apply a heuristic to ignore Kotlin's getter as cheap. *)
        let is_cheap_call = is_kotlin_getter callee args in
        match mode with
        | `Strict -> (
            let is_static = Procname.is_static callee in
            match (callee_summary, expensiveness_model) with
            | _, Some KnownCheap ->
                (* If callee is known cheap call, ignore it. *)
                astate
            | ( Ok
                  { Summary.unchecked_callees= callee_summary
                  ; unchecked_callees_cond= callee_summary_cond
                  ; has_call_stmt }
              , _ )
              when has_call_stmt ->
                (* If callee's summary is not leaf, use it. *)
                join_callee_summary callee_summary callee_summary_cond
            | Ok {Summary.has_call_stmt; ret}, _
              when (not has_call_stmt)
                   && ( is_config_setter_typ ~is_static ret_typ args
                      || (is_config_getter_typ ~is_static ret_typ args && Val.is_field ret) ) ->
                (* If callee seems to be a setter/getter, ignore it. *)
                astate
            | Error _, None when is_config_setter_getter ~is_static ret_typ callee args ->
                (* If callee is unknown setter/getter, ignore it. *)
                astate
            | _, _ ->
                (* Otherwise, add callee's name. *)
                add_callee_name ~is_known_expensive:false )
        | `Normal -> (
          match expensiveness_model with
          | None when not has_expensive_callee ->
              (* If callee is cheap by heuristics, ignore it. *)
              astate
          | Some KnownCheap ->
              (* If callee is known cheap by model, ignore it. *)
              astate
          | Some KnownExpensive ->
              (* If callee is known expensive by model, add callee's name. *)
              add_callee_name ~is_known_expensive:true
          | None -> (
            match callee_summary with
            | Ok
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
                join_callee_summary callee_summary callee_summary_cond
            | Error _ when Procname.is_objc_init callee ->
                (* If callee is unknown ObjC initializer, ignore it. *)
                astate
            | _ ->
                (* Otherwise, add callee's name. *)
                add_callee_name ~is_known_expensive:false ) )
      else astate
    in
    update_gated_callees ~callee args astate |> update_latent_params formals args


  let throw_exception astate =
    { astate with
      unchecked_callees= UncheckedCallees.empty
    ; unchecked_callees_cond= UncheckedCalleesCond.empty }
end

type analysis_data =
  { interproc: Summary.t InterproceduralAnalysis.t
  ; get_formals: Procname.t -> (Pvar.t * Typ.t) list option
  ; is_param: Pvar.t -> bool }

module TransferFunctions = struct
  module CFG = ProcCfg.Normal
  module Domain = Dom

  type nonrec analysis_data = analysis_data

  let is_modeled_as_id =
    let open ProcnameDispatcher.ProcName in
    let dispatch : (Tenv.t, unit, unit) dispatcher =
      make_dispatcher
        [ +BuiltinDecl.(match_builtin __cast) &--> ()
        ; +PatternMatch.Java.implements_android "text.TextUtils" &:: "isEmpty" &--> ()
        ; +PatternMatch.Java.implements_lang "Boolean" &:: "booleanValue" &--> ()
        ; +PatternMatch.Java.implements_lang "Boolean" &:: "valueOf" &--> ()
        ; +PatternMatch.Java.implements_lang "Double" &:: "doubleValue" &--> ()
        ; +PatternMatch.Java.implements_lang "Double" &:: "valueOf" &--> ()
        ; +PatternMatch.Java.implements_lang "Long" &:: "longValue" &--> ()
        ; +PatternMatch.Java.implements_lang "Long" &:: "valueOf" &--> ()
        ; +PatternMatch.Java.implements_lang "Number" &::.*--> () ]
    in
    fun tenv pname -> dispatch tenv pname |> Option.is_some


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


  let add_ret analyze_dependency id callee astate =
    match analyze_dependency callee with
    | Ok {Summary.ret= ret_val} ->
        Dom.add_mem (Loc.of_id id) ret_val astate
    | Error _ ->
        astate


  let get_kotlin_lazy_method =
    let regexp =
      (* NOTE: Found two cases so far, `getFoo` and `getFoo$<full class path>`. *)
      Str.regexp "^get\\([a-zA-Z0-9_]*\\)"
    in
    fun ~caller ~callee ->
      if
        Procname.is_java callee && String.equal (Procname.to_string callee) "Object Lazy.getValue()"
      then
        let getter = Procname.get_method caller in
        match Procname.get_class_type_name caller with
        | Some class_name when Str.string_match regexp getter 0 ->
            let original_method = String.uncapitalize (Str.matched_group 1 getter) in
            let invoke_class_name =
              Typ.Name.Java.from_string (Typ.Name.name class_name ^ "$" ^ original_method ^ "$2")
            in
            Some
              (Procname.make_java ~class_name:invoke_class_name
                 ~return_type:(Some StdTyp.Java.pointer_to_java_lang_object) ~method_name:"invoke"
                 ~parameters:[] ~kind:Non_Static )
        | _ ->
            None
      else None


  let call {interproc= {proc_desc; tenv; analyze_dependency}; get_formals; is_param}
      (ret_id, ret_typ) callee args location astate =
    let callee =
      get_kotlin_lazy_method ~caller:(Procdesc.get_proc_name proc_desc) ~callee
      |> Option.value_map ~default:callee ~f:(fun invoke ->
             Logging.d_printfln_escaped "Replace (%a) to (%a)" Procname.pp callee Procname.pp invoke ;
             invoke )
    in
    match FbGKInteraction.get_config_check ~is_param tenv callee args with
    | Some (`Config config) ->
        Dom.call_config_check ret_id config astate
    | Some (`Exp (Exp.Var id)) ->
        Dom.copy_value ret_id id astate
    | Some (`Exp (Exp.Lvar pvar)) ->
        Dom.copy_mem ~tgt:(Loc.of_id ret_id) ~src:(Loc.of_pvar pvar) astate
    | Some (`Exp _) ->
        (* NOTE: We need a more proper evaluation function for handling the case. *)
        add_ret analyze_dependency ret_id callee astate
    | Some (`ConfigToPvar (config, pvar)) ->
        Dom.add_mem (Loc.of_pvar pvar) (Val.of_config config) astate
    | None ->
        (* normal function calls *)
        let formals = get_formals callee in
        Dom.call tenv analyze_dependency ret_typ ~callee formals args location astate
        |> add_ret analyze_dependency ret_id callee


  let exec_instr ({Dom.condition_checks} as astate)
      ({interproc= {tenv; analyze_dependency}; is_param} as analysis_data) _node _idx instr =
    match (instr : Sil.instr) with
    | Load {id; e} -> (
      match FbGKInteraction.get_config ~is_param e with
      | Some config ->
          Dom.call_config_check id config astate
      | None -> (
        match e with
        | Lvar pvar when is_param pvar ->
            Dom.load_param id pvar astate
        | Lvar pvar ->
            Dom.load_config id pvar astate
        | Lfield (_, fn, _) ->
            Dom.load_field id fn astate
        | _ ->
            astate ) )
    | Store {e1= Lvar pvar; e2= Const zero} when Const.iszero_int_float zero ->
        Dom.add_mem (Loc.of_pvar pvar) (Val.of_temp_bool ~is_true:false condition_checks) astate
    | Store {e1= Lvar pvar; e2= Const one} when Const.isone_int_float one ->
        Dom.add_mem (Loc.of_pvar pvar) (Val.of_temp_bool ~is_true:true condition_checks) astate
    | Store {e1= Lvar pvar; e2= Var id} ->
        Dom.store_config pvar id astate
    | Store {e1= Lvar pvar; e2= Exn _} when Pvar.is_return pvar ->
        Dom.throw_exception astate
    | Store {e1= Lfield (_, fn, _); e2= Var id} ->
        Dom.store_field fn id astate
    | Call (_, (Const (Cfun callee) | Closure {name= callee}), _, _, _)
      when Procname.is_java_class_initializer callee
           (* Mitigation: We ignore Java class initializer to avoid non-deterministic FP. *)
           || FbGKInteraction.is_lazy_instance callee ->
        astate
    | Call ((ret_id, _), (Const (Cfun callee) | Closure {name= callee}), (Var id, _) :: _, _, _)
      when is_modeled_as_id tenv callee ->
        Dom.copy_value ret_id id astate
    | Call ((ret_id, _), (Const (Cfun callee) | Closure {name= callee}), _, _, _)
      when equal_mode mode `Normal && is_known_cheap_method tenv callee ->
        add_ret analyze_dependency ret_id callee astate
    | Call
        ( ret
        , (Const (Cfun dispatch_sync) | Closure {name= dispatch_sync})
        , [_; (Closure {name= callee; captured_vars}, _)]
        , location
        , _ )
      when Procname.equal dispatch_sync BuiltinDecl.dispatch_sync ->
        let args = List.map captured_vars ~f:(fun (exp, _, typ, _) -> (exp, typ)) in
        call analysis_data ret callee args location astate
    | Call (ret, Const (Cfun callee), args, location, _)
    | Call (ret, Closure {name= callee}, args, location, _) ->
        call analysis_data ret callee args location astate
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
  let pname = Procdesc.get_proc_name proc_desc in
  if
    Procname.is_java_class_initializer pname
    (* Mitigation: We ignore Java class initializer to avoid non-deterministic FP. *)
    || is_in_test_paths (Procdesc.get_attributes proc_desc).translation_unit
  then None
  else
    let get_formals pname =
      Attributes.load pname |> Option.map ~f:ProcAttributes.get_pvar_formals
    in
    let is_param =
      let formals = Procdesc.get_pvar_formals proc_desc in
      fun pvar -> List.exists formals ~f:(fun (formal, _) -> Pvar.equal pvar formal)
    in
    let analysis_data = {interproc= analysis_data; get_formals; is_param} in
    Option.map (Analyzer.compute_post analysis_data ~initial:Dom.init proc_desc) ~f:(fun astate ->
        let has_call_stmt = has_call_stmt proc_desc in
        Dom.to_summary pname ~has_call_stmt astate )
