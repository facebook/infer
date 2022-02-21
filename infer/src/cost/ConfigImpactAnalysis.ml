(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module ConfigName = FbGKInteraction.ConfigName

let is_in_strict_mode_paths file =
  SourceFile.is_matching Config.config_impact_strict_mode_paths file


let strict_mode =
  Config.config_impact_strict_mode
  ||
  match SourceFile.read_config_changed_files () with
  | None ->
      not (List.is_empty Config.config_impact_strict_mode_paths)
  | Some changed_files ->
      SourceFile.Set.exists is_in_strict_mode_paths changed_files


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

module ConfigChecks = struct
  include AbstractDomain.SafeInvertedMap (ConfigName) (Branch)

  let add_all x ~into = fold (fun k v acc -> add k v acc) x into
end

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
  include AbstractDomain.Map (Fields) (UncheckedCallees)

  let weak_update fields callees fields_map =
    update fields
      (function None -> Some callees | Some prev -> Some (UncheckedCallees.union prev callees))
      fields_map


  let replace_location_by_call ~via location fields_map =
    map (UncheckedCallees.replace_location_by_call ~via location) fields_map


  let has_known_expensive_callee fields_map =
    exists (fun _ callees -> UncheckedCallees.has_known_expensive_callee callees) fields_map


  let filter_known_expensive fields_map =
    filter_map
      (fun _ callees ->
        let expensive_callees = UncheckedCallees.filter_known_expensive callees in
        Option.some_if (not (UncheckedCallees.is_empty expensive_callees)) expensive_callees )
      fields_map
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
    | True of ConfigChecks.t  (** config checks when the temporary boolean is true *)
    | False of ConfigChecks.t  (** config checks when the temporary boolean is false *)
    | Joined of {true_: ConfigChecks.t; false_: ConfigChecks.t}
        (** config checks when the temporary boolean is true or false *)

  let pp =
    let pp_helper f (b, config_checks) =
      F.fprintf f "@[%b when %a checked@]" b ConfigChecks.pp config_checks
    in
    fun f -> function
      | Bot ->
          AbstractDomain.BottomLiftedUtils.pp_bottom f
      | True config_checks ->
          pp_helper f (true, config_checks)
      | False config_checks ->
          pp_helper f (false, config_checks)
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
        ConfigChecks.leq ~lhs:c1 ~rhs:c2
    | Joined {true_= t1; false_= f1}, Joined {true_= t2; false_= f2} ->
        ConfigChecks.leq ~lhs:t1 ~rhs:t2 && ConfigChecks.leq ~lhs:f1 ~rhs:f2
    | _, _ ->
        false


  let join x y =
    match (x, y) with
    | Bot, v | v, Bot ->
        v
    | True c1, True c2 ->
        True (ConfigChecks.join c1 c2)
    | False c1, False c2 ->
        False (ConfigChecks.join c1 c2)
    | True c1, False c2 | False c2, True c1 ->
        Joined {true_= c1; false_= c2}
    | True c1, Joined {true_= c2; false_} | Joined {true_= c1; false_}, True c2 ->
        Joined {true_= ConfigChecks.join c1 c2; false_}
    | False c1, Joined {true_; false_= c2} | Joined {true_; false_= c1}, False c2 ->
        Joined {true_; false_= ConfigChecks.join c1 c2}
    | Joined {true_= t1; false_= f1}, Joined {true_= t2; false_= f2} ->
        Joined {true_= ConfigChecks.join t1 t2; false_= ConfigChecks.join f1 f2}


  let widen ~prev ~next ~num_iters =
    match (prev, next) with
    | Bot, v | v, Bot ->
        v
    | True c1, True c2 ->
        True (ConfigChecks.widen ~prev:c1 ~next:c2 ~num_iters)
    | False c1, False c2 ->
        False (ConfigChecks.widen ~prev:c1 ~next:c2 ~num_iters)
    | True c1, False c2 | False c2, True c1 ->
        Joined {true_= c1; false_= c2}
    | True c1, Joined {true_= c2; false_} | Joined {true_= c1; false_}, True c2 ->
        Joined {true_= ConfigChecks.widen ~prev:c1 ~next:c2 ~num_iters; false_}
    | False c1, Joined {true_; false_= c2} | Joined {true_; false_= c1}, False c2 ->
        Joined {true_; false_= ConfigChecks.widen ~prev:c1 ~next:c2 ~num_iters}
    | Joined {true_= t1; false_= f1}, Joined {true_= t2; false_= f2} ->
        Joined
          { true_= ConfigChecks.widen ~prev:t1 ~next:t2 ~num_iters
          ; false_= ConfigChecks.widen ~prev:f1 ~next:f2 ~num_iters }


  let bottom = Bot

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
  type t = {config: ConfigLifted.t; fields: Fields.t; temp_bool: TempBool.t}

  let pp f {config; fields; temp_bool} =
    F.fprintf f "@[@[config:@,%a@]@ @[fields:@,%a@]@ @[temp_bool:@,%a@]@]" ConfigLifted.pp config
      Fields.pp fields TempBool.pp temp_bool


  let leq ~lhs ~rhs =
    ConfigLifted.leq ~lhs:lhs.config ~rhs:rhs.config
    && Fields.leq ~lhs:lhs.fields ~rhs:rhs.fields
    && TempBool.leq ~lhs:lhs.temp_bool ~rhs:rhs.temp_bool


  let join x y =
    { config= ConfigLifted.join x.config y.config
    ; fields= Fields.join x.fields y.fields
    ; temp_bool= TempBool.join x.temp_bool y.temp_bool }


  let widen ~prev ~next ~num_iters =
    { config= ConfigLifted.widen ~prev:prev.config ~next:next.config ~num_iters
    ; fields= Fields.widen ~prev:prev.fields ~next:next.fields ~num_iters
    ; temp_bool= TempBool.widen ~prev:prev.temp_bool ~next:next.temp_bool ~num_iters }


  let bottom = {config= ConfigLifted.bottom; fields= Fields.bottom; temp_bool= TempBool.bottom}

  let of_config config = {bottom with config= ConfigLifted.v config}

  let of_field fn = {bottom with fields= Fields.singleton fn}

  let of_temp_bool ~is_true config_checks =
    {bottom with temp_bool= TempBool.make ~is_true config_checks}
end

module Mem = struct
  include AbstractDomain.Map (Loc) (Val)

  let lookup loc mem = find_opt loc mem |> Option.value ~default:Val.bottom
end

module ClassGateConditions = struct
  module Conditions = AbstractDomain.FiniteSet (Fields)

  type t = Conditional of Conditions.t | Top

  let pp f = function
    | Conditional conds when Conditions.is_empty conds ->
        F.pp_print_string f "gated"
    | Conditional fields ->
        Conditions.pp f fields
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

  let singleton fields = Conditional (Conditions.singleton fields)

  let add fields = function
    | Conditional conds ->
        Conditional (Conditions.add fields conds)
    | Top ->
        Top


  let is_gated_condition ~config_fields fields =
    Fields.exists (fun fname -> Fields.mem fname config_fields) fields


  let is_gated config_fields = function
    | Conditional conds ->
        Conditions.for_all (is_gated_condition ~config_fields) conds
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

module FieldAlias = struct
  include AbstractDomain.FiniteMultiMap (Field) (Field)

  let empty = bottom

  let add_all src tgt x = Fields.fold (fun src acc -> add src tgt acc) src x

  let union x y = fold (fun key field acc -> add key field acc) y x
end

module Summary = struct
  type t =
    { unchecked_callees: UncheckedCallees.t  (** Set of unchecked callees *)
    ; unchecked_callees_cond: UncheckedCalleesCond.t
          (** Sets of unchecked callees that are called conditionally by object fields *)
    ; has_call_stmt: bool  (** True if a function includes a call statement *)
    ; config_fields: Fields.t
          (** Intra-procedurally collected fields that may have config values *)
    ; gated_classes: GatedClasses.t  (** Intra-procedurally collected gated classes *)
    ; field_alias: FieldAlias.t  (** Aliases between field names *)
    ; ret: Val.t  (** Return value of the procedure *) }

  let pp f
      { unchecked_callees
      ; unchecked_callees_cond
      ; has_call_stmt
      ; config_fields
      ; gated_classes
      ; field_alias
      ; ret } =
    F.fprintf f
      "@[@[unchecked callees:@,\
       %a@],@ @[unchecked callees cond:@,\
       %a@],@ @[has_call_stmt:%b@],@ @[config fields:%a@],@ @[gated classes:%a@],@ @[field \
       alias:%a@],@ @[ret:%a@]@]"
      UncheckedCallees.pp unchecked_callees UncheckedCalleesCond.pp unchecked_callees_cond
      has_call_stmt Fields.pp config_fields GatedClasses.pp gated_classes FieldAlias.pp field_alias
      Val.pp ret


  let get_config_fields {config_fields} = config_fields

  let get_gated_classes {gated_classes} = gated_classes

  let get_unchecked_callees {unchecked_callees} = unchecked_callees

  let get_field_alias {field_alias} = field_alias

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


  let has_known_expensive_callee {unchecked_callees; unchecked_callees_cond} =
    UncheckedCallees.has_known_expensive_callee unchecked_callees
    || UncheckedCalleesCond.has_known_expensive_callee unchecked_callees_cond
end

module Dom = struct
  type t =
    { config_checks: ConfigChecks.t
    ; field_checks: FieldChecks.t
    ; unchecked_callees: UncheckedCallees.t
    ; unchecked_callees_cond: UncheckedCalleesCond.t
    ; mem: Mem.t
    ; config_fields: Fields.t
    ; gated_classes: GatedClasses.t
    ; field_alias: FieldAlias.t }

  let pp f
      { config_checks
      ; field_checks
      ; unchecked_callees
      ; unchecked_callees_cond
      ; mem
      ; config_fields
      ; gated_classes
      ; field_alias } =
    F.fprintf f
      "@[@[config checks:@,\
       %a@]@ @[field checks:@,\
       %a@]@ @[unchecked callees:@,\
       %a@]@ @[unchecked callees cond:@,\
       %a@]@ @[mem:@,\
       %a@]@ @[config fields:@,\
       %a@]@ @[gated classes:@,\
       %a@]@ @[field alias:@,\
       %a@]@]"
      ConfigChecks.pp config_checks FieldChecks.pp field_checks UncheckedCallees.pp
      unchecked_callees UncheckedCalleesCond.pp unchecked_callees_cond Mem.pp mem Fields.pp
      config_fields GatedClasses.pp gated_classes FieldAlias.pp field_alias


  let leq ~lhs ~rhs =
    ConfigChecks.leq ~lhs:lhs.config_checks ~rhs:rhs.config_checks
    && FieldChecks.leq ~lhs:lhs.field_checks ~rhs:rhs.field_checks
    && UncheckedCallees.leq ~lhs:lhs.unchecked_callees ~rhs:rhs.unchecked_callees
    && UncheckedCalleesCond.leq ~lhs:lhs.unchecked_callees_cond ~rhs:rhs.unchecked_callees_cond
    && Mem.leq ~lhs:lhs.mem ~rhs:rhs.mem
    && Fields.leq ~lhs:lhs.config_fields ~rhs:rhs.config_fields
    && GatedClasses.leq ~lhs:lhs.gated_classes ~rhs:rhs.gated_classes
    && FieldAlias.leq ~lhs:lhs.field_alias ~rhs:rhs.field_alias


  let join x y =
    { config_checks= ConfigChecks.join x.config_checks y.config_checks
    ; field_checks= FieldChecks.join x.field_checks y.field_checks
    ; unchecked_callees= UncheckedCallees.join x.unchecked_callees y.unchecked_callees
    ; unchecked_callees_cond=
        UncheckedCalleesCond.join x.unchecked_callees_cond y.unchecked_callees_cond
    ; mem= Mem.join x.mem y.mem
    ; config_fields= Fields.join x.config_fields y.config_fields
    ; gated_classes= GatedClasses.join x.gated_classes y.gated_classes
    ; field_alias= FieldAlias.join x.field_alias y.field_alias }


  let widen ~prev ~next ~num_iters =
    { config_checks= ConfigChecks.widen ~prev:prev.config_checks ~next:next.config_checks ~num_iters
    ; field_checks= FieldChecks.widen ~prev:prev.field_checks ~next:next.field_checks ~num_iters
    ; unchecked_callees=
        UncheckedCallees.widen ~prev:prev.unchecked_callees ~next:next.unchecked_callees ~num_iters
    ; unchecked_callees_cond=
        UncheckedCalleesCond.widen ~prev:prev.unchecked_callees_cond
          ~next:next.unchecked_callees_cond ~num_iters
    ; mem= Mem.widen ~prev:prev.mem ~next:next.mem ~num_iters
    ; config_fields= Fields.widen ~prev:prev.config_fields ~next:next.config_fields ~num_iters
    ; gated_classes= GatedClasses.widen ~prev:prev.gated_classes ~next:next.gated_classes ~num_iters
    ; field_alias= FieldAlias.widen ~prev:prev.field_alias ~next:next.field_alias ~num_iters }


  let to_summary pname ~has_call_stmt
      {unchecked_callees; unchecked_callees_cond; config_fields; gated_classes; field_alias; mem} =
    let ret = Mem.lookup (Loc.of_pvar (Pvar.get_ret_pvar pname)) mem in
    { Summary.unchecked_callees
    ; unchecked_callees_cond
    ; has_call_stmt
    ; config_fields
    ; gated_classes
    ; field_alias
    ; ret }


  let init =
    { config_checks= ConfigChecks.top
    ; field_checks= FieldChecks.top
    ; unchecked_callees= UncheckedCallees.bottom
    ; unchecked_callees_cond= UncheckedCalleesCond.bottom
    ; mem= Mem.bottom
    ; config_fields= Fields.bottom
    ; gated_classes= GatedClasses.bottom
    ; field_alias= FieldAlias.bottom }


  let add_mem loc v ({mem} as astate) = {astate with mem= Mem.add loc v mem}

  let copy_mem ~tgt ~src ({mem} as astate) = add_mem tgt (Mem.lookup src mem) astate

  let call_config_check ret config astate = add_mem (Loc.of_id ret) (Val.of_config config) astate

  let load_config id pvar astate = copy_mem ~tgt:(Loc.of_id id) ~src:(Loc.of_pvar pvar) astate

  let load_field id fn astate = add_mem (Loc.of_id id) (Val.of_field fn) astate

  let store_config pvar id astate = copy_mem ~tgt:(Loc.of_pvar pvar) ~src:(Loc.of_id id) astate

  let store_field fn id ({mem; config_fields; field_alias} as astate) =
    let {Val.config; fields} = Mem.lookup (Loc.of_id id) mem in
    if ConfigLifted.is_bottom config then
      {astate with field_alias= FieldAlias.add_all fields fn field_alias}
    else {astate with config_fields= Fields.add fn config_fields}


  let copy_value id_tgt id_src astate =
    copy_mem ~tgt:(Loc.of_id id_tgt) ~src:(Loc.of_id id_src) astate


  let apply_cmp_branch cmp_branch res =
    Option.bind res ~f:(function
      | `Unconditional (config, Branch.True) ->
          Some (`Unconditional (config, cmp_branch))
      | `Conditional (fields, Branch.True) ->
          Some (`Conditional (fields, cmp_branch))
      | _ ->
          None )


  let rec get_config_check_prune ~is_true_branch e mem =
    match (e : Exp.t) with
    | Var id -> (
        let {Val.config; fields; temp_bool} = Mem.lookup (Loc.of_id id) mem in
        let branch = if is_true_branch then Branch.True else Branch.False in
        match ConfigLifted.get config with
        | Some config ->
            Some (`Unconditional (config, branch))
        | None when not (Fields.is_empty fields) ->
            Some (`Conditional (fields, branch))
        | None ->
            (* Note: If the [id] is not a config value and not a field value, address it as a
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


  let prune e ({config_checks; field_checks; mem} as astate) =
    get_config_check_prune ~is_true_branch:true e mem
    |> Option.value_map ~default:astate ~f:(function
         | `Unconditional (config, branch) ->
             {astate with config_checks= ConfigChecks.add config branch config_checks}
         | `TempBool config_checks' ->
             {astate with config_checks= ConfigChecks.add_all config_checks' ~into:config_checks}
         | `Conditional (fields, branch) ->
             { astate with
               field_checks=
                 Fields.fold (fun field acc -> FieldChecks.add field branch acc) fields field_checks
             } )


  type known_expensiveness = FbGKInteraction.known_expensiveness = KnownCheap | KnownExpensive

  let get_expensiveness_model =
    let dispatch : (Tenv.t, known_expensiveness, unit) ProcnameDispatcher.Call.dispatcher =
      let open ProcnameDispatcher.Call in
      let dispatcher =
        make_dispatcher
          [ +BuiltinDecl.(match_builtin __cast) <>--> KnownCheap
          ; +BuiltinDecl.(match_builtin __java_throw) <>--> KnownCheap
          ; +PatternMatch.Java.implements_android "content.SharedPreferences"
            &:: "edit" &--> KnownExpensive
          ; +PatternMatch.Java.implements_android "content.SharedPreferences"
            &::+ (fun _ method_name -> String.is_prefix method_name ~prefix:"get")
            &--> KnownExpensive
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


  let is_setter_getter pname =
    let method_name = Procname.get_method pname in
    String.is_prefix method_name ~prefix:"set" || String.is_prefix method_name ~prefix:"get"


  let update_gated_callees ~callee args ({config_checks; field_checks; gated_classes} as astate) =
    match args with
    | [(Exp.Sizeof {typ= {desc= Tstruct typ_name}}, _)]
      when Procname.equal callee BuiltinDecl.__objc_alloc_no_fail ->
        if ConfigChecks.exists (fun _ -> function True -> true | _ -> false) config_checks then
          (* Gated by true gate condition *)
          {astate with gated_classes= GatedClasses.add_gated typ_name gated_classes}
        else
          let fields =
            FieldChecks.fold
              (fun fname branch acc -> match branch with True -> Fields.add fname acc | _ -> acc)
              field_checks Fields.empty
          in
          if Fields.is_empty fields then
            (* Ungated by any condition *)
            {astate with gated_classes= GatedClasses.add typ_name Top gated_classes}
          else
            (* Gated by fields *)
            {astate with gated_classes= GatedClasses.add_cond typ_name fields gated_classes}
    | _ ->
        astate


  let call tenv analyze_dependency ~(instantiated_cost : CostInstantiate.instantiated_cost option)
      ~callee args location
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
        let is_cheap_call = match instantiated_cost with Some Cheap -> true | _ -> false in
        let is_unmodeled_call = match instantiated_cost with Some NoModel -> true | _ -> false in
        if strict_mode then
          match callee_summary with
          | Some
              { Summary.unchecked_callees= callee_summary
              ; unchecked_callees_cond= callee_summary_cond
              ; has_call_stmt }
            when has_call_stmt ->
              (* If callee's summary is not leaf, use it. *)
              join_callee_summary callee_summary callee_summary_cond
          | _ -> (
            match expensiveness_model with
            | None when is_setter_getter callee ->
                (* If callee is unknown setter/getter, ignore it. *)
                astate
            | Some KnownCheap ->
                (* If callee is known cheap call, ignore it. *)
                astate
            | _ ->
                (* Otherwise, add callee's name. *)
                add_callee_name ~is_known_expensive:false )
        else
          match expensiveness_model with
          | None when is_cheap_call && not has_expensive_callee ->
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
                join_callee_summary callee_summary callee_summary_cond
            | None when Procname.is_objc_init callee || is_unmodeled_call ->
                (* If callee is unknown ObjC initializer or has no cost model, ignore it. *)
                astate
            | _ ->
                (* Otherwise, add callee's name. *)
                add_callee_name ~is_known_expensive:false )
      else astate
    in
    update_gated_callees ~callee args astate


  let throw_exception astate =
    { astate with
      unchecked_callees= UncheckedCallees.empty
    ; unchecked_callees_cond= UncheckedCalleesCond.empty }
end

type analysis_data =
  { interproc:
      (BufferOverrunAnalysisSummary.t option * Summary.t option * CostDomain.summary option)
      InterproceduralAnalysis.t
  ; get_instantiated_cost: CostInstantiate.Call.t -> CostInstantiate.instantiated_cost option }

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
    | Some (_, (_, Some {Summary.ret= ret_val}, _)) ->
        Dom.add_mem (Loc.of_id id) ret_val astate
    | _ ->
        astate


  let call {interproc= {tenv; analyze_dependency}; get_instantiated_cost} node idx
      ((ret_id, _) as ret) callee args captured_vars location astate =
    match FbGKInteraction.get_config_check tenv callee args with
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
        let call =
          CostInstantiate.Call.
            { loc= location
            ; pname= callee
            ; node= CFG.Node.to_instr idx node
            ; args
            ; captured_vars
            ; ret }
        in
        let instantiated_cost = get_instantiated_cost call in
        Dom.call tenv analyze_dependency ~instantiated_cost ~callee args location astate
        |> add_ret analyze_dependency ret_id callee


  let exec_instr ({Dom.config_checks} as astate)
      ({interproc= {tenv; analyze_dependency}} as analysis_data) node idx instr =
    match (instr : Sil.instr) with
    | Load {id; e} -> (
      match FbGKInteraction.get_config e with
      | Some config ->
          Dom.call_config_check id config astate
      | None -> (
        match e with
        | Lvar pvar ->
            Dom.load_config id pvar astate
        | Lfield (_, fn, _) ->
            Dom.load_field id fn astate
        | _ ->
            astate ) )
    | Store {e1= Lvar pvar; e2= Const zero} when Const.iszero_int_float zero ->
        Dom.add_mem (Loc.of_pvar pvar) (Val.of_temp_bool ~is_true:false config_checks) astate
    | Store {e1= Lvar pvar; e2= Const one} when Const.isone_int_float one ->
        Dom.add_mem (Loc.of_pvar pvar) (Val.of_temp_bool ~is_true:true config_checks) astate
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
      when (not strict_mode) && is_known_cheap_method tenv callee ->
        add_ret analyze_dependency ret_id callee astate
    | Call
        ( ret
        , (Const (Cfun dispatch_sync) | Closure {name= dispatch_sync})
        , [_; (Closure {name= callee; captured_vars}, _)]
        , location
        , _ )
      when Procname.equal dispatch_sync BuiltinDecl.dispatch_sync ->
        let args = List.map captured_vars ~f:(fun (exp, _, typ, _) -> (exp, typ)) in
        call analysis_data node idx ret callee args [] location astate
    | Call (ret, Const (Cfun callee), args, location, _) ->
        call analysis_data node idx ret callee args [] location astate
    | Call (ret, Closure {name= callee; captured_vars}, args, location, _) ->
        call analysis_data node idx ret callee args captured_vars location astate
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
  if Procname.is_java_class_initializer pname then
    (* Mitigation: We ignore Java class initializer to avoid non-deterministic FP. *)
    None
  else
    let get_instantiated_cost = CostInstantiate.get_instantiated_cost analysis_data in
    let analysis_data = {interproc= analysis_data; get_instantiated_cost} in
    Option.map (Analyzer.compute_post analysis_data ~initial:Dom.init proc_desc) ~f:(fun astate ->
        let has_call_stmt = has_call_stmt proc_desc in
        Dom.to_summary pname ~has_call_stmt astate )
