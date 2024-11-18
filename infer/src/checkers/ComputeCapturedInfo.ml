(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module CheckedForNull = struct
  module DomainData = struct
    type t = {checked: bool; captured_in_closure: Procname.t option} [@@deriving compare]

    let pp fmt {checked; captured_in_closure} =
      F.fprintf fmt "checked=%b, captured_in_closure=%a" checked (Pp.option Procname.pp)
        captured_in_closure
  end

  module PVars = struct
    include PrettyPrintable.MakePPMonoMap (Mangled) (DomainData)

    let compare = compare DomainData.compare
  end

  module Vars = struct
    include PrettyPrintable.MakePPMonoMap (Ident) (Mangled)

    let compare = compare Mangled.compare
  end

  module Mem = struct
    type t = {vars: Vars.t; pvars: PVars.t} [@@deriving compare]

    let pp fmt {vars; pvars} = F.fprintf fmt "Vars= %a@.PVars= %a@." Vars.pp vars PVars.pp pvars

    let find_block_param id (astate : t) = Vars.find_opt id astate.vars

    let exec_null_check_id id ({vars; pvars} as astate : t) =
      match find_block_param id astate with
      | Some name ->
          let pvars = PVars.add name {checked= true; captured_in_closure= None} pvars in
          {vars; pvars}
      | None ->
          astate


    let load id pvar _ astate =
      let name = Pvar.get_name pvar in
      let vars = Vars.add id name astate.vars in
      {astate with vars}


    let call exp astate =
      let update_captured_in_closure closure_pname astate (_, captured_var) =
        let pvars =
          PVars.update
            (Pvar.get_name captured_var.CapturedVar.pvar)
            (fun data_opt ->
              match data_opt with
              | Some data ->
                  Some {data with captured_in_closure= Some closure_pname}
              | None ->
                  Some {checked= false; captured_in_closure= Some closure_pname} )
            astate.pvars
        in
        {astate with pvars}
      in
      match exp with
      | Exp.Closure {name; captured_vars} ->
          List.fold ~f:(update_captured_in_closure name) captured_vars ~init:astate
      | _ ->
          astate
  end

  module Domain = struct
    include AbstractDomain.FiniteSet (Mem)

    let exec_null_check_id id astate = map (Mem.exec_null_check_id id) astate

    let load id pvar loc astate = map (Mem.load id pvar loc) astate

    let call args astate = map (Mem.call args) astate
  end

  module TransferFunctions = struct
    module Domain = Domain
    module CFG = ProcCfg.Normal

    type analysis_data = Procdesc.t

    let pp_session_name _node fmt = F.pp_print_string fmt "ComputeCapturedInfo - CheckedForNull"

    let exec_instr (astate : Domain.t) _proc_desc _cfg_node _ (instr : Sil.instr) =
      match instr with
      | Load {id; e= Lvar pvar; loc} ->
          Domain.load id pvar loc astate
      | Prune (Var id, _, _, _) ->
          Domain.exec_null_check_id id astate
      (* If (block != nil) or equivalent else branch *)
      | Prune (BinOp (Binop.Ne, e1, e2), _, _, _)
      (* If (!(block == nil)) or equivalent else branch *)
      | Prune (UnOp (LNot, BinOp (Binop.Eq, e1, e2), _), _, _, _) -> (
        match (e1, e2) with
        | Var id, e when Exp.is_null_literal e ->
            Domain.exec_null_check_id id astate
        | e, Var id when Exp.is_null_literal e ->
            Domain.exec_null_check_id id astate
        | _ ->
            astate )
      | Call (_, Exp.Const (Const.Cfun _procname), args, _loc, _call_flags) ->
          List.fold_right ~f:Domain.call (List.map ~f:(fun el -> fst el) args) ~init:astate
      | Call (_, (Exp.Closure _ as exp), _args, _loc, _call_flags) ->
          Domain.call exp astate
      | Store {e2} ->
          Domain.call e2 astate
      | _ ->
          astate
  end

  module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

  let flatten_checked_for_null_domain domain =
    let process_mem mem domain_elements =
      let process_pvar_domain pvar (domain : DomainData.t) domain_elements =
        match domain.captured_in_closure with
        | Some procname when domain.checked ->
            Procname.Map.update procname
              (fun pvars ->
                match pvars with Some pvars -> Some (pvar :: pvars) | None -> Some [pvar] )
              domain_elements
        | _ ->
            domain_elements
      in
      PVars.fold process_pvar_domain mem.Mem.pvars domain_elements
    in
    Domain.fold process_mem domain Procname.Map.empty


  let process_checked_for_null_pvar checked_for_null_data captured_pvar =
    let pvar_name = Pvar.get_name captured_pvar.CapturedVar.pvar in
    let checked = List.exists ~f:(fun pvar -> Mangled.equal pvar pvar_name) checked_for_null_data in
    let context_info =
      match captured_pvar.CapturedVar.context_info with
      | Some context_info ->
          {context_info with CapturedVar.is_checked_for_null= checked}
      | None ->
          {CapturedVar.is_checked_for_null= checked; is_internal_pointer_of= None}
    in
    {captured_pvar with CapturedVar.context_info= Some context_info}
end

module InternalStringPointer = struct
  module VarsDomainData = struct
    type t = {internal_pointer_of: Typ.t; captured_in_closure: Procname.t option}
    [@@deriving compare]

    let pp fmt {internal_pointer_of} =
      F.fprintf fmt "internal_pointer_of=%a" (Typ.pp Pp.text) internal_pointer_of
  end

  module Vars = struct
    include PrettyPrintable.MakePPMonoMap (Ident) (VarsDomainData)

    let compare = compare VarsDomainData.compare
  end

  module PVarsDomainData = struct
    type t = {id: Ident.t; captured_in_closure: Procname.t option} [@@deriving compare]

    let pp fmt {id; captured_in_closure} =
      F.fprintf fmt "id=%a, captured_in_closure=%a" Ident.pp id (Pp.option Procname.pp)
        captured_in_closure
  end

  module PVars = struct
    include PrettyPrintable.MakePPMonoMap (Mangled) (PVarsDomainData)

    let compare = compare PVarsDomainData.compare
  end

  module Mem = struct
    type t = {vars: Vars.t; pvars: PVars.t} [@@deriving compare]

    let pp fmt {vars; pvars} = F.fprintf fmt "Vars= %a@\nPVars= %a@\n" Vars.pp vars PVars.pp pvars

    let set_internal_pointer var typ astate =
      let vars =
        Vars.add var
          {VarsDomainData.internal_pointer_of= typ; captured_in_closure= None}
          astate.vars
      in
      {astate with vars}


    let call exps astate =
      let update_captured_in_closure_in_arg astate (exp, _) =
        let update_captured_in_closure closure_pname astate (_, captured_var) =
          Logging.d_printfln "captured var is %a" CapturedVar.pp captured_var ;
          let pvars =
            PVars.update
              (Pvar.get_name captured_var.CapturedVar.pvar)
              (fun data_opt ->
                Option.value_map
                  ~f:(fun data ->
                    Some {data with PVarsDomainData.captured_in_closure= Some closure_pname} )
                  data_opt ~default:None )
              astate.pvars
          in
          {astate with pvars}
        in
        match exp with
        | Exp.Closure {name; captured_vars} ->
            Logging.d_printfln "closure name is %a" Procname.pp name ;
            List.fold ~f:(update_captured_in_closure name) captured_vars ~init:astate
        | _ ->
            astate
      in
      List.fold ~f:update_captured_in_closure_in_arg exps ~init:astate


    let store pvar id astate =
      let pvars = PVars.add (Pvar.get_name pvar) {id; captured_in_closure= None} astate.pvars in
      {astate with pvars}
  end

  module Domain = struct
    include AbstractDomain.FiniteSet (Mem)

    let set_internal_pointer var typ astate : t = map (Mem.set_internal_pointer var typ) astate

    let store pvar id astate = map (Mem.store pvar id) astate

    let call exps astate = map (Mem.call exps) astate
  end

  module TransferFunctions = struct
    module Domain = Domain
    module CFG = ProcCfg.Normal

    type analysis_data = Procdesc.t

    let pp_session_name _node fmt =
      F.pp_print_string fmt "ComputeCapturedInfo - InternalStringPointer"


    let std_string_c_str_matcher =
      QualifiedCppName.Match.of_fuzzy_qual_names ["std::basic_string::c_str"]


    let cstring_using_encoding =
      QualifiedCppName.Match.of_fuzzy_qual_names ["NSString::cStringUsingEncoding:"]


    let utf8string = QualifiedCppName.Match.of_fuzzy_qual_names ["NSString::UTF8String"]

    let is_local pvar proc_desc =
      let attributes = Procdesc.get_attributes proc_desc in
      let formals = attributes.ProcAttributes.formals in
      let is_formal name =
        List.exists
          ~f:(fun (formal, typ, _) -> Mangled.equal formal name && Typ.is_pointer_to_function typ)
          formals
      in
      (not (Pvar.is_global pvar)) && not (is_formal (Pvar.get_name pvar))


    let exec_instr (astate : Domain.t) proc_desc _cfg_node _ (instr : Sil.instr) =
      match instr with
      | Store {e1= Lvar pvar; e2= Exp.Var id} ->
          Domain.store pvar id astate
      | Call ((var, _), Exp.Const (Const.Cfun procname), [(Lvar pvar, typ)], _, _)
        when QualifiedCppName.Match.match_qualifiers std_string_c_str_matcher
               (Procname.get_qualifiers procname) -> (
        match typ.desc with
        | Tptr (inside_typ, _) ->
            if is_local pvar proc_desc then Domain.set_internal_pointer var inside_typ astate
            else astate
        | _ ->
            astate )
      | Call ((var, _), Exp.Const (Const.Cfun procname), (_, typ) :: _args, _, _)
        when QualifiedCppName.Match.match_qualifiers cstring_using_encoding
               (Procname.get_qualifiers procname)
             || QualifiedCppName.Match.match_qualifiers utf8string
                  (Procname.get_qualifiers procname) -> (
        match typ.desc with
        | Tptr (inside_typ, _) ->
            Domain.set_internal_pointer var inside_typ astate
        | _ ->
            astate )
      | Call (_, _, args, _loc, _call_flags) ->
          Domain.call args astate
      | _ ->
          astate
  end

  module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

  let flatten_is_internal_pointer_domain domain =
    let process_mem mem domain_elements =
      let process_var_domain pvar {PVarsDomainData.id; captured_in_closure} domain_elements =
        match captured_in_closure with
        | Some procname -> (
          match Vars.find_opt id mem.Mem.vars with
          | Some (vars_domain_data : VarsDomainData.t) ->
              Procname.Map.update procname
                (fun pvars ->
                  let tuple = (pvar, vars_domain_data.internal_pointer_of) in
                  match pvars with Some pvars -> Some (tuple :: pvars) | None -> Some [tuple] )
                domain_elements
          | None ->
              domain_elements )
        | _ ->
            domain_elements
      in
      PVars.fold process_var_domain mem.Mem.pvars domain_elements
    in
    Domain.fold process_mem domain Procname.Map.empty


  let process_is_internal_pointer_pvar internal_string_pointer_data captured_pvar =
    let pvar_name = Pvar.get_name captured_pvar.CapturedVar.pvar in
    match
      List.find ~f:(fun (pvar, _) -> Mangled.equal pvar pvar_name) internal_string_pointer_data
    with
    | Some (_, is_internal_pointer_of) ->
        let context_info =
          match captured_pvar.CapturedVar.context_info with
          | Some context_info ->
              {context_info with CapturedVar.is_internal_pointer_of= Some is_internal_pointer_of}
          | None ->
              { CapturedVar.is_internal_pointer_of= Some is_internal_pointer_of
              ; CapturedVar.is_checked_for_null= false }
        in
        {captured_pvar with CapturedVar.context_info= Some context_info}
    | None ->
        captured_pvar
end

let update_captured_context_closures blocks checked_for_null_domain_opt
    internal_string_pointer_domain_opt =
  let update_context_in_closure block attributes procname domain_opt process_pvar =
    match domain_opt with
    | Some domain -> (
      match Procname.Map.find_opt procname domain with
      | Some internal_string_pointer_data ->
          let updated_captured =
            List.map
              ~f:(process_pvar internal_string_pointer_data)
              attributes.ProcAttributes.captured
          in
          let attributes = {attributes with ProcAttributes.captured= updated_captured} in
          Procdesc.set_attributes block attributes
      | None ->
          () )
    | None ->
        ()
  in
  let process_block block =
    let attributes = Procdesc.get_attributes block in
    let procname = Procdesc.get_proc_name block in
    update_context_in_closure block attributes procname checked_for_null_domain_opt
      CheckedForNull.process_checked_for_null_pvar ;
    update_context_in_closure block attributes procname internal_string_pointer_domain_opt
      InternalStringPointer.process_is_internal_pointer_pvar
  in
  List.iter ~f:process_block blocks


let process cfg =
  let process cfg _ proc_desc =
    let initial =
      CheckedForNull.Domain.singleton
        {CheckedForNull.Mem.vars= CheckedForNull.Vars.empty; pvars= CheckedForNull.PVars.empty}
    in
    let checked_for_null_domain =
      CheckedForNull.Analyzer.compute_post proc_desc ~initial proc_desc
    in
    let flat_checked_for_null_domain =
      Option.map
        ~f:(fun map -> CheckedForNull.flatten_checked_for_null_domain map)
        checked_for_null_domain
    in
    let initial =
      InternalStringPointer.Domain.singleton
        { InternalStringPointer.Mem.vars= InternalStringPointer.Vars.empty
        ; pvars= InternalStringPointer.PVars.empty }
    in
    let internal_string_pointer_domain =
      InternalStringPointer.Analyzer.compute_post proc_desc ~initial proc_desc
    in
    let flat_internal_string_pointer_domain =
      Option.map
        ~f:(fun map -> InternalStringPointer.flatten_is_internal_pointer_domain map)
        internal_string_pointer_domain
    in
    let get_blocks cfg =
      Cfg.fold_sorted cfg
        ~f:(fun blocks proc_desc ->
          let procname = Procdesc.get_proc_name proc_desc in
          if Procname.is_objc_block procname then proc_desc :: blocks else blocks )
        ~init:[]
    in
    let blocks = get_blocks cfg in
    update_captured_context_closures blocks flat_checked_for_null_domain
      flat_internal_string_pointer_domain
  in
  Procname.Hash.iter (process cfg) cfg
