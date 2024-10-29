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
end

module InternalStringPointer = struct
  module VarsDomainData = struct
    type t = {internal_pointer_of: Typ.t} [@@deriving compare]

    let pp fmt {internal_pointer_of} =
      F.fprintf fmt "internal_pointer_of=%a" (Typ.pp Pp.text) internal_pointer_of
  end

  module Vars = struct
    include PrettyPrintable.MakePPMonoMap (Ident) (VarsDomainData)

    let compare = compare VarsDomainData.compare
  end

  module PVars = struct
    include PrettyPrintable.MakePPMonoMap (Mangled) (Ident)

    let compare = compare Ident.compare
  end

  module Mem = struct
    type t = {vars: Vars.t; pvars: PVars.t} [@@deriving compare]

    let pp fmt {vars; pvars} = F.fprintf fmt "Vars= %a@\nPVars= %a@\n" Vars.pp vars PVars.pp pvars

    let set_internal_pointer var typ astate =
      let vars = Vars.add var {VarsDomainData.internal_pointer_of= typ} astate.vars in
      {astate with vars}


    let is_internal_pointer_of pvar astate =
      match PVars.find_opt pvar astate.pvars with
      | Some var -> (
        match Vars.find_opt var astate.vars with
        | Some domainData ->
            Some domainData.internal_pointer_of
        | None ->
            None )
      | None ->
          None


    let store pvar id astate =
      let pvars = PVars.add (Pvar.get_name pvar) id astate.pvars in
      {astate with pvars}
  end

  module Domain = struct
    include AbstractDomain.FiniteSet (Mem)

    let set_internal_pointer var typ astate : t = map (Mem.set_internal_pointer var typ) astate

    let is_internal_pointer_of pvar astate =
      fold
        (fun state typ_opt ->
          match typ_opt with Some _ -> typ_opt | None -> Mem.is_internal_pointer_of pvar state )
        astate None


    let store pvar id astate = map (Mem.store pvar id) astate
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
      | Call ((var, _), Exp.Const (Const.Cfun procname), (_, typ) :: _args, _, _) ->
          let procname_qualifiers = Procname.get_qualifiers procname in
          if
            QualifiedCppName.Match.match_qualifiers cstring_using_encoding procname_qualifiers
            || QualifiedCppName.Match.match_qualifiers utf8string procname_qualifiers
          then
            match typ.desc with
            | Tptr (inside_typ, _) ->
                Domain.set_internal_pointer var inside_typ astate
            | _ ->
                astate
          else astate
      | _ ->
          astate
  end

  module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)
end

let update_captured_in_closures cfg checked_for_null_domain_opt internal_string_pointer_domain_opt =
  let process_mem {CheckedForNull.Mem.pvars} =
    let process_pvar pvar {CheckedForNull.DomainData.checked; captured_in_closure} =
      match captured_in_closure with
      | Some procname ->
          let proc_desc = Procname.Hash.find cfg procname in
          let attributes = Procdesc.get_attributes proc_desc in
          let update_captured captured_var =
            let pvar_name = Pvar.get_name captured_var.CapturedVar.pvar in
            if Mangled.equal pvar pvar_name then
              let is_internal_pointer_of =
                Option.bind
                  ~f:(fun domain -> InternalStringPointer.Domain.is_internal_pointer_of pvar domain)
                  internal_string_pointer_domain_opt
              in
              let context_info =
                {CapturedVar.is_checked_for_null= checked; is_internal_pointer_of}
              in
              {captured_var with CapturedVar.context_info= Some context_info}
            else captured_var
          in
          let captured = List.map ~f:update_captured attributes.ProcAttributes.captured in
          let attributes = {attributes with captured} in
          Procdesc.set_attributes proc_desc attributes
      | None ->
          ()
    in
    CheckedForNull.PVars.iter process_pvar pvars
  in
  match checked_for_null_domain_opt with
  | Some checked_for_null_domain ->
      CheckedForNull.Domain.iter process_mem checked_for_null_domain
  | None ->
      ()


let process cfg =
  let process cfg _ proc_desc =
    let initial =
      CheckedForNull.Domain.singleton
        {CheckedForNull.Mem.vars= CheckedForNull.Vars.empty; pvars= CheckedForNull.PVars.empty}
    in
    let checked_for_null_domain =
      CheckedForNull.Analyzer.compute_post proc_desc ~initial proc_desc
    in
    let initial =
      InternalStringPointer.Domain.singleton
        { InternalStringPointer.Mem.vars= InternalStringPointer.Vars.empty
        ; pvars= InternalStringPointer.PVars.empty }
    in
    let internal_string_pointer_domain =
      InternalStringPointer.Analyzer.compute_post proc_desc ~initial proc_desc
    in
    update_captured_in_closures cfg checked_for_null_domain internal_string_pointer_domain
  in
  Procname.Hash.iter (process cfg) cfg
