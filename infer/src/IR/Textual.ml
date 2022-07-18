(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Location = struct
  type t = {line: int; col: int}

  let pp fmt {line; col} = F.fprintf fmt "line %d, column %d" line col
end

module type NAME = sig
  type t = {value: string; loc: Location.t}
end

(* this signature will not be exported in .mli *)
module type COMMON_NAME = sig
  include NAME

  val equal : t -> t -> bool

  val pp : F.formatter -> t -> unit
end

module Name : COMMON_NAME = struct
  type t = {value: string; loc: Location.t}

  let equal name1 name2 = String.equal name1.value name2.value

  let pp fmt name = F.pp_print_string fmt name.value
end

module ProcBaseName : COMMON_NAME = Name

module FieldBaseName : COMMON_NAME = Name

module TypeName : COMMON_NAME = Name

module VarName : COMMON_NAME = Name

module NodeName : COMMON_NAME = Name

module Typ = struct
  type t = Tint | Tfloat | Tnull | Tvoid | Tptr of t | Tstruct of TypeName.t | Tarray of t
end

module Ident = struct
  type t = int

  let of_int x = x
end

module Const = struct
  type t = Cint of Z.t | Cnull | Cstr of string | Cfloat of float
end

module Procname = struct
  type proc_kind = Virtual | NonVirtual

  type kind = Builtin | SilInstr | Proc of {pkind: proc_kind}

  type t = {name: ProcBaseName.t; targs: Typ.t list; tres: Typ.t; kind: kind}
end

module Pvar = struct
  type kind = Global | Local of Procname.t

  type t = {name: VarName.t; kind: kind}
end

module Fieldname = struct
  type t = {name: FieldBaseName.t; typ: Typ.t; enclosing_type: TypeName.t}
end

module Struct = struct
  type t = {name: TypeName.t; fields: Fieldname.t list}
end

module Decls = struct
  (* We do not export this module. We record here each name to a more elaborate object *)

  type t =
    { globals: (string, Pvar.t) Hashtbl.t
    ; labels: (string, (string, unit) Hashtbl.t) Hashtbl.t
    ; procnames: (string, Procname.t) Hashtbl.t
    ; structs: (string, Struct.t) Hashtbl.t }

  let init () =
    { globals= Hashtbl.create (module String)
    ; labels= Hashtbl.create (module String)
    ; procnames= Hashtbl.create (module String)
    ; structs= Hashtbl.create (module String) }


  let declare_global decls (pvar : Pvar.t) =
    let key = pvar.name.value in
    ignore (Hashtbl.add decls.globals ~key ~data:pvar)


  let declare_label decls pname label =
    let tbl =
      match Hashtbl.find decls.labels pname.ProcBaseName.value with
      | Some tbl ->
          tbl
      | None ->
          let tbl = Hashtbl.create (module String) in
          ignore (Hashtbl.add decls.labels ~key:pname.ProcBaseName.value ~data:tbl) ;
          tbl
    in
    ignore (Hashtbl.add tbl ~key:label.NodeName.value ~data:())


  let declare_procname decls (pname : Procname.t) =
    ignore (Hashtbl.add decls.procnames ~key:pname.name.value ~data:pname)


  let declare_struct decls (s : Struct.t) =
    ignore (Hashtbl.add decls.structs ~key:s.name.value ~data:s)


  let is_fieldname_declared decls (tname : TypeName.t) (fname : FieldBaseName.t) =
    match Hashtbl.find decls.structs tname.value with
    | None ->
        false
    | Some s ->
        List.exists s.fields ~f:(fun {Fieldname.name} -> FieldBaseName.equal name fname)


  let is_label_declared decls pname label =
    match Hashtbl.find decls.labels pname.ProcBaseName.value with
    | None ->
        false
    | Some tbl ->
        Hashtbl.mem tbl label.NodeName.value


  let is_procname_declared decls pname = Hashtbl.mem decls.procnames pname.ProcBaseName.value
end

module Exp = struct
  type t =
    | Var of Ident.t
    | Lvar of VarName.t
    | Lfield of {exp: t; tname: TypeName.t; fname: FieldBaseName.t}
    | Lindex of t * t
    (*  | Sizeof of sizeof_data *)
    | EConst of Const.t
    | ECall of ProcBaseName.t * t list
    | ECast of Typ.t * t
end

module Instr = struct
  type t =
    | Load of {id: Ident.t; exp: Exp.t; typ: Typ.t; loc: Location.t}
    | Store of {exp1: Exp.t; typ: Typ.t; exp2: Exp.t; loc: Location.t}
    | Prune of {exp: Exp.t; b: bool; loc: Location.t}
    | Call of {ret: Ident.t; f: ProcBaseName.t; args: Exp.t list; loc: Location.t}
end

module Terminator = struct
  type node_call = {label: NodeName.t; ssa_args: Ident.t list}

  type t = Ret of Exp.t | Jump of node_call list | Throw of Exp.t
end

module Node = struct
  type t =
    { label: NodeName.t
    ; ssa_parameters: Ident.t list
    ; exn_succs: NodeName.t list
    ; last: Terminator.t
    ; instrs: Instr.t list
    ; last_loc: Location.t
    ; label_loc: Location.t }
end

module Procdesc = struct
  type t = {procname: Procname.t; nodes: Node.t list; start: NodeName.t; params: VarName.t list}
end

module Module = struct
  type decl = Global of Pvar.t | Struct of Struct.t | Procname of Procname.t | Proc of Procdesc.t

  type t = {decls: decl list; sourcefile: SourceFile.t}

  let make_decls m =
    let decls = Decls.init () in
    let register decl =
      match decl with
      | Global pvar ->
          Decls.declare_global decls pvar
      | Struct s ->
          Decls.declare_struct decls s
      | Procname pname ->
          Decls.declare_procname decls pname
      | Proc pdesc ->
          let register_label node =
            let label = node.Node.label in
            let pname = pdesc.procname.name in
            Decls.declare_label decls pname label
          in
          List.iter pdesc.nodes ~f:register_label ;
          Decls.declare_procname decls pdesc.procname
    in
    List.iter m.decls ~f:register ;
    decls
end

module Verification = struct
  type error =
    | UnknownFieldname of {tname: TypeName.t; fname: FieldBaseName.t}
    | UnknownProcname of {pname: ProcBaseName.t}
    | UnknownLabel of {label: NodeName.t; pname: ProcBaseName.t}
  (* TODO: check that a name is not declared twice *)
  (* TODO: add basic type verification *)

  let pp_error fmt sourcefile error =
    F.fprintf fmt "SIL consistency error in file %a" SourceFile.pp sourcefile ;
    match error with
    | UnknownFieldname {tname; fname} ->
        F.fprintf fmt ", %a: field %a.%a is not declared\n" Location.pp tname.loc TypeName.pp tname
          FieldBaseName.pp fname
    | UnknownProcname {pname} ->
        F.fprintf fmt ", %a: function %a is not declared\n" Location.pp pname.loc ProcBaseName.pp
          pname
    | UnknownLabel {label; pname} ->
        F.fprintf fmt ", %a: label %a is not declared in function %a\n" Location.pp label.loc
          NodeName.pp label ProcBaseName.pp pname


  let verify_decl ~is_label_declared ~is_fieldname_declared ~is_procname_declared errors
      (decl : Module.decl) =
    let verify_label errors pname label =
      if is_label_declared pname label then errors else UnknownLabel {label; pname} :: errors
    in
    let verify_fieldname errors tname fname =
      if is_fieldname_declared tname fname then errors
      else UnknownFieldname {tname; fname} :: errors
    in
    let verify_procname errors pname =
      if is_procname_declared pname then errors else UnknownProcname {pname} :: errors
    in
    let rec verify_exp errors (e : Exp.t) =
      match e with
      | Var _ | Lvar _ | EConst _ ->
          errors
      | Lfield {exp; tname; fname} ->
          let errors = verify_fieldname errors tname fname in
          verify_exp errors exp
      | Lindex (e1, e2) ->
          let errors = verify_exp errors e1 in
          verify_exp errors e2
      | ECall (f, l) ->
          let errors = verify_procname errors f in
          List.fold ~f:verify_exp ~init:errors l
      | ECast (_, e) ->
          verify_exp errors e
    in
    let verify_instr errors (instr : Instr.t) =
      match instr with
      | Load {exp} ->
          verify_exp errors exp
      | Store {exp1; exp2} ->
          let errors = verify_exp errors exp1 in
          verify_exp errors exp2
      | Prune {exp} ->
          verify_exp errors exp
      | Call {f; args} ->
          let errors = verify_procname errors f in
          List.fold ~f:verify_exp ~init:errors args
    in
    let verify_procdesc errors ({procname; nodes} : Procdesc.t) =
      let verify_label errors = verify_label errors procname.name in
      let verify_terminator errors (t : Terminator.t) =
        match t with
        | Jump l ->
            let f errors {Terminator.label} = verify_label errors label in
            List.fold ~init:errors ~f l
        | Ret e | Throw e ->
            verify_exp errors e
      in
      let verify_node errors ({instrs; last} : Node.t) =
        let errors = List.fold ~f:verify_instr ~init:errors instrs in
        verify_terminator errors last
      in
      List.fold ~f:verify_node ~init:errors nodes
    in
    match decl with
    | Global _ | Struct _ | Procname _ ->
        errors
    | Proc pdesc ->
        verify_procdesc errors pdesc


  let run (m : Module.t) =
    let decls_env = Module.make_decls m in
    let is_label_declared = Decls.is_label_declared decls_env in
    let is_fieldname_declared = Decls.is_fieldname_declared decls_env in
    let is_procname_declared = Decls.is_procname_declared decls_env in
    let f = verify_decl ~is_label_declared ~is_fieldname_declared ~is_procname_declared in
    List.fold ~f ~init:[] m.decls
end
