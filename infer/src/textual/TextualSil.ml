(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module BaseLocation = Location
module SilConst = Const
module SilExp = Exp
module SilFieldname = Fieldname
module SilIdent = Ident
module SilProcdesc = Procdesc
module SilProcname = Procname
module SilPvar = Pvar
module SilStruct = Struct
module SilTyp = Typ
open Textual

module LocationBridge = struct
  open Location

  let to_sil sourcefile loc =
    let file = SourceFile.file sourcefile in
    match loc with
    | Known {line; col} -> (
      match SourceFile.line_map sourcefile with
      | None ->
          BaseLocation.{line; col; file; macro_file_opt= None; macro_line= -1}
      | Some line_map -> (
          let entry = LineMap.find line_map line in
          match entry with
          | None ->
              BaseLocation.{line; col; file; macro_file_opt= None; macro_line= -1}
          (* hackc doesn't output column information yet *)
          | Some original_line ->
              BaseLocation.{line= original_line; col= -1; file; macro_file_opt= None; macro_line= -1}
          ) )
    | Unknown ->
        BaseLocation.none file


  let of_sil ({line; col} : BaseLocation.t) =
    if Int.(line = -1 && col = -1) then Known {line; col} else Unknown
end

module VarNameBridge = struct
  open VarName

  let of_pvar (lang : Lang.t) (pvar : SilPvar.t) =
    match lang with
    | Java ->
        SilPvar.get_name pvar |> Mangled.to_string |> of_java_name
    | Hack ->
        L.die UserError "of_pvar conversion is not supported in Hack mode"
end

module TypeNameBridge = struct
  open TypeName

  let of_sil (tname : SilTyp.Name.t) =
    match tname with
    | JavaClass name ->
        of_java_name (JavaClassName.to_string name)
    | _ ->
        L.die InternalError "Textual conversion: only Java expected here"


  let of_global_pvar (lang : Lang.t) pvar =
    match lang with
    | Java ->
        SilPvar.get_name pvar |> Mangled.to_string |> of_java_name
    | Hack ->
        L.die UserError "of_global_pvar conversion is not supported in Hack mode"


  let replace_2colons_with_dot str = String.substr_replace_all str ~pattern:"::" ~with_:"."

  let string_to_java_sil string : SilTyp.Name.t =
    JavaClass (replace_2colons_with_dot string |> JavaClassName.from_string)


  let to_sil (lang : Lang.t) {value} : SilTyp.Name.t =
    match lang with
    | Java ->
        string_to_java_sil value
    | Hack ->
        HackClass (HackClassName.make value)


  let java_lang_object = of_java_name "java.lang.Object"

  let hack_mixed = SilTyp.HackClass (HackClassName.make "Mixed")
end

let mangle_java_procname jpname =
  let method_name =
    match Procname.Java.get_method jpname with "<init>" -> "__sil_java_constructor" | s -> s
  in
  let parameter_types = Procname.Java.get_parameters jpname in
  let rec pp_java_typ f ({desc} : SilTyp.t) =
    let string_of_int (i : SilTyp.ikind) =
      match i with
      | IInt ->
          "I"
      | IBool ->
          "B"
      | ISChar ->
          "C"
      | IUShort ->
          "S"
      | ILong ->
          "L"
      | IShort ->
          "S"
      | _ ->
          L.die InternalError "pp_java int"
    in
    let string_of_float (float : SilTyp.fkind) =
      match float with FFloat -> "F" | FDouble -> "D" | _ -> L.die InternalError "pp_java float"
    in
    match desc with
    | Tint ik ->
        F.pp_print_string f (string_of_int ik)
    | Tfloat fk ->
        F.pp_print_string f (string_of_float fk)
    | Tvoid ->
        L.die InternalError "pp_java void"
    | Tptr (typ, _) ->
        pp_java_typ f typ
    | Tstruct (JavaClass java_class_name) ->
        JavaClassName.pp_with_verbosity ~verbose:true f java_class_name
    | Tarray {elt} ->
        F.fprintf f "A__%a" pp_java_typ elt
    | _ ->
        L.die InternalError "pp_java rec"
  in
  let rec pp_java_types fmt l =
    match l with [] -> () | typ :: q -> F.fprintf fmt "_%a%a" pp_java_typ typ pp_java_types q
  in
  F.asprintf "%s%a" method_name pp_java_types parameter_types


module TypBridge = struct
  open Typ

  let rec to_sil lang (typ : t) : SilTyp.t =
    let quals = SilTyp.mk_type_quals () in
    let desc : SilTyp.desc =
      match typ with
      | Int ->
          Tint IInt (* FIXME: add size *)
      | Null ->
          Tint IInt
      | Float ->
          Tfloat FFloat (* FIXME: add size *)
      | Void ->
          Tvoid
      | Ptr t ->
          Tptr (to_sil lang t, Pk_pointer)
      | Struct name ->
          SilTyp.Tstruct (TypeNameBridge.to_sil lang name)
      | Array t ->
          Tarray {elt= to_sil lang t; length= None; stride= None}
    in
    {desc; quals}


  let rec of_sil ({desc} : SilTyp.t) =
    match desc with
    | Tint _ ->
        Int (* TODO: check size and make Textual.Tint size aware *)
    | Tfloat _ ->
        Float
    | Tvoid ->
        Void
    | Tfun ->
        L.die InternalError "Textual conversion: Tfun type does not appear in Java"
    | Tptr (t, Pk_pointer) ->
        Ptr (of_sil t)
    | Tptr (_, _) ->
        L.die InternalError "Textual conversion: this ptr type should not appear in Java"
    | Tstruct name ->
        Struct (TypeNameBridge.of_sil name)
    | TVar _ ->
        L.die InternalError "Textual conversion: TVar type should not appear in Java"
    | Tarray {elt} ->
        Array (of_sil elt)


  let annotated_of_sil typ = of_sil typ |> mk_without_attributes

  let hack_mixed =
    let mixed_struct = SilTyp.mk_struct TypeNameBridge.hack_mixed in
    SilTyp.mk_ptr mixed_struct
end

module IdentBridge = struct
  (* TODO: check the Ident generator is ready *)
  let to_sil id = (SilIdent.create SilIdent.knormal) (Ident.to_int id)

  let of_sil (id : SilIdent.t) =
    if not (SilIdent.is_normal id) then
      L.die InternalError "Textual conversion: onlyt normal ident should appear in Java"
    else SilIdent.get_stamp id |> Ident.of_int
end

module ConstBridge = struct
  open Const

  let to_sil (const : t) : SilConst.t =
    match const with
    | Int z ->
        Cint (IntLit.of_big_int z)
    | Null ->
        Cint IntLit.zero
    | Str s ->
        Cstr s
    | Float f ->
        Cfloat f


  let of_sil (const : SilConst.t) =
    match const with
    | Cint i ->
        Int (IntLit.to_big_int i)
    | Cstr str ->
        Str str
    | Cfloat f ->
        Float f
    | Cfun _ ->
        L.die InternalError
          "Textual conversion: Cfun constant should not appear at this position in Java"
    | Cclass _ ->
        L.die InternalError
          "Textual conversion: class constant is not supported yet (see T127289235)"
end

module ProcDeclBridge = struct
  open ProcDecl

  let of_sil (pname : SilProcname.t) =
    let module Procname = SilProcname in
    match pname with
    | Java jpname ->
        let enclosing_class =
          Enclosing (TypeName.of_java_name (Procname.Java.get_class_name jpname))
        in
        let name = mangle_java_procname jpname |> ProcName.of_java_name in
        let qualified_name : qualified_procname = {enclosing_class; name} in
        let formals_types =
          Procname.Java.get_parameters jpname |> List.map ~f:TypBridge.annotated_of_sil
        in
        let formals_types, (attributes : Attr.t list) =
          if Procname.Java.is_static jpname then (formals_types, [Attr.mk_static])
          else
            let typ = Procname.Java.get_class_type_name jpname in
            let this_type =
              Typ.(Ptr (Struct (TypeNameBridge.of_sil typ))) |> Typ.mk_without_attributes
            in
            (this_type :: formals_types, [])
        in
        let result_type = Procname.Java.get_return_typ jpname |> TypBridge.annotated_of_sil in
        let formals_types = Some formals_types in
        (* FIXME when adding inheritance *)
        {qualified_name; formals_types; result_type; attributes}
    | _ ->
        L.die InternalError "Non-Java procname %a should not appear in Java mode" Procname.describe
          pname


  let to_sil lang t : SilProcname.t =
    let method_name = t.qualified_name.name.ProcName.value in
    match (lang : Lang.t) with
    | Java ->
        let class_name =
          TypeNameBridge.to_sil lang
            ( match t.qualified_name.enclosing_class with
            | TopLevel ->
                TypeName.of_java_name "$TOPLEVEL$CLASS$"
            | Enclosing tname ->
                tname )
        in
        let result_type = TypBridge.to_sil lang t.result_type.typ in
        let return_type = Some result_type in
        let formals_types =
          ProcDecl.formals_or_die t ~context:"to_sil: Java formals must be defined"
        in
        let formals_types =
          List.map ~f:(fun ({typ} : Typ.annotated) -> TypBridge.to_sil lang typ) formals_types
        in
        let kind = SilProcname.Java.Non_Static (* FIXME when handling inheritance *) in
        SilProcname.make_java ~class_name ~return_type ~method_name ~parameters:formals_types ~kind
    | Hack ->
        let class_name =
          match t.qualified_name.enclosing_class with
          | TopLevel ->
              None
          | Enclosing name ->
              Some name.value
        in
        SilProcname.make_hack ~class_name ~function_name:method_name
end

module GlobalBridge = struct
  open Global

  let to_sil {name} =
    let mangled = Mangled.from_string name.value in
    SilPvar.mk_global mangled
end

module FieldDeclBridge = struct
  open FieldDecl

  let to_sil lang {qualified_name} =
    SilFieldname.make
      (TypeNameBridge.to_sil lang qualified_name.enclosing_class)
      qualified_name.name.value


  let of_sil f typ is_final =
    let name = SilFieldname.get_field_name f |> FieldName.of_java_name in
    let enclosing_class = SilFieldname.get_class_name f |> TypeNameBridge.of_sil in
    let qualified_name : qualified_fieldname = {name; enclosing_class} in
    let attributes = if is_final then [Attr.mk_final] else [] in
    {qualified_name; typ; attributes}
end

module StructBridge = struct
  open Struct

  let to_sil lang tenv {name; supers; fields} =
    let name = TypeNameBridge.to_sil lang name in
    let supers = List.map supers ~f:(TypeNameBridge.to_sil lang) in
    let fields =
      List.map fields ~f:(fun (fdecl : FieldDecl.t) ->
          (FieldDeclBridge.to_sil lang fdecl, TypBridge.to_sil lang fdecl.typ, Annot.Item.empty) )
    in
    (* FIXME: generate static fields *)
    Tenv.mk_struct tenv ~fields ~supers name |> ignore


  let of_sil name (sil_struct : SilStruct.t) =
    let of_sil_field (fieldname, typ, annots) =
      let typ = TypBridge.of_sil typ in
      FieldDeclBridge.of_sil fieldname typ (Annot.Item.is_final annots)
    in
    let supers = sil_struct.supers |> List.map ~f:TypeNameBridge.of_sil in
    let fields = SilStruct.(sil_struct.fields @ sil_struct.statics) in
    let fields = List.map ~f:of_sil_field fields in
    {name; supers; fields; attributes= []}
end

module ExpBridge = struct
  open Exp

  let declare_struct_from_tenv decls tenv tname =
    match TextualDecls.get_struct decls tname with
    | Some _ ->
        ()
    | None -> (
        let sil_tname = TypeNameBridge.to_sil Lang.Java tname in
        (* FIXME make it not Java-specific *)
        match Tenv.lookup tenv sil_tname with
        | None ->
            L.die InternalError "Java type %a not found in type environment" SilTyp.Name.pp
              sil_tname
        | Some struct_ ->
            StructBridge.of_sil tname struct_ |> TextualDecls.declare_struct decls )


  let rec of_sil decls tenv (e : SilExp.t) =
    match e with
    | Var id ->
        Var (IdentBridge.of_sil id)
    | UnOp (o, e, _) ->
        let pname = ProcDecl.of_unop o in
        call_non_virtual pname [of_sil decls tenv e]
    | BinOp (o, e1, e2) ->
        let pname = ProcDecl.of_binop o in
        call_non_virtual pname [of_sil decls tenv e1; of_sil decls tenv e2]
    | Exn _ ->
        L.die InternalError "Exp Exn translation not supported"
    | Closure _ ->
        L.die InternalError "Exp Closure translation not supported"
    | Const c ->
        Const (ConstBridge.of_sil c)
    | Cast (typ, e) ->
        cast (TypBridge.of_sil typ) (of_sil decls tenv e)
    | Lvar pvar ->
        let name = VarNameBridge.of_pvar Lang.Java pvar in
        ( if SilPvar.is_global pvar then
          let typ : Typ.t = Struct (TypeNameBridge.of_global_pvar Lang.Java pvar) in
          let global : Global.t = {name; typ; attributes= []} in
          TextualDecls.declare_global decls global ) ;
        Lvar name
    | Lfield (e, f, typ) ->
        let typ = TypBridge.of_sil typ in
        let fielddecl = FieldDeclBridge.of_sil f typ false in
        let () = declare_struct_from_tenv decls tenv fielddecl.qualified_name.enclosing_class in
        Field {exp= of_sil decls tenv e; field= fielddecl.qualified_name}
    | Lindex (e1, e2) ->
        Index (of_sil decls tenv e1, of_sil decls tenv e2)
    | Sizeof _ ->
        L.die InternalError "Sizeof expression should note appear here, please report"


  let to_sil lang decls_env procname exp =
    let rec aux e : SilExp.t =
      match e with
      | Var id ->
          Var (IdentBridge.to_sil id)
      | Lvar name ->
          let pvar : SilPvar.t =
            match TextualDecls.get_global decls_env name with
            | Some global ->
                GlobalBridge.to_sil global
            | None ->
                let mangled = Mangled.from_string name.value in
                let pname = ProcDeclBridge.to_sil lang procname in
                SilPvar.mk mangled pname
          in
          Lvar pvar
      | Field {exp; field} -> (
        match TextualDecls.get_fielddecl decls_env field with
        | None ->
            L.die InternalError "field %a.%a has not been declared" TypeName.pp
              field.enclosing_class FieldName.pp field.name
        | Some field ->
            Lfield (aux exp, FieldDeclBridge.to_sil lang field, TypBridge.to_sil lang field.typ) )
      | Index (exp1, exp2) ->
          Lindex (aux exp1, aux exp2)
      | Const const ->
          Const (ConstBridge.to_sil const)
      | Call {proc; args= [Typ typ; exp]} when ProcDecl.is_cast_builtin proc ->
          Cast (TypBridge.to_sil lang typ, aux exp)
      | Call {proc; args} -> (
        match
          ( TextualDecls.get_procname decls_env proc
          , ProcDecl.to_unop proc
          , ProcDecl.to_binop proc
          , args )
        with
        | Some _, None, None, _ ->
            (* TODO(arr): expression locations *)
            let loc = Location.Unknown in
            let msg = lazy (F.asprintf "%a contains a call inside a sub-expression" pp exp) in
            raise (TextualTransformError [{loc; msg}])
        | None, Some unop, None, [exp] ->
            UnOp (unop, aux exp, Some (TypBridge.to_sil lang Int)) (* FIXME: fix the typ *)
        | None, None, Some binop, [exp1; exp2] ->
            BinOp (binop, aux exp1, aux exp2)
        | _, _, _, _ ->
            L.die InternalError "Internal error: procname %a has an unexpected property"
              pp_qualified_procname proc
            (* FIXME: transform instruction to put call at head of expressions *) )
      | Typ _ ->
          L.die InternalError "Internal error: type expressions should not appear outside builtins"
    in
    aux exp
end

module InstrBridge = struct
  open Instr

  let of_sil decls tenv (i : Sil.instr) =
    match i with
    | Load {id; e; typ} ->
        let id = IdentBridge.of_sil id in
        let exp = ExpBridge.of_sil decls tenv e in
        let typ = TypBridge.of_sil typ in
        let loc = Location.Unknown in
        Load {id; exp; typ; loc}
    | Store {e1; typ; e2} ->
        let exp1 = ExpBridge.of_sil decls tenv e1 in
        let typ = TypBridge.of_sil typ in
        let exp2 = ExpBridge.of_sil decls tenv e2 in
        let loc = Location.Unknown in
        Store {exp1; typ; exp2; loc}
    | Prune (e, _, _, _) ->
        Prune {exp= ExpBridge.of_sil decls tenv e; loc= Location.Unknown}
    | Call ((id, _), Const (Cfun pname), (SilExp.Sizeof {typ= {desc= Tstruct name}}, _) :: _, _, _)
      when String.equal (SilProcname.to_simplified_string pname) "__new()" ->
        let typ = Typ.Struct (TypeNameBridge.of_sil name) in
        Let
          { id= IdentBridge.of_sil id
          ; exp= Exp.call_non_virtual ProcDecl.allocate_object_name [Typ typ]
          ; loc= Location.Unknown }
    | Call
        ((id, _), Const (Cfun pname), (SilExp.Sizeof {typ; dynamic_length= Some exp}, _) :: _, _, _)
      when String.equal (SilProcname.to_simplified_string pname) "__new_array()" ->
        let typ = TypBridge.of_sil typ in
        Let
          { id= IdentBridge.of_sil id
          ; exp=
              Exp.call_non_virtual ProcDecl.allocate_array_name
                [Typ typ; ExpBridge.of_sil decls tenv exp]
          ; loc= Location.Unknown }
    | Call ((id, _), Const (Cfun pname), args, _, call_flags) ->
        let procdecl = ProcDeclBridge.of_sil pname in
        let () = TextualDecls.declare_proc decls ~is_implemented:false procdecl in
        let proc = procdecl.qualified_name in
        let args = List.map ~f:(fun (e, _) -> ExpBridge.of_sil decls tenv e) args in
        let loc = Location.Unknown in
        let kind = if call_flags.cf_virtual then Exp.Virtual else Exp.NonVirtual in
        Let {id= IdentBridge.of_sil id; exp= Call {proc; args; kind}; loc}
    | Call _ ->
        L.die InternalError "Translation of a SIL call that is not const not supported"
    | Metadata _ ->
        L.die InternalError "Translation of a metadata instructions not supported"


  let to_sil lang decls_env procname i : Sil.instr =
    let sourcefile = TextualDecls.source_file decls_env in
    match i with
    | Load {id; exp; typ; loc} ->
        let typ = TypBridge.to_sil lang typ in
        let id = IdentBridge.to_sil id in
        let e = ExpBridge.to_sil lang decls_env procname exp in
        let loc = LocationBridge.to_sil sourcefile loc in
        Load {id; e; typ; loc}
    | Store {exp1; typ; exp2; loc} ->
        let e1 = ExpBridge.to_sil lang decls_env procname exp1 in
        let typ = TypBridge.to_sil lang typ in
        let e2 = ExpBridge.to_sil lang decls_env procname exp2 in
        let loc = LocationBridge.to_sil sourcefile loc in
        Store {e1; typ; e2; loc}
    | Prune {exp; loc} ->
        let e = ExpBridge.to_sil lang decls_env procname exp in
        let loc = LocationBridge.to_sil sourcefile loc in
        Prune (e, loc, true, Ik_if {terminated= false})
    | Let {id; exp= Call {proc; args= [Typ typ]}; loc} when ProcDecl.is_allocate_object_builtin proc
      ->
        let typ = TypBridge.to_sil lang typ in
        let sizeof =
          SilExp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
        in
        let class_type = SilTyp.mk_ptr typ in
        let args = [(sizeof, class_type)] in
        let ret = IdentBridge.to_sil id in
        let loc = LocationBridge.to_sil sourcefile loc in
        let builtin_new = SilExp.Const (SilConst.Cfun BuiltinDecl.__new) in
        Call ((ret, class_type), builtin_new, args, loc, CallFlags.default)
    | Let {id; exp= Call {proc; args= Typ element_typ :: exp :: _}; loc}
      when ProcDecl.is_allocate_array_builtin proc ->
        let element_typ = TypBridge.to_sil lang element_typ in
        let typ = SilTyp.mk_array element_typ in
        let e = ExpBridge.to_sil lang decls_env procname exp in
        let sizeof =
          SilExp.Sizeof {typ; nbytes= None; dynamic_length= Some e; subtype= Subtype.exact}
        in
        (* TODO(T133560394): check if we need to remove Array constructors in the type typ *)
        let class_type = SilTyp.mk_ptr typ in
        let args = [(sizeof, class_type)] in
        let ret = IdentBridge.to_sil id in
        let loc = LocationBridge.to_sil sourcefile loc in
        let builtin_new = SilExp.Const (SilConst.Cfun BuiltinDecl.__new_array) in
        Call ((ret, class_type), builtin_new, args, loc, CallFlags.default)
    | Let {id; exp= Call {proc; args; kind}; loc} ->
        let ret = IdentBridge.to_sil id in
        let procname =
          match TextualDecls.get_procname decls_env proc with
          | Some procname ->
              procname
          | None ->
              let msg =
                lazy (F.asprintf "the expression in %a should start with a regular call" pp i)
              in
              raise (TextualTransformError [{loc; msg}])
        in
        let pname = ProcDeclBridge.to_sil lang procname in
        let result_type = TypBridge.to_sil lang procname.result_type.typ in
        let args = List.map ~f:(ExpBridge.to_sil lang decls_env procname) args in
        let formals_types =
          match procname.formals_types with
          | Some formals_types ->
              List.map formals_types ~f:(fun ({typ} : Typ.annotated) -> TypBridge.to_sil lang typ)
          | _ -> (
            match lang with
            | Lang.Hack ->
                (* Declarations with unknown formals are expected in Hack. Assume that formal types
                   are *Mixed and their number matches that of the arguments. *)
                List.map args ~f:(fun _ -> TypBridge.hack_mixed)
            | other ->
                L.die InternalError "Unexpected unknown formals outside of Hack: %s"
                  (Lang.to_string other) )
        in
        let args =
          match List.zip args formals_types with
          | Ok l ->
              l
          | _ ->
              L.die UserError "the call %a has been given a wrong number of arguments" pp i
        in
        let loc = LocationBridge.to_sil sourcefile loc in
        let cf_virtual = Exp.equal_call_kind kind Virtual in
        let cflag = {CallFlags.default with cf_virtual} in
        Call ((ret, result_type), Const (Cfun pname), args, loc, cflag)
    | Let {loc; _} ->
        let msg = lazy (F.asprintf "the expression in %a should start with a regular call" pp i) in
        raise (TextualTransformError [{loc; msg}])
end

let instr_is_return = function Sil.Store {e1= Lvar v} -> SilPvar.is_return v | _ -> false

module TerminatorBridge = struct
  open Terminator

  let to_sil lang decls_env procname pdesc loc (t : t) : Sil.instr option =
    match t with
    | Ret exp ->
        let ret_var = SilPvar.get_ret_pvar (ProcDeclBridge.to_sil lang procname) in
        let ret_type = SilProcdesc.get_ret_type pdesc in
        let e2 = ExpBridge.to_sil lang decls_env procname exp in
        Some (Sil.Store {e1= SilExp.Lvar ret_var; typ= ret_type; e2; loc})
    | Jump _ ->
        None
    | Throw _ ->
        None (* TODO (T132392184) *)
    | Unreachable ->
        None


  let of_sil decls tenv label_of_node ~opt_last succs =
    match opt_last with
    | None ->
        Jump (List.map ~f:(fun n -> {label= label_of_node n; ssa_args= []}) succs)
    | Some (Sil.Store {e2} as instr) when instr_is_return instr ->
        Ret (ExpBridge.of_sil decls tenv e2)
    | Some sil_instr ->
        let pp_sil_instr fmt instr = Sil.pp_instr ~print_types:false Pp.text fmt instr in
        L.die InternalError "Unexpected instruction %a at end of block" pp_sil_instr sil_instr
end

module NodeBridge = struct
  open Node

  let to_sil lang decls_env procname pdesc node =
    ( if not (List.is_empty node.ssa_parameters) then
      let msg = lazy (F.asprintf "node %a should not have SSA parameters" NodeName.pp node.label) in
      raise (TextualTransformError [{loc= node.label_loc; msg}]) ) ;
    let instrs = List.map ~f:(InstrBridge.to_sil lang decls_env procname) node.instrs in
    let sourcefile = TextualDecls.source_file decls_env in
    let last_loc = LocationBridge.to_sil sourcefile node.last_loc in
    let last = TerminatorBridge.to_sil lang decls_env procname pdesc last_loc node.last in
    let instrs = Option.value_map ~default:instrs ~f:(fun instr -> instrs @ [instr]) last in
    let loc = LocationBridge.to_sil sourcefile node.label_loc in
    let nkind = SilProcdesc.Node.Stmt_node MethodBody in
    SilProcdesc.create_node pdesc loc nkind instrs


  let of_sil decls tenv label_of_node node =
    let module Node = SilProcdesc.Node in
    let label = label_of_node node in
    let exn_succs = Node.get_exn node |> List.map ~f:label_of_node in
    let instrs = Node.get_instrs node |> Instrs.get_underlying_not_reversed in
    let opt_last =
      if Array.is_empty instrs then None
      else
        let last = Array.last instrs in
        if instr_is_return last then Some last else None
    in
    let rec backward_iter i acc =
      if i < 0 then acc
      else
        let instr = InstrBridge.of_sil decls tenv instrs.(i) in
        backward_iter (i - 1) (instr :: acc)
    in
    let instrs_length = Array.length instrs in
    let instrs =
      match opt_last with
      | None ->
          backward_iter (instrs_length - 1) []
      | Some _ ->
          backward_iter (instrs_length - 2) []
    in
    let last = TerminatorBridge.of_sil decls tenv label_of_node ~opt_last (Node.get_succs node) in
    let last_loc = Node.get_last_loc node |> LocationBridge.of_sil in
    let label_loc = Node.get_loc node |> LocationBridge.of_sil in
    {label; ssa_parameters= []; exn_succs; last; instrs; last_loc; label_loc}
end

module ProcDescBridge = struct
  open ProcDesc

  let build_formals lang ({procdecl; params} as procdesc) =
    let mk_formal ({typ} : Typ.annotated) vname =
      let name = Mangled.from_string vname.VarName.value in
      let typ = TypBridge.to_sil lang typ in
      (name, typ, Annot.Item.empty)
    in
    match List.map2 (ProcDesc.formals procdesc) params ~f:mk_formal with
    | Ok l ->
        l
    | Unequal_lengths ->
        L.die InternalError "procname %a has not the same number of arg names and arg types"
          pp_qualified_procname procdecl.qualified_name


  let build_locals lang {locals} =
    let make_var_data name typ : ProcAttributes.var_data =
      {name; typ; modify_in_block= false; is_constexpr= false; is_declared_unused= false}
    in
    List.map locals ~f:(fun (var, annotated_typ) ->
        let name = Mangled.from_string var.VarName.value in
        let typ = TypBridge.to_sil lang annotated_typ.Typ.typ in
        make_var_data name typ )


  let to_sil lang decls_env cfgs ({procdecl; nodes; start; exit_loc} as pdesc) =
    let sourcefile = TextualDecls.source_file decls_env in
    let sil_procname = ProcDeclBridge.to_sil lang procdecl in
    let sil_ret_type = TypBridge.to_sil lang procdecl.result_type.typ in
    let definition_loc = LocationBridge.to_sil sourcefile procdecl.qualified_name.name.loc in
    let formals = build_formals lang pdesc in
    let locals = build_locals lang pdesc in
    let pattributes =
      { (ProcAttributes.default (SourceFile.file sourcefile) sil_procname) with
        is_defined= true
      ; formals
      ; locals
      ; ret_type= sil_ret_type
      ; loc= definition_loc }
    in
    let pdesc = Cfg.create_proc_desc cfgs pattributes in
    (* Create standalone start and end nodes. Note that SIL start node does not correspond to the start node in
       Textual. The latter is more like a _first node_. *)
    let module P = SilProcdesc in
    let start_node = P.create_node pdesc definition_loc P.Node.Start_node [] in
    P.set_start_node pdesc start_node ;
    let exit_loc = LocationBridge.to_sil sourcefile exit_loc in
    let exit_node = P.create_node pdesc exit_loc P.Node.Exit_node [] in
    P.set_exit_node pdesc exit_node ;
    (* FIXME: special exit nodes should be added *)
    let node_map : (string, Node.t * P.Node.t) Hashtbl.t = Hashtbl.create 17 in
    List.iter nodes ~f:(fun node ->
        let data = (node, NodeBridge.to_sil lang decls_env procdecl pdesc node) in
        let key = node.Node.label.value in
        Hashtbl.replace node_map key data |> ignore ) ;
    ( match Hashtbl.find_opt node_map start.value with
    | Some (_, first_node) ->
        P.node_set_succs pdesc start_node ~normal:[first_node] ~exn:[]
    | None ->
        L.die InternalError "start node %a npt found" NodeName.pp start ) ;
    (* TODO: register this exit node *)
    let normal_succ : Terminator.t -> P.Node.t list = function
      | Ret _ ->
          [exit_node]
      | Jump l ->
          List.map
            ~f:(fun ({label} : Terminator.node_call) -> Hashtbl.find node_map label.value |> snd)
            l
      | Throw _ ->
          L.die InternalError "TODO: implement throw"
      | Unreachable ->
          []
    in
    Hashtbl.iter
      (fun _ ((node : Node.t), sil_node) ->
        let normal = normal_succ node.last in
        let exn : P.Node.t list =
          List.map ~f:(fun name -> Hashtbl.find node_map name.NodeName.value |> snd) node.exn_succs
        in
        P.node_set_succs pdesc sil_node ~normal ~exn )
      node_map


  let make_label_of_node () =
    let open SilProcdesc in
    let tbl = NodeHash.create 17 in
    let count = ref 0 in
    fun node ->
      match NodeHash.find_opt tbl node with
      | Some lbl ->
          lbl
      | None ->
          let value = "node_" ^ string_of_int !count in
          let res : NodeName.t = {value; loc= Location.Unknown} in
          NodeHash.add tbl node res ;
          incr count ;
          res


  let compute_java_locals_type (nodes : Node.t list) =
    List.fold nodes ~init:VarName.Map.empty ~f:(fun init (node : Node.t) ->
        List.fold node.instrs ~init ~f:(fun map (instr : Instr.t) ->
            match instr with
            | Store {exp1= Lvar var; typ} ->
                VarName.Map.add var typ map
            | Load _ | Store _ | Prune _ | Let _ ->
                map ) )


  let of_sil decls tenv pdesc =
    let module P = SilProcdesc in
    let procdecl = P.get_proc_name pdesc |> ProcDeclBridge.of_sil in
    let node_of_sil = make_label_of_node () |> NodeBridge.of_sil decls tenv in
    let start_node = P.get_start_node pdesc |> node_of_sil in
    let nodes = List.map (P.get_nodes pdesc) ~f:node_of_sil in
    let nodes = List.rev nodes in
    let nodes =
      match nodes with
      | head :: _ when Node.equal head start_node ->
          nodes
      | _ ->
          L.die InternalError "the start node is not in head"
      (* FIXME: this is fine with the current Java frontend, but we could make that more robust *)
    in
    let start = start_node.label in
    let params =
      List.map (P.get_pvar_formals pdesc) ~f:(fun (pvar, _) -> VarNameBridge.of_pvar Lang.Java pvar)
    in
    let java_locals_type = compute_java_locals_type nodes in
    let locals =
      P.get_locals pdesc
      |> List.map ~f:(fun ({name; typ} : ProcAttributes.var_data) ->
             let var = Mangled.to_string name |> VarName.of_java_name in
             let typ =
               if SilTyp.is_void typ then
                 (* the Java frontend gives the void type to some local variables, but it does
                    not makes sense. But this should be only the case for variable that are
                    assigned once in the function, so we can easily collect the corresponding
                    type with the function [compute_java_locals_type] above. *)
                 VarName.Map.find_opt var java_locals_type
                 |> Option.value ~default:Typ.(Ptr (Struct TypeNameBridge.java_lang_object))
               else TypBridge.of_sil typ
             in
             (var, Typ.mk_without_attributes typ) )
    in
    let exit_loc = Location.Unknown in
    {procdecl; nodes; start; params; locals; exit_loc}
end

module ModuleBridge = struct
  open Module

  let to_sil module_ =
    match lang module_ with
    | None ->
        raise
          (TextualTransformError
             [{loc= Location.Unknown; msg= lazy "Missing or unsupported source_language attribute"}]
          )
    | Some lang ->
        let errors, decls_env = TextualDecls.make_decls module_ in
        if not (List.is_empty errors) then
          L.die InternalError
            "to_sil conversion should not be performed if TextualDecls verification has raised any \
             errors before." ;
        let module_ =
          let open TextualTransform in
          module_ |> remove_internal_calls |> let_propagation |> out_of_ssa
        in
        let cfgs = Cfg.create () in
        let tenv = Tenv.create () in
        List.iter module_.decls ~f:(fun decl ->
            match decl with
            | Global _ ->
                ()
            | Struct strct ->
                StructBridge.to_sil lang tenv strct
            | Procdecl _ ->
                ()
            | Proc pdesc ->
                if not (ProcDesc.is_ready_for_to_sil_conversion pdesc) then
                  (* we only run SSA verification if the to_sil conversion  needs
                     extra transformation, because some .sil files that are generated by
                     Java examples are not in SSA *)
                  SsaVerification.run pdesc ;
                ProcDescBridge.to_sil lang decls_env cfgs pdesc ) ;
        (cfgs, tenv)


  let of_sil ~sourcefile ~lang tenv cfg =
    let env = TextualDecls.init sourcefile in
    let decls =
      Cfg.fold_sorted cfg ~init:[] ~f:(fun decls pdesc ->
          let textual_pdesc = ProcDescBridge.of_sil env tenv pdesc in
          Proc textual_pdesc :: decls )
    in
    let decls =
      TextualDecls.fold_globals env ~init:decls ~f:(fun decls _ pvar -> Global pvar :: decls)
    in
    let decls =
      TextualDecls.fold_structs env ~init:decls ~f:(fun decls _ struct_ -> Struct struct_ :: decls)
    in
    let decls =
      TextualDecls.fold_procnames env ~init:decls ~f:(fun decls procname ->
          Procdecl procname :: decls )
    in
    let attrs = [Attr.mk_source_language lang] in
    {attrs; decls; sourcefile}
end

let proc_decl_to_sil = ProcDeclBridge.to_sil

let module_to_sil = ModuleBridge.to_sil

let pp_copyright fmt =
  F.fprintf fmt "// \n" ;
  F.fprintf fmt "// Copyright (c) Facebook, Inc. and its affiliates.\n" ;
  F.fprintf fmt "// \n" ;
  F.fprintf fmt "// This source code is licensed under the MIT license found in the\n" ;
  F.fprintf fmt "// LICENSE file in the root directory of this source tree.\n" ;
  F.fprintf fmt "//\n\n"


let from_java ~filename tenv cfg =
  Utils.with_file_out filename ~f:(fun oc ->
      let fmt = F.formatter_of_out_channel oc in
      let sourcefile = SourceFile.create filename in
      pp_copyright fmt ;
      Module.pp fmt (ModuleBridge.of_sil ~sourcefile ~lang:Java tenv cfg) ;
      Format.pp_print_flush fmt () )
