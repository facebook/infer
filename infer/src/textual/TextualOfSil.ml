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

  let of_sil ({line; col} : BaseLocation.t) =
    if Int.(line = -1 && col = -1) then Known {line; col} else Unknown
end

module VarNameBridge = struct
  open VarName

  let of_pvar (lang : Lang.t) (pvar : SilPvar.t) =
    match lang with
    | Java | C ->
        SilPvar.get_name pvar |> Mangled.to_string |> of_string
    | Hack | Python | Rust | Swift ->
        L.die UserError "of_pvar conversion is not supported in %s mode"
          (Textual.Lang.to_string lang)
end

module TypeNameBridge = struct
  open TypeName

  let of_sil (tname : SilTyp.Name.t) =
    match tname with
    | JavaClass name ->
        of_string (JavaClassName.to_string name)
    | CStruct name | CUnion name ->
        of_string (QualifiedCppName.to_qual_string name)
    | CppClass {name} ->
        of_string (QualifiedCppName.to_qual_string name)
    | _ ->
        L.die InternalError "Textual conversion: unsupported type name %a" SilTyp.Name.pp tname


  let of_global_pvar (lang : Lang.t) pvar =
    match lang with
    | Java | C ->
        SilPvar.get_name pvar |> Mangled.to_string |> of_string
    | Hack | Python | Rust | Swift ->
        L.die UserError "of_global_pvar conversion is not supported in %s mode"
          (Textual.Lang.to_string lang)


  let java_lang_object = of_string "java.lang.Object"
end

module TypBridge = struct
  open Typ

  let annots_of_ptr_kind (kind : SilTyp.ptr_kind) =
    match kind with
    | Pk_pointer ->
        []
    | Pk_lvalue_reference ->
        [Attr.ptr_lvalue_reference]
    | Pk_rvalue_reference ->
        [Attr.ptr_rvalue_reference]
    | Pk_objc_weak ->
        [Attr.ptr_objc_weak]
    | Pk_objc_unsafe_unretained ->
        [Attr.ptr_unsafe_unretained]
    | Pk_objc_autoreleasing ->
        [Attr.ptr_autoreleasing]
    | Pk_objc_nonnull_block ->
        [Attr.ptr_nonull]
    | Pk_objc_nullable_block ->
        [Attr.ptr_nullable]


  let rec of_sil ({desc} : SilTyp.t) =
    match desc with
    | Tint _ ->
        Int (* TODO: check size and make Textual.Tint size aware *)
    | Tfloat _ ->
        Float
    | Tvoid ->
        Void
    | Tfun _ ->
        Fun None
    | Tptr (t, kind) ->
        Typ.Ptr (of_sil t, annots_of_ptr_kind kind)
    | Tstruct name ->
        Struct (TypeNameBridge.of_sil name)
    | TVar _ ->
        L.die InternalError "Textual conversion: TVar type not expected"
    | Tarray {elt} ->
        Array (of_sil elt)


  let annotated_of_sil typ = of_sil typ |> mk_without_attributes
end

module IdentBridge = struct
  let of_sil (id : SilIdent.t) =
    if not (SilIdent.is_normal id) then
      L.die InternalError "Textual conversion: only normal ident expected"
    else SilIdent.get_stamp id |> Ident.of_int
end

module ConstBridge = struct
  open Const

  let of_sil (const : SilConst.t) =
    match const with
    | Cint i ->
        Int (IntLit.to_big_int i)
    | Cstr str ->
        Str str
    | Cfloat f ->
        Float f
    | Cfun _ ->
        L.die InternalError "Textual conversion: Cfun constant should not appear at this position"
    | Cclass _ ->
        L.die InternalError "Textual conversion: Cclass constant is not supported yet"
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


module ProcDeclBridge = struct
  open ProcDecl

  let of_sil (pname : SilProcname.t) ret_typ args_typ =
    let module Procname = SilProcname in
    match pname with
    | Java jpname ->
        let enclosing_class =
          QualifiedProcName.Enclosing (TypeName.of_string (Procname.Java.get_class_name jpname))
        in
        let name = mangle_java_procname jpname |> ProcName.of_string in
        let qualified_name : QualifiedProcName.t = {enclosing_class; name} in
        let formals_types =
          Procname.Java.get_parameters jpname |> List.map ~f:TypBridge.annotated_of_sil
        in
        let formals_types, (attributes : Attr.t list) =
          if Procname.Java.is_static jpname then (formals_types, [Attr.mk_static])
          else
            let typ = Procname.Java.get_class_type_name jpname in
            let this_type =
              Typ.(mk_ptr (Struct (TypeNameBridge.of_sil typ))) |> Typ.mk_without_attributes
            in
            (this_type :: formals_types, [])
        in
        let result_type = Procname.Java.get_return_typ jpname |> TypBridge.annotated_of_sil in
        (* FIXME when adding inheritance *)
        {qualified_name; formals_types= Some formals_types; result_type; attributes}
    | C {c_name} ->
        let name = QualifiedCppName.to_qual_string c_name |> ProcName.of_string in
        let qualified_name : QualifiedProcName.t = {enclosing_class= TopLevel; name} in
        { qualified_name
        ; formals_types= Some (List.map ~f:TypBridge.annotated_of_sil args_typ)
        ; result_type= TypBridge.annotated_of_sil ret_typ
        ; attributes= [] }
    | _ ->
        L.die InternalError "Unsupported procname %a in of_sil conversion" Procname.describe pname
end

module FieldDeclBridge = struct
  open FieldDecl

  let of_sil f typ is_final =
    let name = SilFieldname.get_field_name f |> FieldName.of_string in
    let enclosing_class = SilFieldname.get_class_name f |> TypeNameBridge.of_sil in
    let qualified_name : qualified_fieldname = {name; enclosing_class} in
    let attributes = if is_final then [Attr.mk_final] else [] in
    {qualified_name; typ; attributes}
end

module StructBridge = struct
  open Struct

  let of_hack_class_info class_info =
    match (class_info : SilStruct.ClassInfo.t) with
    | HackClassInfo Trait ->
        Some Textual.Attr.mk_trait
    | _ ->
        None


  let of_sil name (sil_struct : SilStruct.t) =
    let of_sil_field {SilStruct.name= fieldname; typ; annot} =
      let typ = TypBridge.of_sil typ in
      FieldDeclBridge.of_sil fieldname typ (Annot.Item.is_final annot)
    in
    let supers = sil_struct.supers |> List.map ~f:TypeNameBridge.of_sil in
    let fields = SilStruct.(sil_struct.fields @ sil_struct.statics) in
    let fields = List.map ~f:of_sil_field fields in
    let attributes = of_hack_class_info sil_struct.class_info |> Option.to_list in
    {name; supers; fields; attributes}
end

module ExpBridge = struct
  open Exp

  (** Convert a Textual TypeName to a SIL type name. Inlined to avoid circular dependency with
      TextualSil.TypeNameBridge.to_sil *)
  let typename_to_sil (lang : Lang.t) (tname : TypeName.t) : SilTyp.Name.t =
    let value = tname.name.value in
    match lang with
    | Java ->
        JavaClass
          (String.substr_replace_all value ~pattern:"::" ~with_:"." |> JavaClassName.from_string)
    | C ->
        SilTyp.Name.C.from_string value
    | _ ->
        L.die InternalError "typename_to_sil not supported for %s" (Lang.to_string lang)


  let declare_struct_from_tenv lang decls tenv tname =
    match TextualDecls.get_struct decls tname with
    | Some _ ->
        ()
    | None -> (
        let sil_tname = typename_to_sil lang tname in
        match Tenv.lookup tenv sil_tname with
        | None ->
            L.die InternalError "type %a not found in type environment" SilTyp.Name.pp sil_tname
        | Some struct_ ->
            StructBridge.of_sil tname struct_ |> TextualDecls.declare_struct decls )


  let rec of_sil decls tenv (e : SilExp.t) =
    let lang = TextualDecls.lang decls in
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
        let name = VarNameBridge.of_pvar lang pvar in
        ( if SilPvar.is_global pvar then
            let typ : Typ.t =
              match lang with
              | Java ->
                  Struct (TypeNameBridge.of_global_pvar lang pvar)
              | C ->
                  Void
              | _ ->
                  L.die InternalError "of_sil Lvar global not supported for %s"
                    (Lang.to_string lang)
            in
            let global : Global.t = {name; typ; attributes= []; init_exp= None} in
            TextualDecls.declare_global decls global ) ;
        Lvar name
    | Lfield ({exp= e}, f, typ) ->
        let typ = TypBridge.of_sil typ in
        let fielddecl = FieldDeclBridge.of_sil f typ false in
        let () =
          declare_struct_from_tenv lang decls tenv fielddecl.qualified_name.enclosing_class
        in
        Field {exp= of_sil decls tenv e; field= fielddecl.qualified_name}
    | Lindex (e1, e2) ->
        Index (of_sil decls tenv e1, of_sil decls tenv e2)
    | Sizeof _ ->
        L.die InternalError "Sizeof expression should not appear here, please report"
end

module InstrBridge = struct
  open Instr

  let of_sil decls tenv (i : Sil.instr) =
    match i with
    | Load {id; e; typ} ->
        let id = IdentBridge.of_sil id in
        let exp = ExpBridge.of_sil decls tenv e in
        let typ = Some (TypBridge.of_sil typ) in
        let loc = Location.Unknown in
        Load {id; exp; typ; loc}
    | Store {e1; typ; e2} ->
        let exp1 = ExpBridge.of_sil decls tenv e1 in
        let typ = Some (TypBridge.of_sil typ) in
        let exp2 = ExpBridge.of_sil decls tenv e2 in
        let loc = Location.Unknown in
        Store {exp1; typ; exp2; loc}
    | Prune (e, _, _, _) ->
        Prune {exp= ExpBridge.of_sil decls tenv e; loc= Location.Unknown}
    | Call ((id, _), Const (Cfun pname), (SilExp.Sizeof {typ= {desc= Tstruct name}}, _) :: _, _, _)
      when String.equal (SilProcname.to_simplified_string pname) "__new()" ->
        let typ = Typ.Struct (TypeNameBridge.of_sil name) in
        Let
          { id= Some (IdentBridge.of_sil id)
          ; exp= Exp.call_non_virtual ProcDecl.allocate_object_name [Typ typ]
          ; loc= Location.Unknown }
    | Call
        ((id, _), Const (Cfun pname), (SilExp.Sizeof {typ; dynamic_length= Some exp}, _) :: _, _, _)
      when String.equal (SilProcname.to_simplified_string pname) "__new_array()" ->
        let typ = TypBridge.of_sil typ in
        Let
          { id= Some (IdentBridge.of_sil id)
          ; exp=
              Exp.call_non_virtual ProcDecl.allocate_array_name
                [Typ typ; ExpBridge.of_sil decls tenv exp]
          ; loc= Location.Unknown }
    | Call ((id, ret_typ), Const (Cfun pname), args, _, call_flags) ->
        let args_typ = List.map ~f:snd args in
        let procdecl = ProcDeclBridge.of_sil pname ret_typ args_typ in
        let () = TextualDecls.declare_proc decls (Decl procdecl) in
        let proc = procdecl.qualified_name in
        let args = List.map ~f:(fun (e, _) -> ExpBridge.of_sil decls tenv e) args in
        let loc = Location.Unknown in
        let kind = if call_flags.cf_virtual then Exp.Virtual else Exp.NonVirtual in
        Let {id= Some (IdentBridge.of_sil id); exp= Call {proc; args; kind}; loc}
    | Call _ ->
        L.die InternalError "Translation of a SIL call that is not const not supported"
    | Metadata _ ->
        L.die InternalError "Translation of a metadata instructions not supported"
end

let instr_is_return = function Sil.Store {e1= Lvar v} -> SilPvar.is_return v | _ -> false

module TerminatorBridge = struct
  open Terminator

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

  let make_label_of_node () =
    let open SilProcdesc in
    let tbl = NodeHash.create 32 in
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


  let compute_locals_type (nodes : Node.t list) =
    List.fold nodes ~init:VarName.Map.empty ~f:(fun init (node : Node.t) ->
        List.fold node.instrs ~init ~f:(fun map (instr : Instr.t) ->
            match instr with
            | Store {exp1= Lvar var; typ= Some typ} ->
                VarName.Map.add var typ map
            | Load _ | Store _ | Prune _ | Let _ ->
                map ) )


  let of_sil decls tenv pdesc =
    let lang = TextualDecls.lang decls in
    let module P = SilProcdesc in
    let ret_typ = SilProcdesc.get_ret_type pdesc in
    let args_typ = SilProcdesc.get_pvar_formals pdesc |> List.map ~f:snd in
    let procdecl =
      P.get_proc_name pdesc |> fun pname -> ProcDeclBridge.of_sil pname ret_typ args_typ
    in
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
    in
    let start = start_node.label in
    let params =
      List.map (P.get_pvar_formals pdesc) ~f:(fun (pvar, _) -> VarNameBridge.of_pvar lang pvar)
    in
    let locals_type = compute_locals_type nodes in
    let locals =
      P.get_locals pdesc
      |> List.map ~f:(fun ({name; typ} : ProcAttributes.var_data) ->
             let var = Mangled.to_string name |> VarName.of_string in
             let typ =
               if SilTyp.is_void typ then
                 VarName.Map.find_opt var locals_type
                 |> Option.value
                      ~default:
                        ( match lang with
                        | Lang.Java ->
                            Typ.(mk_ptr (Struct TypeNameBridge.java_lang_object))
                        | _ ->
                            Typ.Void )
               else TypBridge.of_sil typ
             in
             (var, Typ.mk_without_attributes typ) )
    in
    let exit_loc = Location.Unknown in
    let fresh_ident = None in
    {procdecl; nodes; fresh_ident; start; params; locals; exit_loc}
end

module ModuleBridge = struct
  open Module

  let of_sil ~sourcefile ~lang tenv cfg =
    let env = TextualDecls.init sourcefile lang in
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
      TextualDecls.fold_procdecls env ~init:decls ~f:(fun decls procname ->
          Procdecl procname :: decls )
    in
    let attrs = [Attr.mk_source_language lang] in
    {attrs; decls; sourcefile}
end

let pp_copyright fmt =
  F.fprintf fmt "// \n" ;
  F.fprintf fmt "// Copyright (c) Facebook, Inc. and its affiliates.\n" ;
  F.fprintf fmt "//\n" ;
  F.fprintf fmt "// This source code is licensed under the MIT license found in the\n" ;
  F.fprintf fmt "// LICENSE file in the root directory of this source tree.\n" ;
  F.fprintf fmt "\n"


let from_sil ~lang ~filename tenv cfg =
  Utils.with_file_out filename ~f:(fun oc ->
      let fmt = F.formatter_of_out_channel oc in
      let sourcefile = SourceFile.create filename in
      pp_copyright fmt ;
      Module.pp fmt (ModuleBridge.of_sil ~sourcefile ~lang tenv cfg) ;
      Format.pp_print_flush fmt () )


let from_java ~filename tenv cfg = from_sil ~lang:Java ~filename tenv cfg

let from_c ~filename tenv cfg = from_sil ~lang:C ~filename tenv cfg
