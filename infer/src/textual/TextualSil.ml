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
    | Python ->
        L.die UserError "of_pvar conversion is not supported in Python mode"
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
    | Python ->
        L.die UserError "of_global_pvar conversion is not supported in Python mode"


  let replace_2colons_with_dot str = String.substr_replace_all str ~pattern:"::" ~with_:"."

  let string_to_java_sil string : SilTyp.Name.t =
    JavaClass (replace_2colons_with_dot string |> JavaClassName.from_string)


  let value_to_sil (lang : Lang.t) value : SilTyp.Name.t =
    match lang with
    | Java ->
        string_to_java_sil value
    | Hack ->
        HackClass (HackClassName.make value)
    | Python ->
        PythonClass (PythonClassName.make value)


  let to_sil (lang : Lang.t) {value} = value_to_sil lang value

  let java_lang_object = of_java_name "java.lang.Object"
end

let hack_dict_type_name = SilTyp.HackClass (HackClassName.make "HackDict")

let hack_dict_iter_type_name = SilTyp.HackClass (HackClassName.make "HackDictIterator")

let hack_vec_type_name = SilTyp.HackClass (HackClassName.make "HackVec")

let hack_vec_iter_type_name = SilTyp.HackClass (HackClassName.make "HackVecIterator")

let hack_bool_type_name = SilTyp.HackClass (HackClassName.make "HackBool")

let hack_int_type_name = SilTyp.HackClass (HackClassName.make "HackInt")

let hack_float_type_name = SilTyp.HackClass (HackClassName.make "HackFloat")

let hack_string_type_name = SilTyp.HackClass (HackClassName.make "HackString")

let hack_splated_vec_type_name = SilTyp.HackClass (HackClassName.make "HackSplatedVec")

let hack_mixed_type_name = SilTyp.HackClass (HackClassName.make "HackMixed")

let hack_awaitable_type_name = SilTyp.HackClass (HackClassName.make "HH::Awaitable")

let mk_hack_mixed_type_textual loc = Typ.Struct TypeName.{value= "HackMixed"; loc}

let hack_mixed_static_companion_type_name = SilTyp.Name.Hack.static_companion hack_mixed_type_name

let hack_builtins_type_name = SilTyp.HackClass (HackClassName.make "$builtins")

(* pseudo-class for top-level functions *)
let hack_root_type_name = SilTyp.HackClass (HackClassName.make "$root")

let python_mixed_type_name = SilTyp.PythonClass (PythonClassName.make "PyObject")

let mk_python_mixed_type_textual loc = Typ.Struct TypeName.{value= "PyObject"; loc}

let default_return_type (lang : Lang.t option) loc =
  match lang with
  | Some Hack ->
      Typ.Ptr (mk_hack_mixed_type_textual loc)
  | Some Python ->
      Typ.Ptr (mk_python_mixed_type_textual loc)
  | Some other ->
      L.die InternalError "Unexpected return type outside of Hack/Python: %s" (Lang.to_string other)
  | None ->
      L.die InternalError "Unexpected return type outside of Hack/Python: None"


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


let wildcard_sil_fieldname lang name =
  match (lang : Lang.t) with
  | Java ->
      L.die InternalError "a wildcard fieldname is only supported in Hack or Python"
  | Hack ->
      SilFieldname.make (HackClass HackClassName.wildcard) name
  | Python ->
      SilFieldname.make (PythonClass PythonClassName.wildcard) name


module TypBridge = struct
  open Typ

  let rec to_sil lang ?(attrs = []) (typ : t) : SilTyp.t =
    let is_const = List.exists attrs ~f:Textual.Attr.is_const in
    let quals = SilTyp.mk_type_quals ~is_const () in
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
    let mixed_struct = SilTyp.mk_struct hack_mixed_type_name in
    SilTyp.mk_ptr mixed_struct


  let python_mixed =
    let mixed_struct = SilTyp.mk_struct python_mixed_type_name in
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
          QualifiedProcName.Enclosing (TypeName.of_java_name (Procname.Java.get_class_name jpname))
        in
        let name = mangle_java_procname jpname |> ProcName.of_java_name in
        let qualified_name : QualifiedProcName.t = {enclosing_class; name} in
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
        (* FIXME when adding inheritance *)
        {qualified_name; formals_types= Some formals_types; result_type; attributes}
    | _ ->
        L.die InternalError "Non-Java procname %a should not appear in Java mode" Procname.describe
          pname


  let hack_class_name_to_sil = function
    | QualifiedProcName.TopLevel ->
        None
    | QualifiedProcName.Enclosing name ->
        Some (HackClassName.make name.value)


  let python_class_name_to_sil = function
    | QualifiedProcName.TopLevel ->
        None
    | QualifiedProcName.Enclosing name ->
        Some (PythonClassName.make name.value)


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
        let kind =
          match List.find t.attributes ~f:(fun attr -> Attr.is_static attr) with
          | Some _ ->
              SilProcname.Java.Static
          | None ->
              SilProcname.Java.Non_Static
        in
        SilProcname.make_java ~class_name ~return_type ~method_name ~parameters:formals_types ~kind
    | Hack ->
        let class_name = hack_class_name_to_sil t.qualified_name.enclosing_class in
        let arity = Option.map t.formals_types ~f:List.length in
        SilProcname.make_hack ~class_name ~function_name:method_name ~arity
    | Python ->
        let class_name = python_class_name_to_sil t.qualified_name.enclosing_class in
        let arity = Option.map t.formals_types ~f:List.length in
        SilProcname.make_python ~class_name ~function_name:method_name ~arity


  let call_to_sil (lang : Lang.t) (callsig : ProcSig.t) t : SilProcname.t =
    let arity = match callsig with Hack {arity} | Python {arity} -> arity | Other _ -> None in
    (* When we translate function calls in Hack or Python, the ProcDecl we get from TextualDecls may have
         unknown args. In such case we need to conjure up a procname with the arity matching that
         of the call site signature. This way we'll be able to match a particular overload of the
         procname with its definition from a different translation unit during the analysis
         phase. *)
    let improved_match name_to_sil make =
      if Option.is_some t.formals_types then to_sil lang t
      else
        let class_name = name_to_sil t.qualified_name.enclosing_class in
        let function_name = t.qualified_name.name.value in
        make ~class_name ~function_name ~arity
    in
    match lang with
    | Java ->
        to_sil lang t
    | Hack ->
        improved_match hack_class_name_to_sil Procname.make_hack
    | Python ->
        improved_match python_class_name_to_sil Procname.make_python
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

  let to_hack_class_info decls_env typ =
    TextualDecls.get_struct decls_env typ
    |> Option.value_map ~default:SilStruct.ClassInfo.NoInfo ~f:(fun {Textual.Struct.attributes} ->
           let has ~f = List.find ~f attributes |> Option.is_some in
           let kind =
             if has ~f:Textual.Attr.is_interface then SilStruct.Interface
             else if has ~f:Textual.Attr.is_trait then SilStruct.Trait
             else if has ~f:Textual.Attr.is_abstract then SilStruct.AbstractClass
             else if has ~f:Textual.Attr.is_alias then SilStruct.Alias
             else SilStruct.Class
           in
           SilStruct.ClassInfo.HackClassInfo kind )


  (** For Hack, [supers] contains the optional parent class but also all used traits. During method
      resolution, we have to look up in a reverse order, and traits always come before the parent
      class. Therefore we keep [supers] as reverse to perform an efficient lookup. *)
  let rev_hack_supers lang supers = match lang with Lang.Hack -> List.rev supers | _ -> supers

  let to_sil lang decls_env tenv proc_entries source_file {name; supers; fields; attributes} =
    let class_info =
      match lang with Textual.Lang.Hack -> Some (to_hack_class_info decls_env name) | _ -> None
    in
    let name = TypeNameBridge.to_sil lang name in
    let supers = rev_hack_supers lang supers in
    let supers = List.map supers ~f:(TypeNameBridge.to_sil lang) in
    let methods =
      List.filter_map proc_entries ~f:(fun proc_entry ->
          match (proc_entry : TextualDecls.ProcEntry.t) with
          | Desc proc ->
              Some proc.procdecl
          | Decl _ ->
              (* TODO: Don't just throw Decls away entirely *)
              None )
      |> List.map ~f:(ProcDeclBridge.to_sil lang)
    in
    let fields =
      List.map fields ~f:(fun ({FieldDecl.typ; attributes} as fdecl) ->
          let annot =
            List.filter_map attributes ~f:(fun attr ->
                if Attr.is_abstract attr then Some Annot.abstract else None )
          in
          SilStruct.mk_field ~annot
            (FieldDeclBridge.to_sil lang fdecl)
            (TypBridge.to_sil lang ~attrs:attributes typ) )
    in
    let annots =
      List.filter_map attributes ~f:(fun attr ->
          if Attr.is_final attr then Some Annot.final else None )
    in
    (* FIXME: generate static fields *)
    Tenv.mk_struct tenv ~fields ~annots ~supers ~methods ?class_info ?source_file name |> ignore


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
      | Load _ ->
          L.die InternalError "to_sil conversion on Load expression should never happen"
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
        | None when TypeName.equal field.enclosing_class TypeName.wildcard ->
            Lfield (aux exp, wildcard_sil_fieldname lang field.name.value, SilTyp.mk SilTyp.Tvoid)
        | None ->
            L.die InternalError "field %a.%a has not been declared" TypeName.pp
              field.enclosing_class FieldName.pp field.name
        | Some field ->
            Lfield
              ( aux exp
              , FieldDeclBridge.to_sil lang field
              , TypBridge.to_sil lang ~attrs:field.attributes field.typ ) )
      | Index (exp1, exp2) ->
          Lindex (aux exp1, aux exp2)
      | Const const ->
          Const (ConstBridge.to_sil const)
      | Call {proc; args= [Typ typ; exp]} when ProcDecl.is_cast_builtin proc ->
          Cast (TypBridge.to_sil lang typ, aux exp)
      | Call {proc; args} -> (
          let nb_args = List.length args in
          let procsig = Exp.call_sig proc nb_args (TextualDecls.lang decls_env) in
          match
            ( TextualDecls.get_procdecl decls_env procsig nb_args
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
                QualifiedProcName.pp proc
          (* FIXME: transform instruction to put call at head of expressions *) )
      | Typ _ ->
          L.die InternalError "Internal error: type expressions should not appear outside builtins"
      | Closure _ | Apply _ ->
          L.die InternalError "Internal error: closures should not appear inside sub-expressions"
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
        let () = TextualDecls.declare_proc decls (Decl procdecl) in
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
    | Load {typ= None} ->
        L.die InternalError "to_sil should come after type inference"
    | Load {id; exp; typ= Some typ; loc} ->
        let typ = TypBridge.to_sil lang typ in
        let id = IdentBridge.to_sil id in
        let e = ExpBridge.to_sil lang decls_env procname exp in
        let loc = LocationBridge.to_sil sourcefile loc in
        Load {id; e; typ; loc}
    | Store {typ= None} ->
        L.die InternalError "to_sil should come after type inference"
    | Store {exp1; typ= Some typ; exp2; loc} ->
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
          SilExp.Sizeof
            {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact; nullable= false}
        in
        let class_type = SilTyp.mk_ptr typ in
        let args = [(sizeof, class_type)] in
        let ret = IdentBridge.to_sil id in
        let loc = LocationBridge.to_sil sourcefile loc in
        let builtin_new = SilExp.Const (SilConst.Cfun BuiltinDecl.__new) in
        Call ((ret, class_type), builtin_new, args, loc, CallFlags.default)
    | Let {id; exp= Call {proc; args= target :: Typ typ :: rest}; loc}
      when ProcDecl.is_instanceof_builtin proc ->
        let typ = TypBridge.to_sil lang typ in
        let nullable =
          match rest with
          | [] ->
              false
          | [Const (Int n)] ->
              Z.equal n Z.one
          | _ ->
              L.die InternalError "non-matching args to instanceof"
        in
        let sizeof =
          SilExp.Sizeof
            {typ; nbytes= None; dynamic_length= None; subtype= Subtype.subtypes_instof; nullable}
        in
        let target = ExpBridge.to_sil lang decls_env procname target in
        let args = [(target, StdTyp.void_star); (sizeof, StdTyp.void)] in
        let ret = IdentBridge.to_sil id in
        let loc = LocationBridge.to_sil sourcefile loc in
        let builtin_instanceof = SilExp.Const (SilConst.Cfun BuiltinDecl.__instanceof) in
        Call ((ret, StdTyp.boolean), builtin_instanceof, args, loc, CallFlags.default)
    | Let {id; exp= Call {proc; args= Typ element_typ :: exp :: _}; loc}
      when ProcDecl.is_allocate_array_builtin proc ->
        let element_typ = TypBridge.to_sil lang element_typ in
        let typ = SilTyp.mk_array element_typ in
        let e = ExpBridge.to_sil lang decls_env procname exp in
        let sizeof =
          SilExp.Sizeof
            {typ; nbytes= None; dynamic_length= Some e; subtype= Subtype.exact; nullable= false}
        in
        (* TODO(T133560394): check if we need to remove Array constructors in the type typ *)
        let class_type = SilTyp.mk_ptr typ in
        let args = [(sizeof, class_type)] in
        let ret = IdentBridge.to_sil id in
        let loc = LocationBridge.to_sil sourcefile loc in
        let builtin_new = SilExp.Const (SilConst.Cfun BuiltinDecl.__new_array) in
        Call ((ret, class_type), builtin_new, args, loc, CallFlags.default)
    | Let {id; exp= Call {proc; args= [Typ typ]}; loc}
      when ProcDecl.is_lazy_class_initialize_builtin proc || ProcDecl.is_get_lazy_class_builtin proc
      ->
        let typ = TypBridge.to_sil lang typ in
        let sizeof =
          SilExp.Sizeof
            {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact; nullable= false}
        in
        let class_type = SilTyp.mk_ptr typ in
        let args = [(sizeof, class_type)] in
        let ret = IdentBridge.to_sil id in
        let loc = LocationBridge.to_sil sourcefile loc in
        let builtin =
          if ProcDecl.is_lazy_class_initialize_builtin proc then
            SilExp.Const (Cfun BuiltinDecl.__lazy_class_initialize)
          else if ProcDecl.is_get_lazy_class_builtin proc then
            SilExp.Const (Cfun BuiltinDecl.__get_lazy_class)
          else L.die InternalError "should not happen because of the pattern-matching test"
        in
        (* TODO: we may want to use the class_of_class type here *)
        Call ((ret, class_type), builtin, args, loc, CallFlags.default)
    | Let {id; exp= Call {proc; args; kind}; loc} ->
        let ret = IdentBridge.to_sil id in
        let procsig = Exp.call_sig proc (List.length args) (TextualDecls.lang decls_env) in
        let variadic_status, ({formals_types} as callee_procdecl : ProcDecl.t) =
          match TextualDecls.get_procdecl decls_env procsig (List.length args) with
          | Some (variadic_flag, _, procdecl) ->
              (variadic_flag, procdecl)
          | None when QualifiedProcName.contains_wildcard proc ->
              let textual_ret_typ =
                (* Declarations with unknown formals are expected in Hack/Python. Assume that unknown
                   return types are *HackMixed/*PyObject respectively. *)
                default_return_type (Some lang) loc
              in
              ( TextualDecls.NotVariadic
              , { ProcDecl.qualified_name= proc
                ; formals_types= None
                ; result_type= Typ.mk_without_attributes textual_ret_typ
                ; attributes= [] } )
          | None ->
              let msg =
                lazy
                  (F.asprintf
                     "no procdecl for %a the expression in %a should start with a regular call"
                     ProcSig.pp procsig pp i )
              in
              raise (TextualTransformError [{loc; msg}])
        in
        let pname = ProcDeclBridge.call_to_sil lang procsig callee_procdecl in
        let result_type =
          TypBridge.to_sil lang ~attrs:callee_procdecl.result_type.attributes
            callee_procdecl.result_type.typ
        in
        let args = List.map ~f:(ExpBridge.to_sil lang decls_env procname) args in
        let args =
          match formals_types with
          | None ->
              let default_typ =
                match lang with
                | Lang.Hack ->
                    (* Declarations with unknown formals are expected in Hack. Assume that unknown
                       formal types are *HackMixed. *)
                    TypBridge.hack_mixed
                | Lang.Python ->
                    (* Declarations with unknown formals are expected in Python. Assume that unknown
                       formal types are *PyObject. *)
                    TypBridge.python_mixed
                | other ->
                    L.die InternalError "Unexpected unknown formals outside of Hack/Python: %s"
                      (Lang.to_string other)
              in
              List.map args ~f:(fun arg -> (arg, default_typ))
          | Some formals_types -> (
              let formals_types =
                List.map formals_types ~f:(fun ({typ} : Typ.annotated) -> TypBridge.to_sil lang typ)
              in
              let formals_types =
                match (variadic_status : TextualDecls.variadic_status) with
                | NotVariadic ->
                    if TextualDecls.is_trait_method decls_env procsig then
                      List.drop_last_exn formals_types
                    else formals_types
                | Variadic variadic_type ->
                    (* we may have too much arguments, and we then complete formal_args *)
                    (* formals_args = [t1; ...; tn; variadic_type ] *)
                    let n = List.length formals_types - 1 in
                    let variadic_type = TypBridge.to_sil lang variadic_type in
                    List.take formals_types n
                    @ List.init (List.length args - n) ~f:(fun _ -> variadic_type)
              in
              match List.zip args formals_types with
              | Ok l ->
                  l
              | _ ->
                  L.die UserError "the call %a has been given a wrong number of arguments" pp i )
        in
        let loc = LocationBridge.to_sil sourcefile loc in
        let cf_virtual = Exp.equal_call_kind kind Virtual in
        let cflag = {CallFlags.default with cf_virtual} in
        Call ((ret, result_type), Const (Cfun pname), args, loc, cflag)
    | Let {exp= Closure _; loc} ->
        let msg = lazy "closure construction should have been transformed before SIL conversion" in
        raise (TextualTransformError [{loc; msg}])
    | Let {exp= Apply _; loc} ->
        let msg = lazy "closure call should have been transformed before SIL conversion" in
        raise (TextualTransformError [{loc; msg}])
    | Let {loc; _} ->
        let msg = lazy (F.asprintf "the expression in %a should start with a regular call" pp i) in
        raise (TextualTransformError [{loc; msg}])
end

let instr_is_return = function Sil.Store {e1= Lvar v} -> SilPvar.is_return v | _ -> false

module TerminatorBridge = struct
  open Terminator

  let to_sil lang decls_env procname pdesc loc (t : t) : Sil.instr list =
    let write_to_ret_var exp =
      let ret_var = SilPvar.get_ret_pvar (ProcDeclBridge.to_sil lang procname) in
      let ret_type = SilProcdesc.get_ret_type pdesc in
      let e2 = ExpBridge.to_sil lang decls_env procname exp in
      Sil.Store {e1= SilExp.Lvar ret_var; typ= ret_type; e2; loc}
    in
    match t with
    | If _ ->
        L.die InternalError "to_sil should not be called on If terminator"
    | Ret exp ->
        [write_to_ret_var exp]
    | Throw exp ->
        let builtin_throw = SilExp.Const (SilConst.Cfun BuiltinDecl.__hack_throw) in
        let ret = SilIdent.create_fresh SilIdent.kprimed in
        [ write_to_ret_var exp
        ; Call ((ret, SilTyp.mk SilTyp.Tvoid), builtin_throw, [], loc, CallFlags.default) ]
    | Jump _ ->
        []
    | Unreachable ->
        []


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
    let sourcefile = TextualDecls.source_file decls_env in
    let load_thrown_exception =
      match node.ssa_parameters with
      | [] ->
          []
      | [(id, _typ)] ->
          let sil_param_ident = IdentBridge.to_sil id in
          let ret_var = SilPvar.get_ret_pvar (ProcDeclBridge.to_sil lang procname) in
          let ret_type = SilProcdesc.get_ret_type pdesc in
          let load_param : Sil.instr =
            Sil.Load
              { id= sil_param_ident
              ; e= SilExp.Lvar ret_var
              ; typ= ret_type
              ; loc= LocationBridge.to_sil sourcefile node.label_loc }
          in
          [load_param]
      | _ ->
          let msg =
            lazy
              (F.asprintf "node %a should not have more than one SSA parameter" NodeName.pp
                 node.label )
          in
          raise (TextualTransformError [{loc= node.label_loc; msg}])
    in
    let instrs =
      load_thrown_exception @ List.map ~f:(InstrBridge.to_sil lang decls_env procname) node.instrs
    in
    let last_loc = LocationBridge.to_sil sourcefile node.last_loc in
    let last = TerminatorBridge.to_sil lang decls_env procname pdesc last_loc node.last in
    let instrs = match last with [] -> instrs | _ -> instrs @ last in
    (* Use min instr line for node's loc. This makes node placement in a debug HTML a bit more
       predictable and relevant compared to using block labels' locations which can be more detached
       from the actual source code when we're dealing with translated sources
       (e.g. Hack->Textual). *)
    let loc =
      let known_instr_lines =
        List.filter_map instrs ~f:(fun instr ->
            match (Sil.location_of_instr instr).line with -1 -> None | other -> Some other )
      in
      let label_loc = LocationBridge.to_sil sourcefile node.label_loc in
      let first_line = List.fold known_instr_lines ~init:label_loc.line ~f:min in
      {label_loc with line= first_line}
    in
    let nkind =
      match
        List.find_map ~f:(function Sil.Prune (_, _, branch, _) -> Some branch | _ -> None) instrs
      with
      | Some branch ->
          (* This is incomplete as Textual cannot distinguish between if/loops as well as the branch condition for now.*)
          let if_kind = Sil.Ik_if {terminated= false} in
          SilProcdesc.Node.Prune_node (branch, if_kind, PruneNodeKind_MethodBody)
      | None ->
          SilProcdesc.Node.Stmt_node MethodBody
    in
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
    let mk_formal ({typ; attributes} : Typ.annotated) vname =
      let name = Mangled.from_string vname.VarName.value in
      let typ = TypBridge.to_sil lang typ in
      let annots =
        if List.exists ~f:Attr.is_notnull attributes then [Annot.notnull] else Annot.Item.empty
      in
      (name, typ, annots)
    in
    match List.map2 (ProcDesc.formals procdesc) params ~f:mk_formal with
    | Ok l ->
        l
    | Unequal_lengths ->
        L.die InternalError "procname %a has not the same number of arg names and arg types"
          QualifiedProcName.pp procdecl.qualified_name


  let build_locals lang {locals} =
    let make_var_data name typ : ProcAttributes.var_data =
      { name
      ; typ
      ; modify_in_block= false
      ; is_constexpr= false
      ; is_declared_unused= false
      ; is_structured_binding= false
      ; has_cleanup_attribute= false
      ; tmp_id= None }
    in
    List.map locals ~f:(fun (var, annotated_typ) ->
        let name = Mangled.from_string var.VarName.value in
        let typ = TypBridge.to_sil lang ~attrs:annotated_typ.Typ.attributes annotated_typ.Typ.typ in
        make_var_data name typ )


  let to_sil lang decls_env cfgs ({procdecl; nodes; start; exit_loc} as pdesc) =
    let sourcefile = TextualDecls.source_file decls_env in
    let sil_procname = ProcDeclBridge.to_sil lang procdecl in
    let sil_ret_type =
      TypBridge.to_sil lang ~attrs:procdecl.result_type.attributes procdecl.result_type.typ
    in
    let definition_loc = LocationBridge.to_sil sourcefile procdecl.qualified_name.name.loc in
    let is_hack_async = List.exists procdecl.attributes ~f:Attr.is_async in
    let is_abstract = List.exists procdecl.attributes ~f:Attr.is_abstract in
    let is_hack_wrapper = List.exists procdecl.attributes ~f:Attr.is_hack_wrapper in
    let hack_variadic_position =
      Option.value_map ~default:None procdecl.formals_types ~f:(fun formals_types ->
          List.findi formals_types ~f:(fun _ typ -> Typ.is_annotated typ ~f:Attr.is_variadic)
          |> Option.map ~f:fst )
    in
    let formals = build_formals lang pdesc in
    let locals = build_locals lang pdesc in
    let pattributes =
      { (ProcAttributes.default (SourceFile.file sourcefile) sil_procname) with
        is_defined= true
      ; is_hack_async
      ; is_abstract
      ; is_hack_wrapper
      ; hack_variadic_position
      ; formals
      ; locals
      ; ret_type= sil_ret_type
      ; loc= definition_loc }
    in
    let pdesc = Cfg.create_proc_desc cfgs pattributes in
    (* Create standalone start, end, and exn_sink nodes. Note that SIL start node does not correspond to the start node in
       Textual. The latter is more like a _first node_. *)
    let module P = SilProcdesc in
    let start_node = P.create_node pdesc definition_loc P.Node.Start_node [] in
    P.set_start_node pdesc start_node ;
    let exit_loc = LocationBridge.to_sil sourcefile exit_loc in
    let exit_node = P.create_node pdesc exit_loc P.Node.Exit_node [] in
    P.set_exit_node pdesc exit_node ;
    let exn_sink_node = P.create_node pdesc exit_loc P.Node.exn_sink_kind [] in
    P.node_set_succs pdesc exn_sink_node ~normal:[exit_node] ~exn:[exit_node] ;
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
    let normal_succ (term : Terminator.t) =
      match term with
      | If _ ->
          L.die InternalError "to_sil should not be called on If terminator"
      | Ret _ ->
          [exit_node]
      | Jump l ->
          List.map
            ~f:(fun ({label} : Terminator.node_call) -> Hashtbl.find node_map label.value |> snd)
            l
      | Throw _ | Unreachable ->
          []
    in
    Hashtbl.iter
      (fun _ ((node : Node.t), sil_node) ->
        let exn : P.Node.t list =
          List.map ~f:(fun name -> Hashtbl.find node_map name.NodeName.value |> snd) node.exn_succs
        in
        let exn = if List.is_empty exn then [exn_sink_node] else exn in
        let normal = normal_succ node.last in
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
            | Store {exp1= Lvar var; typ= Some typ} ->
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
          (* note: because && and || operators are lazy we must remove them before moving calls *)
          module_ |> remove_if_terminator
          |> remove_effects_in_subexprs lang decls_env
          |> let_propagation |> out_of_ssa
        in
        let all_proc_entries, types_used_as_enclosing_but_not_defined =
          TextualDecls.get_proc_entries_by_enclosing_class decls_env
        in
        let cfgs = Cfg.create () in
        let tenv = Tenv.create () in
        TypeName.Set.iter
          (fun name -> TypeNameBridge.to_sil lang name |> Tenv.mk_struct ~dummy:true tenv |> ignore)
          types_used_as_enclosing_but_not_defined ;
        List.iter module_.decls ~f:(fun decl ->
            match decl with
            | Global _ ->
                ()
            | Struct strct ->
                let proc_entries =
                  TypeName.Map.find_opt strct.name all_proc_entries |> Option.value ~default:[]
                in
                let source_file = Some (TextualDecls.source_file decls_env |> SourceFile.file) in
                StructBridge.to_sil lang decls_env tenv proc_entries source_file strct
            | Procdecl _ ->
                ()
            | Proc pdesc ->
                if not (ProcDesc.is_ready_for_to_sil_conversion pdesc) then
                  (* we only run SSA verification if the to_sil conversion  needs
                     extra transformation, because some .sil files that are generated by
                     Java examples are not in SSA *)
                  SsaVerification.run pdesc ;
                ProcDescBridge.to_sil lang decls_env cfgs pdesc ) ;
        (* Register undefined types in tenv *)
        let is_undefined_type tname =
          (not (TypeName.Set.mem tname types_used_as_enclosing_but_not_defined))
          && TextualDecls.get_struct decls_env tname |> Option.is_none
        in
        TextualDecls.get_undefined_types decls_env
        |> Seq.iter (fun tname ->
               if is_undefined_type tname then
                 let sil_tname = TypeNameBridge.to_sil lang tname in
                 Tenv.mk_struct ~dummy:true tenv sil_tname |> ignore ) ;
        (cfgs, tenv)


  let of_sil ~sourcefile ~lang tenv cfg =
    let env = TextualDecls.init sourcefile (Some lang) in
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

let proc_decl_to_sil lang procdecl = ProcDeclBridge.to_sil lang procdecl

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


let dump_module ~filename module_ =
  Utils.with_file_out filename ~f:(fun oc ->
      let fmt = F.formatter_of_out_channel oc in
      pp_copyright fmt ;
      Module.pp fmt module_ ;
      Format.pp_print_flush fmt () )


let textual_ext = ".sil"

let to_filename path =
  let flat = Utils.flatten_path path in
  let noext, _ = Filename.split_extension flat in
  noext ^ textual_ext
