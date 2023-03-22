(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open DoliAst
module L = Logging

let global_doli_matcher = ref ([] : (DoliAst.matching * Procname.t) list)

let lastLocation (ndList : Textual.Node.t list) : Textual.Location.t =
  match ndList with [] -> Unknown | nd :: _ -> nd.last_loc


(* warning: it will assign the global [global_doli_matcher] *)
let program_to_textual_module sourcefile (DoliProgram rules) : Textual.Module.t =
  let open Textual in
  let lang = Lang.Java in
  (* we have the choice between Hack or Java currently *)
  let make_decl_from_rule (rule : doliRule) : Textual.ProcDecl.t =
    (* TODO make the entries in this record  more precise *)
    { qualified_name= {enclosing_class= TopLevel; name= {value= rule.ruleName; loc= Unknown}}
    ; formals_types= DoliAst.param_types_to_textual rule
    ; are_formal_types_fully_declared= true
    ; result_type= {typ= DoliAst.return_type_to_textual rule; attributes= []}
    ; attributes= [] }
  in
  let make_desc_from_rule (rule : doliRule) : ProcDesc.t =
    let nodes = rule.body.nodes in
    (* FIXME make the entries in this record  more precise *)
    { procdecl= make_decl_from_rule rule
    ; nodes
    ; start=
        { value= (match nodes with [] -> "error_string" | hd_node :: _ -> hd_node.label.value)
        ; loc= Unknown }
    ; params= DoliAst.get_parameter_names rule
    ; locals= []
    ; exit_loc= lastLocation nodes }
  in
  let rules_with_decls = List.map ~f:(fun rule -> (rule, make_desc_from_rule rule)) rules in
  let decls = List.map ~f:(fun (_, procdesc) -> Module.Proc procdesc) rules_with_decls in
  global_doli_matcher :=
    List.map rules_with_decls ~f:(fun (instr, procdesc) ->
        let procname = TextualSil.proc_decl_to_sil Lang.Java procdesc.ProcDesc.procdecl in
        (instr.match_, procname) ) ;
  {attrs= [Attr.mk_source_language lang]; decls; sourcefile}


let rec match_type_paths (str_list : string list) (doli_class_types : DoliJavaAst.classType list) :
    bool =
  match (str_list, doli_class_types) with
  | [], [] ->
      true
  | str :: strs, CT (doli_str, _) :: rest ->
      String.equal str doli_str && match_type_paths strs rest
  | _ ->
      false


let match_receiver_classes (ext_sign : DoliJavaAst.extendedSignature)
    (java_proc_name : Procname.Java.t) :
    bool
    (* checks whether the two patchs to the tyoes are identical *)
    (* TODO consider class hierarchy *)
    (* TODO consider generics *) =
  let strs = String.split_on_chars ~on:['.'] (Procname.Java.get_class_name java_proc_name) in
  match ext_sign.under with RT classTypes -> match_type_paths strs classTypes


let match_sign_with_procname (sign : DoliJavaAst.signature) (java_proc_name : Procname.Java.t) :
    bool =
  (* matches the identifiers of the methods *)
  String.equal
    (DoliJavaAst.get_func_identifier_simple sign)
    (Procname.Java.get_method java_proc_name)


let match_ext_sign_with_procname (ext_sign : DoliJavaAst.extendedSignature)
    (java_proc_name : Procname.Java.t) : bool =
  (* mathes the identifiers as well as the receivers *)
  (* TODO: also match the argument types *)
  (* TODO: consider subtypes for the arguments *)
  match_receiver_classes ext_sign java_proc_name
  && List.exists ~f:(fun sign -> match_sign_with_procname sign java_proc_name) ext_sign.signs


let exec_matching (matching : matching) (ir_procname : Procname.t) : bool =
  (* tests whether [ir_procname] is matched by [match_] *)
  match (matching, ir_procname) with
  | ObjCMatching _, _ ->
      L.die InternalError "we have not yet implemented ObjC for doli"
  | JavaMatching doli_java_ext_signs, Java java_proc_name ->
      List.exists
        ~f:(fun ext_sign -> match_ext_sign_with_procname ext_sign java_proc_name)
        doli_java_ext_signs
  | _ ->
      false


let matcher (ir_procname : Procname.t) =
  List.find_map !global_doli_matcher ~f:(fun (match_, model_procname) ->
      if exec_matching match_ ir_procname then Some model_procname else None )
