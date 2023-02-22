(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open DoliAst

let global_doli_matcher = ref ([] : (DoliAst.matching * Procname.t) list)

(* warning: it will assign the global [global_doli_matcher] *)
let program_to_textual_module sourcefile (DoliProgram rules) : Textual.Module.t =
  let open Textual in
  let lang = Lang.Java in
  (* we have the choice between Hack or Java currently *)
  let make_decl_from_rule (rule : doliRule) : Textual.ProcDecl.t =
    (* TODO make the entries in this record  more precise *)
    { qualified_name= {enclosing_class= TopLevel; name= {value= rule.ruleName; loc= Unknown}}
    ; formals_types= []
    ; are_formal_types_fully_declared= true
    ; result_type= {typ= Int; attributes= []}
    ; attributes= [] }
  in
  let make_desc_from_rule (rule : doliRule) : ProcDesc.t =
    let nodes = rule.body.nodes in
    (* TODO make the entries in this record  more precise *)
    { procdecl= make_decl_from_rule rule
    ; nodes
    ; start=
        { value= (match nodes with [] -> "error_string" | hd_node :: _ -> hd_node.label.value)
        ; loc= Unknown }
    ; params= []
    ; locals= []
    ; exit_loc= Unknown }
  in
  let rules_with_decls = List.map ~f:(fun rule -> (rule, make_desc_from_rule rule)) rules in
  let decls = List.map ~f:(fun (_, procdesc) -> Module.Proc procdesc) rules_with_decls in
  global_doli_matcher :=
    List.map rules_with_decls ~f:(fun (instr, procdesc) ->
        let procname = TextualSil.proc_decl_to_sil Lang.Java procdesc.ProcDesc.procdecl in
        (instr.match_, procname) ) ;
  {attrs= [Attr.mk_source_language lang]; decls; sourcefile}


let exec_matching (matching : matching) (ir_procname : Procname.t) : bool =
  (* tests whether [ir_procname] is matched by [match_] *)
  let matchSignWithProcname (sign : DoliJavaAst.signature) (_ir_procname : Procname.t) =
    (* TODO make the check more precise *)
    String.equal sign.identifier (Procname.to_simplified_string ir_procname)
  in
  let matchSign (sign : DoliJavaAst.signature) = matchSignWithProcname sign ir_procname in
  let matchSigns (signs : DoliJavaAst.signature list) = List.exists ~f:matchSign signs in
  let matchExtSign (extSign : DoliJavaAst.extendedSignature) = matchSigns extSign.signs in
  match matching with
  | ObjCMatching _ ->
      false
  | JavaMatching doliJavaExtendedSignatures ->
      List.exists ~f:matchExtSign doliJavaExtendedSignatures


let matcher ir_procname =
  List.find_map !global_doli_matcher ~f:(fun (match_, model_procname) ->
      if exec_matching match_ ir_procname then Some model_procname else None )
