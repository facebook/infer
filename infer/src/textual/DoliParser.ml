(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open DoliAst

let parse path =
  if String.is_suffix path ~suffix:".doli" then (
    let cin = In_channel.create path in
    let filebuf = CombinedLexer.Lexbuf.from_channel cin in
    let filename = Filename.basename path in
    try
      let lexer = CombinedLexer.Lexbuf.with_tokenizer CombinedLexer.mainlex filebuf in
      let prog =
        MenhirLib.Convert.Simplified.traditional2revised CombinedMenhir.doliProgram lexer
      in
      In_channel.close cin ;
      Ok prog
    with CombinedMenhir.Error ->
      let token = CombinedLexer.Lexbuf.lexeme filebuf in
      let pos, _ = CombinedLexer.Lexbuf.lexing_positions filebuf in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      let msg =
        Printf.sprintf "doli syntax error: unexpected token \"%s\" in %s (line %d, column %d)\n%!"
          token filename line col
      in
      In_channel.close cin ;
      Error msg )
  else Error ("doli parsing error: the file " ^ path ^ " should end with a .doli extension")


let global_doli_matcher = ref ([] : (DoliAst.matching * Procname.t) list)

(* warning: it will assign the global [global_doli_matcher] *)
let program_to_textual_module filename (DoliProgram _instrs) : Textual.Module.t =
  let open Textual in
  let sourcefile = SourceFile.create filename in
  let lang = Lang.Java in
  (* we have the choice between Hack or Java currently *)
  let instrs_with_decls = ([] : (doliInstruction * ProcDesc.t) list) in
  (* TODO: build a list of pairs (Doli instruction, Textual procdesc) *)
  let decls = List.map ~f:(fun (_, procdesc) -> Module.Proc procdesc) instrs_with_decls in
  global_doli_matcher :=
    List.map instrs_with_decls ~f:(fun (instr, procdesc) ->
        let procname = TextualSil.proc_decl_to_sil Lang.Java procdesc.ProcDesc.procdecl in
        (instr.match_, procname) ) ;
  {attrs= [Attr.mk_source_language lang]; decls; sourcefile}


let exec_matching (_match_ : matching) (_ir_procname : Procname.t) : bool = false
(* TODO: tests if [ir_procname] is matched by [match_] *)

let matcher ir_procname =
  List.find_map !global_doli_matcher ~f:(fun (match_, model_procname) ->
      if exec_matching match_ ir_procname then Some model_procname else None )


let run path =
  let filename = Filename.basename path in
  match parse path with
  | Ok doli_program -> (
      Printf.printf "doli parsing of %s succeeded.\n" filename ;
      let module_ = program_to_textual_module path doli_program in
      let sourcefile = module_.Textual.Module.sourcefile in
      try
        let cfg, tenv = TextualSil.module_to_sil module_ ~line_map:None in
        DB.Results_dir.init sourcefile ;
        SourceFiles.add sourcefile cfg (FileLocal tenv) None ;
        Option.iter (Tenv.load_global ()) (* will only work with Java that has a unique Tenv *)
          ~f:(fun global_tenv -> Tenv.merge ~src:tenv ~dst:global_tenv)
      with Textual.TextualTransformError errors ->
        List.iter errors ~f:(fun error ->
            Format.eprintf "%a" (Textual.pp_transform_error sourcefile) error ) )
  | Error msg ->
      Printf.eprintf "%s" msg
