(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging
module F = Format

let annotation_to_string ((annotation: Annot.t), _) =
  "< " ^ annotation.class_name ^ " : " ^
  (IList.to_string (fun x -> x) annotation.parameters) ^ " >"

let field_to_string (fieldname, typ, annotation) =
  (Ident.fieldname_to_string fieldname) ^ " " ^
  (Typ.to_string typ) ^  (IList.to_string annotation_to_string annotation)

let print_tenv tenv =
  Tenv.iter (fun typname struct_t ->
      match typname with
      | Typename.TN_csu (Csu.Class _, _) | Typename.TN_csu (Csu.Protocol, _) ->
          Logging.do_out "%s" (
            (Typename.to_string typname) ^ " " ^
            (Annot.Item.to_string struct_t.annots) ^ "\n" ^
            "---> superclass and protocols " ^ (IList.to_string (fun tn ->
                "\t" ^ (Typename.to_string tn) ^ "\n") struct_t.supers) ^
            "---> methods " ^
            (IList.to_string (fun x ->"\t" ^ (Procname.to_string x) ^ "\n") struct_t.methods)
            ^ "  " ^
            "\t---> fields " ^ (IList.to_string field_to_string struct_t.fields) ^ "\n")
      | _ -> ()
    ) tenv

let print_tenv_struct_unions tenv =
  Tenv.iter (fun typname struct_t ->
      match typname with
      | Typename.TN_csu (Csu.Struct, _) | Typename.TN_csu (Csu.Union, _) ->
          Logging.do_out "%s" (
            (Typename.to_string typname)^"\n"^
            "\t---> fields "^(IList.to_string (fun (fieldname, typ, _) ->
                match typ with
                | Typ.Tstruct tname -> "tvar"^(Typename.to_string tname)
                | _ ->
                    "\t struct "^(Ident.fieldname_to_string fieldname)^" "^
                    (Typ.to_string typ)^"\n") struct_t.fields
              )
          )
      | _ -> ()
    ) tenv

let print_procedures cfg =
  let procs = Cfg.get_all_procs cfg in
  Logging.do_out "%s"
    (IList.to_string (fun pdesc ->
         let pname = Cfg.Procdesc.get_proc_name pdesc in
         "name> "^
         (Procname.to_string pname) ^
         " defined? " ^ (string_of_bool (Cfg.Procdesc.is_defined pdesc)) ^ "\n")
        procs)

let print_failure_info pointer =
  L.err "AST Element> %s IN FILE> %s @.@." pointer !CFrontend_config.json

let print_nodes nodes =
  IList.iter (fun node -> Logging.do_out "%s" (Cfg.Node.get_description pe_text node)) nodes

let instrs_to_string instrs =
  let pp fmt () = Format.fprintf fmt "%a" (Sil.pp_instr_list pe_text) instrs in
  pp_to_string pp ()
