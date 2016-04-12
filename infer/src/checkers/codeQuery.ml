(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Module for code queries. *)

module L = Logging
module F = Format

let verbose = false
let query = ref None

let parse_query s =
  let buf = Lexing.from_string s in
  try
    match CodequeryParser.query CodequeryLexer.token buf with
    | None ->
        L.stdout "empty rule@.";
        assert false
    | Some query ->
        if verbose then L.stdout "%a@." CodeQueryAst.pp_query query;
        query
  with
  | Parsing.Parse_error ->
      let lexbuf = Bytes.to_string buf.Lexing.lex_buffer in
      L.stdout "@.parsing stops on line %d: \n\n%s@." !CodequeryLexer.line_number lexbuf;
      assert false

let query_ast =
  lazy (match !query with None -> parse_query "x(y)" | Some s -> parse_query s)

module Err = struct
  (** Update the summary with stats from the checker. *)
  let update_summary proc_name proc_desc =
    let old_summ = Specs.get_summary_unsafe "codeQuery" proc_name in
    let nodes = IList.map (fun n -> Cfg.Node.get_id n) (Cfg.Procdesc.get_nodes proc_desc) in
    let specs =
      let spec =
        { Specs.pre = Specs.Jprop.Prop (1, Prop.prop_emp);
          posts = [];
          visited = Specs.Visitedset.empty
        } in
      [(Specs.spec_normalize spec)] in
    let new_summ = { old_summ with
                     Specs.attributes =
                       { old_summ.Specs.attributes with
                         ProcAttributes.loc = Cfg.Procdesc.get_loc proc_desc };
                     nodes = nodes;
                     payload =
                       { old_summ.Specs.payload with
                         Specs.preposts = Some specs; }
                   } in
    Specs.add_summary proc_name new_summ

  let add_error_to_spec proc_name s node loc =
    Checkers.ST.pname_add proc_name "codequery" "active"; (* force creation of .specs files *)
    State.set_node node;
    let exn = Exceptions.Codequery (Localise.verbatim_desc s) in
    Reporting.log_error proc_name ~loc: (Some loc) exn
end

(** Matcher for rules. *)
module Match = struct
  type value =
    | Vfun of Procname.t
    | Vval of Sil.exp

  let pp_value fmt = function
    | Vval e -> F.fprintf fmt "%a" (Sil.pp_exp pe_text) e
    | Vfun pn -> F.fprintf fmt "%s" (Procname.to_string pn)

  let value_equal v1 v2 = match v1, v2 with
    | Vval e1, Vval e2 -> Sil.exp_equal e1 e2
    | Vval _, _ -> false
    | _, Vval _ -> false
    | Vfun pn1, Vfun pn2 -> Procname.equal pn1 pn2

  let init_env () = Hashtbl.create 1

  let env_copy env = Hashtbl.copy env

  let env_add env id value =
    try
      let value' = Hashtbl.find env id in
      value_equal value value'
    with Not_found ->
      Hashtbl.add env id value;
      true
  let pp_env fmt env =
    let pp_item id value =
      F.fprintf fmt "%s=%a " id pp_value value in
    Hashtbl.iter pp_item env

  let exp_match env ae value = match ae, value with
    | CodeQueryAst.Null, Vval e -> Sil.exp_equal e Sil.exp_zero
    | CodeQueryAst.Null, _ -> false
    | CodeQueryAst.ConstString s, (Vfun pn) -> string_contains s (Procname.to_string pn)
    | CodeQueryAst.ConstString _, _ -> false
    | CodeQueryAst.Ident id, x ->
        env_add env id x

  let rec exp_list_match env ael vl = match ael, vl with
    | [], [] -> true
    | [], _:: _ -> false
    | _:: _, [] -> false
    | ae:: ael', v:: vl' ->
        if exp_match env ae v then exp_list_match env ael' vl'
        else false

  let opt_match match_elem env x_opt y = match x_opt with
    | None -> true
    | Some x -> match_elem env x y

  let binop_match op1 op2 = match op1, op2 with
    | Sil.Eq, "==" -> true
    | Sil.Ne, "!=" -> true
    | _ -> false

  let rec cond_match env idenv cond (ae1, op, ae2) = match cond with
    | Sil.BinOp (bop, _e1, _e2) ->
        let e1 = Idenv.expand_expr idenv _e1 in
        let e2 = Idenv.expand_expr idenv _e2 in
        binop_match bop op && exp_match env ae1 (Vval e1) && exp_match env ae2 (Vval e2)
    | Sil.UnOp (Sil.LNot, (Sil.BinOp (Sil.Eq, e1, e2)), _) ->
        cond_match env idenv (Sil.BinOp (Sil.Ne, e1, e2)) (ae1, op, ae2)
    | Sil.UnOp (Sil.LNot, (Sil.BinOp (Sil.Ne, e1, e2)), _) ->
        cond_match env idenv (Sil.BinOp (Sil.Eq, e1, e2)) (ae1, op, ae2)
    | _ -> false

  (** Iterate over the instructions of the linearly succ nodes. *)
  let rec iter_succ_nodes node iter =
    match Cfg.Node.get_succs node with
    | [node'] ->
        let instrs = Cfg.Node.get_instrs node in
        IList.iter (fun instr -> iter (node', instr)) instrs;
        iter_succ_nodes node' iter
    | [] -> ()
    | _:: _ -> ()

  let linereader = Printer.LineReader.create ()

  let print_action env action proc_name node loc = match action with
    | CodeQueryAst.Noaction ->
        L.stdout "%a@." pp_env env
    | CodeQueryAst.Source source_range_opt ->
        let x, y = match source_range_opt with
          | None -> 10, 10
          | Some (x, y) -> x, y in
        L.stdout "%a@." (Checkers.PP.pp_loc_range linereader x y) loc
    | CodeQueryAst.Error s_opt ->
        let err_name = match s_opt with
          | None -> "codequery"
          | Some s -> s in
        Err.add_error_to_spec proc_name err_name node loc

  let rec match_query show env idenv caller_pn (rule, action) proc_name node instr =
    match rule, instr with
    | CodeQueryAst.Call (ae1, ae2), Sil.Call (_, Sil.Const (Sil.Cfun pn), _, loc, _) ->
        if exp_match env ae1 (Vfun caller_pn) && exp_match env ae2 (Vfun pn) then
          begin
            if show then print_action env action proc_name node loc;
            true
          end
        else false
    | CodeQueryAst.Call _, _ -> false
    | CodeQueryAst.MethodCall (ae1, ae2, ael_opt),
      Sil.Call (_, Sil.Const (Sil.Cfun pn), (_e1, _):: params, loc, { Sil.cf_virtual = true }) ->
        let e1 = Idenv.expand_expr idenv _e1 in
        let vl = IList.map (function _e, _ -> Vval (Idenv.expand_expr idenv _e)) params in
        if exp_match env ae1 (Vval e1) && exp_match env ae2 (Vfun pn) && opt_match exp_list_match env ael_opt vl then
          begin
            if show then print_action env action proc_name node loc;
            true
          end
        else false
    | CodeQueryAst.MethodCall _, _ -> false
    | CodeQueryAst.If (ae1, op, ae2, body_rule), Sil.Prune (cond, loc, true_branch, _) ->
        if true_branch && cond_match env idenv cond (ae1, op, ae2) then
          begin
            let found = ref false in
            let iter (_, instr') =
              let env' = env_copy env in
              if not !found
              && match_query false env' idenv caller_pn (body_rule, action) proc_name node instr'
              then found := true in
            iter_succ_nodes node iter;
            let line_contains_null () =
              match Printer.LineReader.from_loc linereader loc with
              | None -> false
              | Some s -> string_contains "null" s in
            if !found && line_contains_null () (* TODO: this replaces lack of typing where null and 0 and false are the same *) then
              begin
                L.stdout "conditional %a@." (Sil.pp_exp pe_text) cond;
                print_action env action proc_name node loc
              end;
            !found
          end
        else false
    | CodeQueryAst.If _, _ -> false

end

let code_query_callback { Callbacks.proc_desc; idenv; proc_name } =
  let do_instr node instr =
    let env = Match.init_env () in
    let _found =
      Match.match_query true env idenv proc_name (Lazy.force query_ast) proc_name node instr in
    () in
  if verbose then L.stdout "code_query_callback on %a@." Procname.pp proc_name;
  Cfg.Procdesc.iter_instrs do_instr proc_desc;
  Err.update_summary proc_name proc_desc
