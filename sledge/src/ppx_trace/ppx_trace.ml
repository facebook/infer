(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Extension point rewriter for debug trace logging

    This ppx rewriter declares a [--debug] command line option, to be passed
    by the build system in debug but not optimized build modes.

    It rewrites [[%Trace.info f]] to a call [Trace.info mod_name fun_name f]
    where [mod_name] and [fun_name] are the enclosing module and function
    names in the parsetree. This is only done in debug mode, if [--debug] is
    not passed, then [[%Trace.info f]] is rewritten to [()].

    Similarly, [[%Trace.call]] is rewritten to a call to [Trace.call] or
    [()], and [[%Trace.retn]] to a call to [Trace.retn] or [Fn.id].

    For example, this enables writing

    [%Trace.call fun {pf} -> pf "%a" pp_arg_type arg] ; func arg |>
    [%Trace.retn fun {pf} -> pf "%a" pp_result_type]

    to trace calls to [f] in debug mode while completely compiling out the
    debug code in non-debug builds.

    This mechanism can also be used e.g. to dynamically check assertions
    only in debug mode.

    Additionally, [[%debug]] is rewritten to the compile-time boolean
    constant determined by whether or not [--debug] is passed. *)

open Ppxlib
open Ast_builder.Default
module Ast_mapper = Selected_ast.Ast.Ast_mapper

let debug = ref false

;;
Driver.add_arg "--debug" (Caml.Arg.Set debug)
  ~doc:"Enable debug tracing output"

let rec get_fun_name pat =
  match pat.ppat_desc with
  | Ppat_var {txt; _} -> txt
  | Ppat_alias (pat, _) | Ppat_constraint (pat, _) -> get_fun_name pat
  | _ ->
      Location.raise_errorf ~loc:pat.ppat_loc
        "Unexpected pattern in binding containing [%%Trace]: %a"
        (fun f p ->
          Ocaml_common.Pprintast.pattern f
            (Selected_ast.To_ocaml.copy_pattern p) )
        pat

let vb_stack_with, vb_stack_top =
  let stack = ref [] in
  let with_ x ~f =
    stack := x :: !stack ;
    let r = f () in
    stack := List.tl !stack ;
    r
  in
  let top () = List.hd !stack in
  (with_, top)

let mapper =
  let value_binding (m : Ast_mapper.mapper) vb =
    vb_stack_with vb.pvb_pat ~f:(fun () ->
        Ast_mapper.default_mapper.value_binding m vb )
  in
  let expr (m : Ast_mapper.mapper) exp =
    let append_here_args args =
      let mod_name = evar ~loc:Location.none "Caml.__MODULE__" in
      let fun_name =
        estring ~loc:Location.none (get_fun_name (vb_stack_top ()))
      in
      (Nolabel, mod_name) :: (Nolabel, fun_name) :: args
    in
    match exp.pexp_desc with
    | Pexp_extension ({txt= "debug"; loc}, PStr []) -> ebool ~loc !debug
    | Pexp_extension
        ( { txt=
              ( "Trace.info" | "Trace.printf" | "Trace.fprintf"
              | "Trace.kprintf" ) as txt
          ; loc }
        , PStr [{pstr_desc= Pstr_eval (arg, []); _}] ) ->
        if not !debug then eunit ~loc:exp.pexp_loc
        else
          let args =
            match arg.pexp_desc with
            | Pexp_apply (op, args) -> (Nolabel, op) :: args
            | _ -> [(Nolabel, arg)]
          in
          pexp_apply ~loc:exp.pexp_loc (evar ~loc txt)
            (append_here_args args)
    | Pexp_extension
        ( {txt= "Trace.call"; loc= call_loc}
        , PStr [{pstr_desc= Pstr_eval (call_fun, []); _}] ) ->
        if not !debug then eunit ~loc:exp.pexp_loc
        else
          pexp_apply ~loc:exp.pexp_loc
            (evar ~loc:call_loc "Trace.call")
            (append_here_args [(Nolabel, call_fun)])
    | Pexp_extension
        ( {txt= "Trace.retn"; loc= retn_loc}
        , PStr [{pstr_desc= Pstr_eval (retn_fun, []); _}] ) ->
        if not !debug then evar ~loc:exp.pexp_loc "Fn.id"
        else
          pexp_apply ~loc:exp.pexp_loc
            (evar ~loc:retn_loc "Trace.retn")
            (append_here_args [(Nolabel, retn_fun)])
    | _ -> Ast_mapper.default_mapper.expr m exp
  in
  {Ast_mapper.default_mapper with expr; value_binding}

let impl = Selected_ast.Ast.map_structure mapper

;;
Driver.register_transformation "trace" ~impl
