(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Extension point rewriter for debug trace logging

    This ppx rewriter reads a cookie to determine whether to rewrite in
    "debug" mode or not. To enable "debug" mode, pass
    [--cookie 'ppx_trace_enabled="1"'] (or with [true] instead or [1]).

    It rewrites [\[%trace\] ~call ~retn ~rais] to a call
    [Trace.trace ~call ~retn ~rais mod_name fun_name] where [mod_name] and
    [fun_name] are the enclosing module and function names in the parsetree.
    This is only done in debug mode, otherwise
    [\[%trace\] ~call ~retn ~rais] is rewritten to [(fun k -> k ())].

    Similarly, [\[%Trace.info\]], [\[%Trace.infok\]], [\[%Trace.printf\]],
    [\[%Trace.fprintf\]], [\[%Trace.kprintf\]], and [\[%Trace.call\]] are
    rewritten to their analogues in the [Trace] module, or [()]; and
    [\[%Trace.retn\]] is rewritten to a call to [Trace.retn] or
    [(fun x -> x)].

    For example, this enables writing

    {[
      let func arg =
        [%trace]
          ~call:(fun {pf} -> pf "@ %a" pp_arg_type arg)
          ~retn:(fun {pf} -> pf "%a" pp_result_type)
        @@ fun () -> func arg
    ]}

    or

    {[
      let func arg =
        [%Trace.call fun {pf} -> pf "@ %a" pp_arg_type arg]
        ;
        func arg
        |>
        [%Trace.retn fun {pf} -> pf "%a" pp_result_type]
    ]}

    to trace calls to [func] in debug mode while completely compiling out
    the debug code in non-debug builds.

    This mechanism can also be used e.g. to dynamically check assertions
    only in debug mode.

    Additionally, [\[%debug\]] is rewritten to the compile-time boolean
    constant indicating if rewriting was done in debug mode. *)

open Ppxlib
open Ast_builder.Default

let debug = ref false

;;
Driver.Cookies.add_simple_handler "ppx_trace_enabled" Ast_pattern.__
  ~f:(function
  | Some {pexp_desc= Pexp_constant (Pconst_string (("1" | "true"), _))} ->
      debug := true
  | _ -> () )

let expand_debug ~ctxt =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  ebool ~loc !debug

let debug_extension =
  Extension.V3.declare "debug" Extension.Context.expression
    (Ast_pattern.pstr Ast_pattern.nil)
    expand_debug

let debug_rule = Context_free.Rule.extension debug_extension

;;
Driver.register_transformation ~rules:[debug_rule] "debug"

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

(* (fun x -> x) *)
let fun_id loc = pexp_fun ~loc Nolabel None (pvar ~loc "x") (evar ~loc "x")

(* (fun k -> k ()) *)
let fun_go loc =
  pexp_fun ~loc Nolabel None (pvar ~loc "k")
    (eapply ~loc (evar ~loc "k") [eunit ~loc])

let mapper =
  object
    inherit Ast_traverse.map as super

    method! value_binding vb =
      vb_stack_with vb.pvb_pat ~f:(fun () -> super#value_binding vb)

    method! expression exp =
      let append_here_args args =
        let mod_name = evar ~loc:Location.none "Stdlib.__MODULE__" in
        let fun_name =
          estring ~loc:Location.none (get_fun_name (vb_stack_top ()))
        in
        (Nolabel, mod_name) :: (Nolabel, fun_name) :: args
      in
      match exp.pexp_desc with
      | Pexp_apply
          ( { pexp_desc= Pexp_extension ({txt= "trace"; loc}, PStr [])
            ; pexp_loc }
          , args ) ->
          if not !debug then fun_go pexp_loc
          else
            pexp_apply ~loc:exp.pexp_loc (evar ~loc "Trace.trace")
              (append_here_args args)
      | Pexp_extension
          ( { txt=
                ( "Trace.info" | "Trace.infok" | "Trace.printf"
                | "Trace.fprintf" | "Trace.kprintf" ) as txt
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
          if not !debug then fun_id exp.pexp_loc
          else
            pexp_apply ~loc:exp.pexp_loc
              (evar ~loc:retn_loc "Trace.retn")
              (append_here_args [(Nolabel, retn_fun)])
      | _ -> super#expression exp
  end

let impl = mapper#structure

;;
Driver.register_transformation "trace" ~impl
