(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Extension point rewriter for debug trace logging

    This ppx rewriter reads a cookie to determine whether to rewrite in
    "debug" mode or not. To enable "debug" mode, pass
    [--cookie 'ppx_dbg_enabled="1"'] (or with [true] instead or [1]).

    It rewrites [\[%dbg\] ~call ~retn ~rais] to a call
    [Dbg.dbg ~call ~retn ~rais fun_name] where [fun_name] is the value of
    [__FUNCTION__] at the call site. This is only done in debug mode,
    otherwise [\[%dbg\] ~call ~retn ~rais] is rewritten to
    [(fun k -> k ())].

    Similarly, it rewrites [\[%dbgs\] ~call ~retn ~rais] to a call
    [Dbg.dbgs ~call ~retn ~rais fun_name]. This is only done in debug mode,
    otherwise [\[%dbgs\] ~call ~retn ~rais] is rewritten to [(fun x -> x)].

    Similarly, [\[%Dbg.info\]], [\[%Dbg.infok\]], [\[%Dbg.printf\]],
    [\[%Dbg.fprintf\]], [\[%Dbg.kprintf\]], and [\[%Dbg.call\]] are
    rewritten to their analogues in the [Dbg] module, or [()]; and
    [\[%Dbg.retn\]] is rewritten to a call to [Dbg.retn] or [(fun x -> x)].

    For example, this enables writing

    {[
      let func arg =
        [%dbg]
          ~call:(fun {pf} -> pf "@ %a" pp_arg_type arg)
          ~retn:(fun {pf} -> pf "%a" pp_result_type)
        @@ fun () -> func arg
    ]}

    or

    {[
      let func arg =
        [%Dbg.call fun {pf} -> pf "@ %a" pp_arg_type arg]
        ;
        func arg
        |>
        [%Dbg.retn fun {pf} -> pf "%a" pp_result_type]
    ]}

    to trace calls to [func] in debug mode while completely compiling out
    the debug code in non-debug builds.

    This mechanism can also be used e.g. to dynamically check assertions
    only in debug mode.

    Additionally, [\[%debug\]] is rewritten to the compile-time boolean
    constant indicating if rewriting was done in debug mode. *)

open Ppxlib
open Ast_builder.Default

let debug = ref false ;;

Driver.Cookies.add_simple_handler "ppx_dbg_enabled" Ast_pattern.__
  ~f:(function
  | Some {pexp_desc= Pexp_constant (Pconst_string (("1" | "true"), _, _))}
    ->
      debug := true
  | _ -> () )

let expand_debug ~ctxt =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  ebool ~loc !debug

let debug_extension =
  Extension.V3.declare "debug" Extension.Context.expression
    (Ast_pattern.pstr Ast_pattern.nil)
    expand_debug

let debug_rule = Context_free.Rule.extension debug_extension ;;

Driver.register_transformation ~rules:[debug_rule] "debug"

let mapper =
  object (self)
    inherit Ast_traverse.map as super

    method! expression exp =
      let append_here_args args =
        let fun_name = evar ~loc:Location.none "Stdlib.__FUNCTION__" in
        (Nolabel, fun_name) :: args
      in
      match exp.pexp_desc with
      (* [%dbg] dbg_args @@ fun () -> body *)
      | Pexp_apply
          ( {pexp_desc= Pexp_ident {txt= Lident "@@"}}
          , [ ( Nolabel
              , { pexp_desc=
                    Pexp_apply
                      ( { pexp_desc=
                            Pexp_extension ({txt= "dbg"; loc}, PStr []) }
                      , dbg_args ) } )
            ; ( Nolabel
              , ( { pexp_desc=
                      Pexp_fun
                        ( Nolabel
                        , None
                        , { ppat_desc=
                              Ppat_construct ({txt= Lident "()"}, None) }
                        , body ) } as arg ) ) ] )
      (* [%dbg] dbg_args (fun () -> body) *)
       |Pexp_apply
          ( { pexp_desc=
                Pexp_apply
                  ( {pexp_desc= Pexp_extension ({txt= "dbg"; loc}, PStr [])}
                  , dbg_args ) }
          , [ ( Nolabel
              , ( { pexp_desc=
                      Pexp_fun
                        ( Nolabel
                        , None
                        , { ppat_desc=
                              Ppat_construct ({txt= Lident "()"}, None) }
                        , body ) } as arg ) ) ] ) ->
          if not !debug then self#expression body
          else
            pexp_apply ~loc:exp.pexp_loc (evar ~loc "Dbg.dbg")
              (append_here_args
                 (dbg_args @ [(Nolabel, self#expression arg)]) )
      (* [%dbgs] dbg_args @@ body *)
      | Pexp_apply
          ( {pexp_desc= Pexp_ident {txt= Lident "@@"}}
          , [ ( Nolabel
              , { pexp_desc=
                    Pexp_apply
                      ( { pexp_desc=
                            Pexp_extension ({txt= "dbgs"; loc}, PStr []) }
                      , dbg_args ) } )
            ; (Nolabel, body) ] )
      (* [%dbgs] dbg_args body *)
       |Pexp_apply
          ( { pexp_desc=
                Pexp_apply
                  ( {pexp_desc= Pexp_extension ({txt= "dbgs"; loc}, PStr [])}
                  , dbg_args ) }
          , [(Nolabel, body)] ) ->
          if not !debug then self#expression body
          else
            pexp_apply ~loc:exp.pexp_loc (evar ~loc "Dbg.dbgs")
              (append_here_args
                 (dbg_args @ [(Nolabel, self#expression body)]) )
      | Pexp_extension
          ( { txt=
                ( "Dbg.info" | "Dbg.infok" | "Dbg.printf" | "Dbg.fprintf"
                | "Dbg.kprintf" ) as txt
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
          ( {txt= "Dbg.call"; loc}
          , PStr [{pstr_desc= Pstr_eval (call_fun, []); _}] ) ->
          if not !debug then eunit ~loc:exp.pexp_loc
          else
            pexp_apply ~loc:exp.pexp_loc (evar ~loc "Dbg.call")
              (append_here_args [(Nolabel, call_fun)])
      (* body |> [%Dbg.retn retn_fun] *)
      | Pexp_apply
          ( {pexp_desc= Pexp_ident {txt= Lident "|>"}}
          , [ (Nolabel, body)
            ; ( Nolabel
              , { pexp_desc=
                    Pexp_extension
                      ( {txt= "Dbg.retn"; loc}
                      , PStr [{pstr_desc= Pstr_eval (retn_fun, [])}] ) } )
            ] )
      (* [%Dbg.retn retn_fun] body *)
       |Pexp_apply
          ( { pexp_desc=
                Pexp_extension
                  ( {txt= "Dbg.retn"; loc}
                  , PStr [{pstr_desc= Pstr_eval (retn_fun, [])}] ) }
          , [(Nolabel, body)] ) ->
          if not !debug then self#expression body
          else
            pexp_apply ~loc:exp.pexp_loc (evar ~loc "Dbg.retn")
              (append_here_args
                 [(Nolabel, retn_fun); (Nolabel, self#expression body)] )
      | _ -> super#expression exp
  end

let impl = mapper#structure ;;

Driver.register_transformation "dbg" ~impl
