(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Configuration options *)

let trace_conv =
  let print fs {trace_all; trace_mods_funs} =
    let pf fmt = Format.fprintf fs fmt in
    if trace_all then pf "*"
    else
      Map.iteri trace_mods_funs
        ~f:(fun ~key:mod_name ~data:{trace_mod; trace_funs} ->
          ( match trace_mod with
          | Some true -> pf "+%s" mod_name
          | Some false -> pf "-%s" mod_name
          | None -> () ) ;
          Map.iteri trace_funs ~f:(fun ~key:fun_name ~data:fun_enabled ->
              if fun_enabled then pf "+%s.%s" mod_name fun_name
              else pf "-%s.%s" mod_name fun_name ) )
  in
  (Trace.parse, print)

type t =
  { compile_only: bool
         [@aka ["c"]]
        (** Do not analyze: terminate after translating input LLVM to LLAIR. *)
  ; input: string
         [@pos 0] [@docv "input.bc"]
        (** LLVM bitcode file to analyze, in either binary $(b,.bc) or
            textual $(b,.ll) form. *)
  ; output: string option
         [@aka ["o"]] [@docv "output.llair"]
        (** Dump $(i,input.bc) translated to LLAIR in human-readable form to
            $(i,output.llair), or $(b,-) for $(b,stdout). *)
  ; trace: Trace.config
         [@aka ["t"]]
         [@docv "spec"]
         [@conv trace_conv]
         [@default Trace.none]
        (** Enable debug tracing according to $(i,spec), which is a sequence
            of module and function names separated by $(b,+) or $(b,-). For
            example, $(b,Control-Control.exec_inst) enables all tracing in
            the $(b,Control) module except the $(b,Control.exec_inst)
            function. The $(i,spec) value $(b,* )enables all debug tracing. *)
  }
[@@deriving cmdliner]

let run main =
  let info = Cmdliner.Term.info "sledge" ~version:Version.version in
  Cmdliner.Term.eval (cmdliner_term (), info)
  |> function
  | `Error _ -> Caml.exit 1
  | `Help | `Version -> Caml.exit 0
  | `Ok {compile_only; input; output; trace} ->
      Trace.init ~config:trace () ;
      main ~input ~output ~compile_only
