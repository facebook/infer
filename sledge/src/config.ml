(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Configuration options *)

let trace_conv =
  let default = Map.empty (module String) in
  let parse s =
    try
      let index_from s i =
        Option.merge ~f:min
          (String.index_from s i '+')
          (String.index_from s i '-')
      in
      let rec split s rev_parts i =
        match index_from s (i + 1) with
        | Some j when j = i -> split s rev_parts j
        | Some j ->
            split s (String.sub s ~pos:i ~len:(j - i) :: rev_parts) j
        | _ -> List.rev (String.subo s ~pos:i :: rev_parts)
      in
      let parts = split s [] 0 in
      List.fold parts ~init:default ~f:(fun m part ->
          let parse_part part =
            let sign, rest =
              match part.[0] with
              | '-' -> (false, String.subo part ~pos:1)
              | '+' -> (true, String.subo part ~pos:1)
              | _ -> (true, part)
            in
            assert (not (String.is_empty rest)) ;
            assert (Char.is_uppercase rest.[0]) ;
            match String.lsplit2 rest ~on:'.' with
            | Some (mod_name, fun_name) ->
                assert (Char.is_lowercase fun_name.[0]) ;
                (mod_name, Some fun_name, sign)
            | None -> (rest, None, sign)
          in
          match parse_part part with
          | mod_name, Some fun_name, enabled ->
              let {trace_mod; trace_funs} =
                match Map.find m mod_name with
                | Some c -> c
                | None -> {trace_mod= None; trace_funs= default}
              in
              Map.set m ~key:mod_name
                ~data:
                  { trace_mod
                  ; trace_funs=
                      Map.set trace_funs ~key:fun_name ~data:enabled }
          | mod_name, None, enabled ->
              Map.set m ~key:mod_name
                ~data:{trace_mod= Some enabled; trace_funs= default} )
      |> fun x -> Ok x
    with _ -> Error (`Msg ("Invalid trace spec: " ^ s))
  in
  let print fs c =
    let pf fmt = Format.fprintf fs fmt in
    Map.iteri c ~f:(fun ~key:mod_name ~data:{trace_mod; trace_funs} ->
        ( match trace_mod with
        | Some true -> pf "+%s" mod_name
        | Some false -> pf "-%s" mod_name
        | None -> () ) ;
        Map.iteri trace_funs ~f:(fun ~key:fun_name ~data:fun_enabled ->
            if fun_enabled then pf "+%s.%s" mod_name fun_name
            else pf "-%s.%s" mod_name fun_name ) )
  in
  (parse, print)

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
  ; trace: Trace.trace_mods_funs
         [@aka ["t"]] [@docv "spec"] [@conv trace_conv]
        (** Enable debug tracing according to $(i,spec), which is a sequence
            of module and function names separated by $(b,+) or $(b,-). For
            example, $(b,Control-Control.exec_inst) enables all tracing in
            the $(b,Control) module except the $(b,Control.exec_inst)
            function. *)
  ; trace_all: bool [@aka ["v"]]  (** Enable all debug tracing. *) }
[@@deriving cmdliner]

let run main =
  let info = Cmdliner.Term.info "sledge" ~version:Version.version in
  Cmdliner.Term.eval (cmdliner_term (), info)
  |> function
  | `Error _ -> Caml.exit 1
  | `Help | `Version -> Caml.exit 0
  | `Ok {compile_only; input; output; trace; trace_all} ->
      Trace.init ~config:{trace_all; trace_mods_funs= trace} () ;
      main ~input ~output ~compile_only
