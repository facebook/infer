(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface
open PulseDomainInterface

(* Regular expression to match a Hack function call like foo(...) *)
let re_unawaited_awaitable =
  let open Re in
  let identifier = rep1 (alt [alnum; char '_']) in
  let function_name = seq [lower; rep (alt [alnum; char '_'])] in
  let constructor = seq [alt [alnum; char '_']] in
  (* Matches function parameters (anything inside ()) *)
  let params =
    rep
      (alt
         [ diff any (set "()")
         ; (* Match anything except `(` and `)` *)
           seq [char '('; rep any; char ')'] (* Allow nested parentheses *) ] )
  in
  compile
  @@ alt
       [ (* Match $x->foo(...) *)
         seq [str "$"; identifier; str "->"; function_name; str "("; params; str ")"]
       ; (* Match C::foo(...) *)
         seq [identifier; str "::"; function_name; str "("; params; str ")"]
       ; (* Match foo(...) *)
         seq [alt [bos; space]; group (seq [function_name; str "("; params; str ")"])]
       ; (* Match (new C())->gen() *)
         seq
           [ str "("
           ; str "new"
           ; rep space
           ; constructor
           ; params
           ; (* Match (new C()) *)
             str ")"
           ; str "->"
           ; identifier
           ; str "("
           ; params
           ; str ")" ] ]


let make_autofix ?(is_alloc_loc = false) {Location.file; line; col} ~replacer =
  let line_str =
    let file_path = SourceFile.to_abs_path file in
    match ISys.is_file file_path with
    | `No | `Unknown ->
        None
    | `Yes ->
        Utils.with_file_in file_path ~f:(fun in_chan ->
            Container.fold_until in_chan
              ~fold:(In_channel.fold_lines ~fix_win_eol:false)
              ~finish:(fun _final_line_number -> None)
              ~init:1
              ~f:(fun line_number line_str ->
                if Int.equal line_number line then Stop (Some line_str)
                else Continue (line_number + 1) ) )
  in
  match line_str with
  | None ->
      []
  | Some line_str ->
      List.filter_opt
        (List.map (replacer line_str) ~f:(fun (original, replacement) ->
             if
               Bool.equal is_alloc_loc false
               && String.is_substring_at line_str ~pos:(col - 1) ~substring:original
             then
               Some
                 {Jsonbug_t.original= Some original; replacement= Some replacement; additional= None}
             else
               Option.map (String.substr_index line_str ~pattern:original) ~f:(fun idx ->
                   { Jsonbug_t.original= None
                   ; replacement= None
                   ; additional= Some [{line; column= idx + 1; original; replacement}] } ) ) )


let check_balanced_parenthesis str =
  let rec check str index depth =
    if index >= String.length str then Int.equal depth 0
    else
      match str.[index] with
      | '(' ->
          check str (index + 1) (depth + 1)
      | ')' ->
          if Int.equal depth 0 then false else check str (index + 1) (depth - 1)
      | _ ->
          check str (index + 1) depth
  in
  check str 0 0


let get_autofix pdesc diagnostic =
  match (diagnostic : Diagnostic.t) with
  | UnnecessaryCopy {copied_into; source_opt; location; copied_location= None} -> (
      let is_formal pvar =
        let pvar_name = Pvar.get_name pvar in
        List.exists (Procdesc.get_formals pdesc) ~f:(fun (formal, _, _) ->
            Mangled.equal pvar_name formal )
      in
      let is_local pvar =
        let pvar_name = Pvar.get_name pvar in
        List.exists (Procdesc.get_locals pdesc) ~f:(fun ProcAttributes.{name= local} ->
            Mangled.equal pvar_name local )
      in
      match (copied_into, source_opt) with
      | IntoField {field}, Some (DecompilerExpr.PVar pvar, [Dereference])
        when Procname.is_constructor (Procdesc.get_proc_name pdesc) && is_formal pvar ->
          let param = Pvar.to_string pvar in
          make_autofix location ~replacer:(fun _ ->
              [ ( F.asprintf "%a(%s)" Fieldname.pp field param
                , F.asprintf "%a(std::move(%s))" Fieldname.pp field param )
              ; ( F.asprintf "%a{%s}" Fieldname.pp field param
                , F.asprintf "%a{std::move(%s)}" Fieldname.pp field param )
              ; ( F.asprintf "%a = %s;" Fieldname.pp field param
                , F.asprintf "%a = std::move(%s);" Fieldname.pp field param ) ] )
      | IntoField {field}, Some (DecompilerExpr.PVar pvar, []) when is_local pvar ->
          let param = Pvar.to_string pvar in
          make_autofix location ~replacer:(fun _ ->
              [ ( F.asprintf "%a = %s;" Fieldname.pp field param
                , F.asprintf "%a = std::move(%s);" Fieldname.pp field param )
              ; ( F.asprintf ".%a = %s," Fieldname.pp field param
                , F.asprintf ".%a = std::move(%s)," Fieldname.pp field param ) ] )
      | IntoVar {copied_var= ProgramVar pvar}, Some (DecompilerExpr.PVar _, [MethodCall _]) ->
          let tgt = Pvar.to_string pvar in
          make_autofix location ~replacer:(fun _ ->
              [(F.asprintf "auto %s = " tgt, F.asprintf "auto& %s = " tgt)] )
      | _ ->
          [] )
  | ResourceLeak {resource= Awaitable; allocation_trace} ->
      if Language.curr_language_is Hack then
        let allocation_loc = Trace.get_outer_location allocation_trace in
        let is_alloc_loc = true in
        make_autofix ~is_alloc_loc allocation_loc ~replacer:(fun line_str ->
            let pos = allocation_loc.col - 1 in
            let input =
              if pos >= 0 then String.sub line_str ~pos ~len:(String.length line_str - pos)
              else line_str
            in
            let matches = Re.matches re_unawaited_awaitable input in
            match matches with
            | [] ->
                []
            | matched :: _ ->
                if check_balanced_parenthesis matched then
                  let matched = String.chop_prefix_if_exists matched ~prefix:" " in
                  let new_str = "await " ^ matched in
                  if String.is_substring line_str ~substring:new_str then []
                  else [(matched, new_str)]
                else [] )
      else []
  | _ ->
      []
