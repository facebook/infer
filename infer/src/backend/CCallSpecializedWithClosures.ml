(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let replace_with_specialize_methods instr =
  match instr with
  | Sil.Call (ret, Exp.Const (Const.Cfun callee_pname), actual_params, loc, flags) -> (
      let args =
        List.map actual_params ~f:(function
          | Exp.Closure c, _ ->
              let captured =
                List.map c.captured_vars ~f:(fun (_, pvar, typ, capture_mode) ->
                    CapturedVar.{pvar; typ; capture_mode} )
              in
              Some (ProcAttributes.Block (c.name, captured))
          | _ ->
              None )
      in
      match
        BlockSpecialization.create_specialized_procdesc callee_pname
          ~extra_formals_to_blocks:Pvar.Map.empty ~captured_actuals:[] ~arg_actuals:args
      with
      | Some specialized_pname ->
          Sil.Call (ret, Exp.Const (Const.Cfun specialized_pname), actual_params, loc, flags)
      | None ->
          instr )
  | _ ->
      instr


let process proc_desc =
  (* For each procdesc:
     1. If we are a specialized procdesc:
      1.1. Copy original procdesc
      1.2. Update blocks' uses
     2. Update calls' closure arguments
     3. Update calls with closures as arguments to specialized calls:
      3.1. Create procdescs for specialized callees if they don't already exist
     4. Update closure calls
  *)
  (* 1.
      1.1. Create a specialized copy of [proc_desc]'s [orig_pdesc] (if it exists).
      1.2. Specialize at instruction level: [foo(f)] when [f = block] replaces every
           use of [f] (calls and as argument) with the corresponding [block] and its
           captured variables. *)
  ClosureSubstSpecializedMethod.process proc_desc ;
  (* 2.
      Replace each indirect use of closure (as argument) with a direct use of closure.
     e.g.
      [foo(a, b, c)] when b = Closure(closure_b, ...) becomes
      [foo(a, Closure(closure_b, ...), c)] *)
  ClosuresSubstitution.process_closure_param proc_desc ;
  (* 3.
      Replace every function call taking a known closure as argument with specialized calls
      and (3.1.) create the corresponding (empty for now) specialized procdescs.
     e.g.
      [foo(a, Closure(closure_b, captured_vars...), c)] becomes
      [foo\[closure_b\](a, Closure(closure_b, captured_vars...), c)]
      and the procdesc for [foo\[closure_b\]\ is created.
     Note:
      The newly created procdescs will be filled out by [ClosureSubstSpecializedMethod.process]
      in step 1. above when it will be their turn to be (pre)processed. *)
  Procdesc.replace_instrs proc_desc ~f:(fun _node instr -> replace_with_specialize_methods instr)
  |> ignore ;
  (* 4.
      Replace [ident] calls by [closure] calls if [ident = closure] or if any of the arguments is
      a known [closure].
     e.g.
      - [id(a, b, c)] when [id = Closure(closure_id, captured_vars...)] becomes
        [Closure(closure_id, captured_vars...)(a, b, c)]
      - [foo\[closure_b\](a, Closure(closure_b, captured_vars...), c)] becomes
        [Closure(foo\[closure_b\], captured_vars...)(a, Closure(closure_b, captured_vars...), c)] *)
  ClosuresSubstitution.process_closure_call proc_desc
