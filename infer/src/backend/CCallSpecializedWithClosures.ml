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
              BlockSpecialization.Block (c.name, captured)
          | _ ->
              BlockSpecialization.Var )
      in
      match BlockSpecialization.create_specialized_procdesc callee_pname args with
      | Some specialized_pname ->
          Sil.Call (ret, Exp.Const (Const.Cfun specialized_pname), actual_params, loc, flags)
      | None ->
          instr )
  | _ ->
      instr


let process proc_desc =
  (* For each procdesc:
     1. If we are a specialized pdesc:
      1.1. Copy original procdesc
      1.2. Update blocks' uses (at instruction level)
     2. Update calls' closure arguments (at expression level)
     3. Update calls with closures as arguments to specialized calls
     4. Create procdescs for specialized callees if they don't already exist
     5. Update closure calls (at expression level)
  *)
  (* Create a specialized copy of proc_desc's orig_pdesc (if it exists).
     Specialize at instruction level: [foo(f)] when [f = block] replaces every
     use of [f] (calls and as argument) with the corresponding [block] and its
     captured arguments. *)
  ClosureSubstSpecializedMethod.process proc_desc ;
  (* Replace each indirect use of closure (as argument) with a direct use of closure.
     e.g. [foo(a, b, c)] when b = Closure(closure_b, ...) becomes
          [foo(a, Closure(closure_b, ...), c)] *)
  ClosuresSubstitution.process_closure_param proc_desc ;
  (* Replace every function call taking a known closure as argument with specialized calls
     and create the corresponding (empty for now) specialized pdescs.
     e.g. [foo(a, Closure(closure_b, captured_args...), c)] becomes
      [foo\[closure_b\](a, captured_args..., Closure(closure_b, captured_args...), c)]
      and the pdesc for [foo\[closure_b\]\ is created.
     Note:
      Captured args placement in the list of args depends on the set ordering).
      The newly created pdescs (stored in [need_specalization]) will be filled out by
      [ClosureSubstSpecializedMethod.process] above when it will be their turn to be
      (pre)processed.
  *)
  Procdesc.replace_instrs proc_desc ~f:(fun _node instr -> replace_with_specialize_methods instr)
  |> ignore ;
  (* Replace [ident] calls by [closure] calls with added [closure]'s arguments
     if [ident = closure].
     e.g. [foo(a, b, c)] when [foo = Closure(closure_foo, captured_args...)] becomes
          [closure_foo(captured_args..., a, b, c)] *)
  ClosuresSubstitution.process_closure_call proc_desc
