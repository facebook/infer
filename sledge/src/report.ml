(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Issue reporting *)

let unknown_call call =
  [%Trace.kprintf
    Stop.on_unknown_call
      "@\n@[<v 2>%a Unknown function call %a@;<1 2>@[%a@]@]@."
      (fun fs call -> Loc.pp fs (Llair.Term.loc call))
      call
      (fun fs (call : Llair.Term.t) ->
        match call with
        | Call {callee} -> (
          match Var.of_exp callee with
          | Some var -> Var.pp_demangled fs var
          | None -> Exp.pp fs callee )
        | _ -> () )
      call Llair.Term.pp call]

let invalid_access state pp access loc =
  let rep fs =
    Format.fprintf fs "%a Invalid memory access@;<1 2>@[%a@]" Loc.pp
      (loc access) pp access
  in
  Format.printf "@\n@[<v 2>%t@]@." rep ;
  [%Trace.printf
    "@\n@[<v 2>%t@;<1 2>@[{ %a@ }@]@]@." rep State_domain.pp state] ;
  Stop.on_invalid_access ()

let invalid_access_inst state inst =
  invalid_access state Llair.Inst.pp inst Llair.Inst.loc

let invalid_access_term state term =
  invalid_access state Llair.Term.pp term Llair.Term.loc
