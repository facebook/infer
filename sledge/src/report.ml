(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Issue reporting *)

let unknown_call call =
  [%Trace.kprintf
    (fun _ -> assert false)
      "@\n\
       @[<v 2>%a Called unknown function %a executing instruction@;<1 \
       2>@[%a@]@]@."
      (fun fs call -> Loc.pp fs (Llair.Term.loc call))
      call
      (fun fs (call : Llair.Term.t) ->
        match call with
        | Call {call= {dst}} -> (
          match Var.of_exp dst with
          | Some var -> Var.pp_demangled fs var
          | None -> Exp.pp fs dst )
        | _ -> () )
      call Llair.Term.pp call]

let invalid_access inst state =
  Format.printf
    "@\n\
     @[<v 2>%a Invalid memory access executing instruction@;<1 2>@[%a@]@]@."
    Loc.pp (Llair.Inst.loc inst) Llair.Inst.pp inst ;
  [%Trace.kprintf
    (fun _ -> assert false)
      "@\n\
       @[<v 2>%a Invalid memory access executing instruction@;<1 2>@[%a@]@ \
       from symbolic state@;<1 2>@[{ %a@ }@]@]@."
      Loc.pp (Llair.Inst.loc inst) Llair.Inst.pp inst Domain.pp state]
