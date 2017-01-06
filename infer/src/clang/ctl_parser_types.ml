(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd


(* Types used by the ctl parser *)

(* "set" clauses are used for defining mandatory variables that will be used
   by when reporting issues: eg for defining the condition.

   "desc" clauses are used for defining the error message,
   the suggestion, the severity.

   "let" clauses are used to define temporary formulas which are then
   used to abbreviate the another formula. For example

   let f = a And B

   set formula  = f OR f

   set message = "bla"

*)
type clause =
  | CLet  of string * CTL.t (* Let clause: let id = definifion;  *)
  | CSet of string * CTL.t (* Set clause: set id = definition *)
  | CDesc of string * string (* Description clause eg: set message = "..." *)

type ctl_checker = {
  name : string; (* Checker's name *)
  definitions : clause list (* A list of let/set definitions *)
}

let infer_prefix = "__infer_ctl_"
let formula_id_const = infer_prefix ^ "formula_id__"
let report_when_const = "report_when"
let message_const = "message"
let suggestion_const = "suggestion"
let severity_const = "severity"
let mode_const = "mode"

let print_checker c =
  Logging.out "\n-------------------- \n";
  Logging.out "\nChecker name: %s\n" c.name;
  IList.iter (fun d -> (match d with
      | CSet (clause_name, phi)
      | CLet (clause_name, phi) ->
          Logging.out "    %s=  \n    %a\n\n"
            clause_name CTL.Debug.pp_formula phi
      | CDesc (clause_name, s) ->
          Logging.out "    %s=  \n    %s\n\n" clause_name s)
    ) c.definitions;
  Logging.out "\n-------------------- \n"
