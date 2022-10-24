(*  Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

(* start change *)
%start _summary
(* end change *)

(* Doli-summary-specific keywords *)
%token SUMMARYKW
(* abbreviations -- will disappear eventually *)
%token SUMMARYSTUB
(* start *)
(*
%start _summary
*)
%%


_summary:
     SUMMARYKW;
	 LBRACE; sm=summaryStub; RBRACE;
     { sm }
     ;

summaryStub:
	| SUMMARYSTUB { Summary }
	;
