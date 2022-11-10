(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

%start _body

(* Doli-body-specific keywords *)
%token BODYKW

(* abbreviations -- will disappear eventually *)
%token BODYSTUB

%%

_body:
     BODYKW;
	 LBRACE; bdStub=bodyStub; RBRACE;
     { bdStub }
     ;

bodyStub:
	| BODYSTUB { Body }
	;
