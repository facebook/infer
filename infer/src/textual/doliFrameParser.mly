(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

%{
open DoliAst
%}

(* types of imported parsers *)
%type <DoliAst.matching> _javaMatch
%type <DoliAst.matching> _objCMatch
%type <DoliAst.doliBody> _body
%type <DoliAst.doliProgram>  doliProgram
(* start *)
%start doliProgram
%%

doliProgram:
 | dis = doliInstruction* EOF { DoliProgram dis }
 ;

 doliInstruction:
	| IN; Java jm = _javaMatch bd= _body
	{  { match_ = jm; body = bd }  }
	| IN; ObjC ocm = _objCMatch bd= _body
	{   { match_ = ocm; body = bd }  }
	;
