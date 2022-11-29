(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

%{
	open DoliObjCAst;;
%}

%start  _objCMatch

%token  ObjCSIGNSTUB

%%

_objCMatch:
    MATCH; LBRACKET; ess=extendedSignatureList; RBRACKET;
	                   { ObjCMatching(ess) }
;

extendedSignatureList:
 	|  ObjCSIGNSTUB;  {  [ { stub = 33 } ] }
 ;
