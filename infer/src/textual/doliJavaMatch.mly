(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

%{
open DoliJavaAst;;
%}

%start  _javaMatch

%%

_javaMatch: MATCH;
	LBRACKET; ess=list(extendedSignature); RBRACKET;
	                   { JavaMatching ess }

extendedSignature:
 	| sigs=separated_nonempty_list(SEMICOLON,signature); UNDER; rt=referenceType
	 (* the SEMICOLON separator is needed in order to avoid shift-reduce conflicts *)
	  { {signs=sigs; under=rt}  }
	;

signature:
	| mds=modifier*; rt=returnType; funcId=ID;
	LPAREN fPTs=separated_list(COMMA, formalParameterType) RPAREN
	option(throws)
		{ {modifiers = mds; returns = rt; identifier = funcId; formParTypes = fPTs } }
	;

modifier:
    | PUBLIC { Public }
	| PROTECTED { Protected }
	| PRIVATE { Private }
	| STATIC { Static }
	| FINAL { Final }
	| ABSTRACT { Abstract }
	| NATIVE { Native }
	;

returnType:
  | VOID { VoidType }
	| nvt=nonVoidType { NonVoid nvt }
	;

referenceType:
  | cts=separated_nonempty_list(DOT, classType) { RT cts }
   ;

classType:
    | cId=ID { CT (cId, []) }
	| cId=ID LABRACKET tArgs=separated_list(COMMA, typeArgument)  RABRACKET { CT (cId,tArgs) }
	;


nonVoidType:
(* QUESTION: Do we want to "flatten" tree in the future?,
   ie avoid having one constructor appied to an other*)
	| BYTE { BasicType ByteType }
	| INT { BasicType IntType }
	| BOOLEAN { BasicType BoolType }
	| CHAR { BasicType CharType }
	| DOUBLE { BasicType DoubleType }
	| FLOAT { BasicType FloatType }
	| LONG { BasicType LongType }
	| SHORT { BasicType ShortType }
	| rt=referenceType { RefType rt }
	| nvt=nonVoidType; LSBRACKET RSBRACKET { Array nvt }
	;

formalParameterType:
   | nvt=nonVoidType; ID { nvt }
   ;


typeArgument:
    | rt=referenceType { PLAIN rt }
    | QUESTION SUPER; rt=referenceType { SUPER rt }
    | QUESTION EXTENDS rt=referenceType { EXTENDS rt }
	 ;


throws:
	| THROWS  separated_nonempty_list(COMMA, referenceType){ }
	;
