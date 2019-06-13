/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

%{
  open! IStd

  open Ctl_parser_types

  module L = Logging

(* See StringRef BuiltinType::getName in
  https://clang.llvm.org/doxygen/Type_8cpp_source.html *)
  let tokens_to_abs_types l =
    match l with
    | [t] -> BuiltIn t
    | [Int; Char_U] -> BuiltIn SChar
    | [Long; Long] -> BuiltIn LongLong
    | [UInt; Char_U] -> BuiltIn UChar
    | [UInt; Short] -> BuiltIn UShort
    | [UInt; Int] -> BuiltIn UInt
    | [UInt; Long] -> BuiltIn ULong
    | [UInt; Long; Long] -> BuiltIn ULongLong
    | [Long; Double] -> BuiltIn LongDouble
    | [UInt; Int128] -> BuiltIn UInt128
    | _ -> raise (CTLExceptions.ALParserInvariantViolationException
      ("ERROR: syntax error on types"))


%}

%token CHAR
%token CHAR16_T
%token CHAR32_T
%token WCHAR_T
%token UNDUNDWCHAR_T
%token BOOL
%token SHORT
%token INT
%token LONG
%token FLOAT
%token DOUBLE
%token VOID
%token SIGNED
%token UNSIGNED
%token INT128
%token FLOAT128
%token HALF
%token UNDUNDFP16
%token NULLPTR
%token OBJCID
%token OBJCCLASS
%token OBJCSEL
%token STAR
%token AMPERSAND
%token EOF
%token REGEXP
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_ANGLE
%token RIGHT_ANGLE
%token <string> IDENTIFIER
%token <string> STRING
%token <string> REARG

%start <Ctl_parser_types.abs_ctype> abs_ctype
%%

abs_ctype:
 | ctype_specifier_seq EOF {
   L.(debug Linters Verbose) "\tType effectively parsed: `%s`@\n"
   (Ctl_parser_types.abs_ctype_to_string $1);
   $1 }
 ;

ctype_specifier_seq:
| protocol_or_generics_type_spec { $1 }
| noptr_type_spec  { $1 }
| ptr_type_spec  { $1 }
| reference_type_spec { $1 }
| type_name { $1 }
;

ptr_type_spec:
| noptr_type_spec STAR { Pointer $1 }
| ptr_type_spec STAR { Pointer $1 }
| type_name STAR { Pointer $1 }
| protocol_or_generics_type_spec STAR { Pointer $1 }
;

reference_type_spec:
| type_name AMPERSAND { Reference $1 }
;

protocol_or_generics_type_spec:
|  type_name_or_objid LEFT_ANGLE ctype_specifier_seq RIGHT_ANGLE {
   let tname = $1 in
    L.(debug Linters Verbose) "\tProtocol or Generics parsed: `%s<%s>`@\n"
    (Ctl_parser_types.abs_ctype_to_string tname)
    (Ctl_parser_types.abs_ctype_to_string $3);
    ObjCGenProt (tname, $3)
  }
;


type_name_or_objid:
 | OBJCID { BuiltIn ObjCId}
 | type_name { $1 }
 ;

type_name:
  | alexp {
    L.(debug Linters Verbose) "\tType_name parsed: `%s`@\n"
    (ALVar.alexp_to_string $1);
    TypeName $1 }

noptr_type_spec:
  | trailing_type_specifier_seq
    { let atyp = tokens_to_abs_types $1 in
      atyp
     }
  ;

trailing_type_specifier:
 | simple_type_specifier { $1 }
 ;

trailing_type_specifier_seq:
  | trailing_type_specifier { [$1] }
  | trailing_type_specifier trailing_type_specifier_seq { $1 :: $2 }
  ;

simple_type_specifier:
  | CHAR { Char_U }
  | CHAR16_T { Char16 }
  | CHAR32_T { Char32 }
  | WCHAR_T { WChar_U }
  | UNDUNDWCHAR_T { WChar_U }
  | BOOL { Bool }
  | SHORT { Short }
  | INT { Int }
  | LONG { Long }
  | FLOAT { Float }
  | DOUBLE { Double }
  | VOID { Void }
  | SIGNED { Int }
  | UNSIGNED { UInt }
  | INT128 { Int128 }
  | FLOAT128 { Float128 }
  | HALF { Half }
  | UNDUNDFP16 { Half }
  | NULLPTR { NullPtr }
  | OBJCID { ObjCId }
  | OBJCCLASS { ObjCClass }
  | OBJCSEL { ObjCSel }
  ;

  alexp:
   | STRING { L.(debug Linters Verbose) "\tParsed string constant '%s' @\n" $1;
              ALVar.Const $1 }
   | REGEXP LEFT_PAREN REARG RIGHT_PAREN
            { L.(debug Linters Verbose) "\tParsed regular expression '%s' @\n" $3;
              ALVar.Regexp {string=$3; regexp=lazy (Str.regexp $3)} }
   | IDENTIFIER { ALVar.Var $1 }
   ;

%%
