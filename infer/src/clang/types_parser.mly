/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

%{
  open Ctl_parser_types

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
    | _ -> raise (Ctl_parser_types.ALParsingException
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
%token EOF


%start <Ctl_parser_types.abs_ctype> abs_ctype
%%

abs_ctype:
 | ctype_specifier_seq EOF {
   Logging.out "\tType effectively parsed: `%s`\n"
   (Ctl_parser_types.abs_ctype_to_string $1);
   $1 }
 ;

ctype_specifier_seq:
| noptr_type_spec  { $1 }
| ptr_type_spec  { $1 }
;

ptr_type_spec:
| noptr_type_spec STAR { Pointer $1 }
| ptr_type_spec STAR { Pointer $1 }
;

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

%%
