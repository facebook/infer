/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

%{

  let dummy_ptr = { Clang_ast_t.ti_pointer = 0;
                    Clang_ast_t.ti_desugared_type = None }

%}

%token CHAR
%token CHAR16_T
%token CHAR32_T
%token WCHAR_T
%token BOOL
%token SHORT
%token INT
%token LONG
%token FLOAT
%token DOUBLE
%token VOID

%start <Clang_ast_t.c_type> ctype_specifier
%%

ctype_specifier:
 | simple_type_specifier { $1 }

simple_type_specifier:
  | CHAR { Clang_ast_t.BuiltinType(dummy_ptr, `Char_U) }
  | CHAR16_T { Clang_ast_t.BuiltinType(dummy_ptr, `Char16) }
  | CHAR32_T { Clang_ast_t.BuiltinType(dummy_ptr, `Char32) }
  | WCHAR_T { Clang_ast_t.BuiltinType(dummy_ptr, `WChar_U) }
  | BOOL { Clang_ast_t.BuiltinType(dummy_ptr, `Bool) }
  | SHORT { Clang_ast_t.BuiltinType(dummy_ptr, `Short) }
  | INT { Clang_ast_t.BuiltinType(dummy_ptr, `Int) }
  | LONG { Clang_ast_t.BuiltinType(dummy_ptr, `Long) }
  | FLOAT { Clang_ast_t.BuiltinType(dummy_ptr, `Float) }
  | DOUBLE { Clang_ast_t.BuiltinType(dummy_ptr, `Double) }
  | VOID { Clang_ast_t.BuiltinType(dummy_ptr, `Void) }
  ;

%%
