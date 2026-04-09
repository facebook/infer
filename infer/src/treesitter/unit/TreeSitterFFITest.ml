(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let parse_string source =
  let tmp = "/tmp/treesitter_test.c" in
  Out_channel.write_all tmp ~data:source ;
  let cst = TreeSitterFFI.parse_file tmp in
  cst


let%expect_test "simple function" =
  let cst = parse_string "int add(int x, int y) { return x + y; }" in
  TreeSitterFFI.pp_cst_node F.std_formatter cst ;
  [%expect
    {|
    (translation_unit 0:0-0:39
      (function_definition 0:0-0:39
        (primitive_type field=type 0:0-0:3 "int")
        (function_declarator field=declarator 0:4-0:21
          (identifier field=declarator 0:4-0:7 "add")
          (parameter_list field=parameters 0:7-0:21
            (parameter_declaration 0:8-0:13
              (primitive_type field=type 0:8-0:11 "int")
              (identifier field=declarator 0:12-0:13 "x")
            )
            (parameter_declaration 0:15-0:20
              (primitive_type field=type 0:15-0:18 "int")
              (identifier field=declarator 0:19-0:20 "y")
            )
          )
        )
        (compound_statement field=body 0:22-0:39
          (return_statement 0:24-0:37
            (binary_expression 0:31-0:36
              (identifier field=left 0:31-0:32 "x")
              (identifier field=right 0:35-0:36 "y")
            )
          )
        )
      )
    )
    |}]


let%expect_test "pointer parameter" =
  let cst = parse_string "void f(int *ptr) { *ptr = 42; }" in
  TreeSitterFFI.pp_cst_node F.std_formatter cst ;
  [%expect
    {|
    (translation_unit 0:0-0:31
      (function_definition 0:0-0:31
        (primitive_type field=type 0:0-0:4 "void")
        (function_declarator field=declarator 0:5-0:16
          (identifier field=declarator 0:5-0:6 "f")
          (parameter_list field=parameters 0:6-0:16
            (parameter_declaration 0:7-0:15
              (primitive_type field=type 0:7-0:10 "int")
              (pointer_declarator field=declarator 0:11-0:15
                (identifier field=declarator 0:12-0:15 "ptr")
              )
            )
          )
        )
        (compound_statement field=body 0:17-0:31
          (expression_statement 0:19-0:29
            (assignment_expression 0:19-0:28
              (pointer_expression field=left 0:19-0:23
                (identifier field=argument 0:20-0:23 "ptr")
              )
              (number_literal field=right 0:26-0:28 "42")
            )
          )
        )
      )
    )
    |}]


let%expect_test "struct definition" =
  let cst = parse_string "struct Point { int x; int y; };" in
  TreeSitterFFI.pp_cst_node F.std_formatter cst ;
  [%expect
    {|
    (translation_unit 0:0-0:31
      (struct_specifier 0:0-0:30
        (type_identifier field=name 0:7-0:12 "Point")
        (field_declaration_list field=body 0:13-0:30
          (field_declaration 0:15-0:21
            (primitive_type field=type 0:15-0:18 "int")
            (field_identifier field=declarator 0:19-0:20 "x")
          )
          (field_declaration 0:22-0:28
            (primitive_type field=type 0:22-0:25 "int")
            (field_identifier field=declarator 0:26-0:27 "y")
          )
        )
      )
    )
    |}]


let%expect_test "if-else" =
  let cst = parse_string "int max(int a, int b) { if (a > b) return a; else return b; }" in
  TreeSitterFFI.pp_cst_node F.std_formatter cst ;
  [%expect
    {|
    (translation_unit 0:0-0:61
      (function_definition 0:0-0:61
        (primitive_type field=type 0:0-0:3 "int")
        (function_declarator field=declarator 0:4-0:21
          (identifier field=declarator 0:4-0:7 "max")
          (parameter_list field=parameters 0:7-0:21
            (parameter_declaration 0:8-0:13
              (primitive_type field=type 0:8-0:11 "int")
              (identifier field=declarator 0:12-0:13 "a")
            )
            (parameter_declaration 0:15-0:20
              (primitive_type field=type 0:15-0:18 "int")
              (identifier field=declarator 0:19-0:20 "b")
            )
          )
        )
        (compound_statement field=body 0:22-0:61
          (if_statement 0:24-0:59
            (parenthesized_expression field=condition 0:27-0:34
              (binary_expression 0:28-0:33
                (identifier field=left 0:28-0:29 "a")
                (identifier field=right 0:32-0:33 "b")
              )
            )
            (return_statement field=consequence 0:35-0:44
              (identifier 0:42-0:43 "a")
            )
            (else_clause field=alternative 0:45-0:59
              (return_statement 0:50-0:59
                (identifier 0:57-0:58 "b")
              )
            )
          )
        )
      )
    )
    |}]
