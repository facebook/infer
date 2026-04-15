(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let test xml =
  let textual = TreeSitterCTranslator.translate_xml xml ~file_name:"test.c" in
  match TextualVerification.verify_keep_going textual with
  | Error errs ->
      F.printf "Verification failed:@\n%a" (F.pp_print_list TextualVerification.pp_error) errs
  | Ok (verified, _warnings) ->
      let transformed, _ = TextualTransform.run C verified |> Stdlib.Result.get_ok in
      Textual.Module.pp ~show_location:false F.std_formatter transformed


let%expect_test "simple function" =
  test
    {|<?xml version="1.0"?>
<sources><source name="test.c">
<translation_unit srow="0" scol="0" erow="1" ecol="0">
  <function_definition srow="0" scol="0" erow="0" ecol="39">
    <primitive_type field="type" srow="0" scol="0" erow="0" ecol="3">int</primitive_type>
    <function_declarator field="declarator" srow="0" scol="4" erow="0" ecol="21">
      <identifier field="declarator" srow="0" scol="4" erow="0" ecol="7">add</identifier>
      <parameter_list field="parameters" srow="0" scol="7" erow="0" ecol="21">
        (
        <parameter_declaration srow="0" scol="8" erow="0" ecol="13">
          <primitive_type field="type" srow="0" scol="8" erow="0" ecol="11">int</primitive_type>
          <identifier field="declarator" srow="0" scol="12" erow="0" ecol="13">x</identifier>
        </parameter_declaration>
        ,
        <parameter_declaration srow="0" scol="15" erow="0" ecol="20">
          <primitive_type field="type" srow="0" scol="15" erow="0" ecol="18">int</primitive_type>
          <identifier field="declarator" srow="0" scol="19" erow="0" ecol="20">y</identifier>
        </parameter_declaration>
        )
      </parameter_list>
    </function_declarator>
    <compound_statement field="body" srow="0" scol="22" erow="0" ecol="39">
      {
      <return_statement srow="0" scol="24" erow="0" ecol="37">
        return
        <binary_expression srow="0" scol="31" erow="0" ecol="36">
          <identifier field="left" srow="0" scol="31" erow="0" ecol="32">x</identifier>
          +
          <identifier field="right" srow="0" scol="35" erow="0" ecol="36">y</identifier>
        </binary_expression>
        ;
      </return_statement>
      }
    </compound_statement>
  </function_definition>
</translation_unit>
</source></sources>|} ;
  [%expect
    {|
    .source_language = "C"

    define add(x: int, y: int) : int {
      #bb0:
          n0:int = load &x
          n1:int = load &y
          ret __sil_plusa_int(n0, n1)

    } |}]


let%expect_test "if-else" =
  test
    {|<?xml version="1.0"?>
<sources><source name="test.c">
<translation_unit srow="0" scol="0" erow="1" ecol="0">
  <function_definition srow="0" scol="0" erow="0" ecol="69">
    <primitive_type field="type" srow="0" scol="0" erow="0" ecol="3">int</primitive_type>
    <function_declarator field="declarator" srow="0" scol="4" erow="0" ecol="21">
      <identifier field="declarator" srow="0" scol="4" erow="0" ecol="7">max</identifier>
      <parameter_list field="parameters" srow="0" scol="7" erow="0" ecol="21">
        (
        <parameter_declaration srow="0" scol="8" erow="0" ecol="13">
          <primitive_type field="type" srow="0" scol="8" erow="0" ecol="11">int</primitive_type>
          <identifier field="declarator" srow="0" scol="12" erow="0" ecol="13">x</identifier>
        </parameter_declaration>
        ,
        <parameter_declaration srow="0" scol="15" erow="0" ecol="20">
          <primitive_type field="type" srow="0" scol="15" erow="0" ecol="18">int</primitive_type>
          <identifier field="declarator" srow="0" scol="19" erow="0" ecol="20">y</identifier>
        </parameter_declaration>
        )
      </parameter_list>
    </function_declarator>
    <compound_statement field="body" srow="0" scol="22" erow="0" ecol="69">
      {
      <if_statement srow="0" scol="24" erow="0" ecol="67">
        if
        <parenthesized_expression field="condition" srow="0" scol="27" erow="0" ecol="34">
          (
          <binary_expression srow="0" scol="28" erow="0" ecol="33">
            <identifier field="left" srow="0" scol="28" erow="0" ecol="29">x</identifier>
            &gt;
            <identifier field="right" srow="0" scol="32" erow="0" ecol="33">y</identifier>
          </binary_expression>
          )
        </parenthesized_expression>
        <compound_statement field="consequence" srow="0" scol="35" erow="0" ecol="48">
          {
          <return_statement srow="0" scol="37" erow="0" ecol="46">
            return
            <identifier srow="0" scol="44" erow="0" ecol="45">x</identifier>
            ;
          </return_statement>
          }
        </compound_statement>
        <else_clause field="alternative" srow="0" scol="49" erow="0" ecol="67">
          else
          <compound_statement srow="0" scol="54" erow="0" ecol="67">
            {
            <return_statement srow="0" scol="56" erow="0" ecol="65">
              return
              <identifier srow="0" scol="63" erow="0" ecol="64">y</identifier>
              ;
            </return_statement>
            }
          </compound_statement>
        </else_clause>
      </if_statement>
      }
    </compound_statement>
  </function_definition>
</translation_unit>
</source></sources>|} ;
  [%expect
    {|
    .source_language = "C"

    define max(x: int, y: int) : int {
      #bb0:
          n0:int = load &x
          n1:int = load &y
          jmp bb2, bb3

      #bb2:
          prune __sil_gt(n0, n1)
          n2:int = load &x
          ret n2

      #bb3:
          prune __sil_lnot(__sil_gt(n0, n1))
          n4:int = load &y
          ret n4

    }
    |}]


let%expect_test "null deref with pointer" =
  test
    {|<?xml version="1.0"?>
<sources><source name="test.c">
<translation_unit srow="0" scol="0" erow="3" ecol="0">
  <function_definition srow="0" scol="0" erow="2" ecol="1">
    <primitive_type field="type" srow="0" scol="0" erow="0" ecol="4">void</primitive_type>
    <function_declarator field="declarator" srow="0" scol="5" erow="0" ecol="16">
      <identifier field="declarator" srow="0" scol="5" erow="0" ecol="6">f</identifier>
      <parameter_list field="parameters" srow="0" scol="6" erow="0" ecol="16">
        (
        <parameter_declaration srow="0" scol="7" erow="0" ecol="15">
          <primitive_type field="type" srow="0" scol="7" erow="0" ecol="10">int</primitive_type>
          <pointer_declarator field="declarator" srow="0" scol="10" erow="0" ecol="15">
            *
            <identifier field="declarator" srow="0" scol="12" erow="0" ecol="15">ptr</identifier>
          </pointer_declarator>
        </parameter_declaration>
        )
      </parameter_list>
    </function_declarator>
    <compound_statement field="body" srow="0" scol="17" erow="2" ecol="1">
      {
      <expression_statement srow="1" scol="2" erow="1" ecol="12">
        <assignment_expression srow="1" scol="2" erow="1" ecol="11">
          <pointer_expression field="left" srow="1" scol="2" erow="1" ecol="6">
            *
            <identifier field="argument" srow="1" scol="3" erow="1" ecol="6">ptr</identifier>
          </pointer_expression>
          =
          <number_literal field="right" srow="1" scol="9" erow="1" ecol="11">42</number_literal>
        </assignment_expression>
        ;
      </expression_statement>
      }
    </compound_statement>
  </function_definition>
</translation_unit>
</source></sources>|} ;
  [%expect
    {|
    .source_language = "C"

    define f(ptr: *int) : void {
      #bb0:
          n0:*int = load &ptr
          store n0 <- 42:int
          jmp bb1

      #bb1:
          jmp

    } |}]


let%expect_test "function declaration" =
  test
    {|<?xml version="1.0"?>
<sources><source name="test.c">
<translation_unit srow="0" scol="0" erow="1" ecol="0">
  <declaration srow="0" scol="0" erow="0" ecol="23">
    <primitive_type field="type" srow="0" scol="0" erow="0" ecol="4">void</primitive_type>
    <pointer_declarator field="declarator" srow="0" scol="4" erow="0" ecol="22">
      *
      <function_declarator field="declarator" srow="0" scol="6" erow="0" ecol="22">
        <identifier field="declarator" srow="0" scol="6" erow="0" ecol="12">malloc</identifier>
        <parameter_list field="parameters" srow="0" scol="12" erow="0" ecol="22">
          (
          <parameter_declaration srow="0" scol="13" erow="0" ecol="21">
            <primitive_type field="type" srow="0" scol="13" erow="0" ecol="16">int</primitive_type>
            <identifier field="declarator" srow="0" scol="17" erow="0" ecol="21">size</identifier>
          </parameter_declaration>
          )
        </parameter_list>
      </function_declarator>
    </pointer_declarator>
    ;
  </declaration>
</translation_unit>
</source></sources>|} ;
  [%expect {|
    .source_language = "C"

    declare malloc(int) : *void |}]


let%expect_test "for loop" =
  test
    {|<?xml version="1.0"?>
<sources><source name="test.c">
<translation_unit srow="0" scol="0" erow="5" ecol="0">
  <function_definition srow="0" scol="0" erow="4" ecol="1">
    <primitive_type field="type" srow="0" scol="0" erow="0" ecol="3">int</primitive_type>
    <function_declarator field="declarator" srow="0" scol="4" erow="0" ecol="14">
      <identifier field="declarator" srow="0" scol="4" erow="0" ecol="7">sum</identifier>
      <parameter_list field="parameters" srow="0" scol="7" erow="0" ecol="14">
        (
        <parameter_declaration srow="0" scol="8" erow="0" ecol="13">
          <primitive_type field="type" srow="0" scol="8" erow="0" ecol="11">int</primitive_type>
          <identifier field="declarator" srow="0" scol="12" erow="0" ecol="13">n</identifier>
        </parameter_declaration>
        )
      </parameter_list>
    </function_declarator>
    <compound_statement field="body" srow="0" scol="15" erow="4" ecol="1">
      {
      <declaration srow="1" scol="2" erow="1" ecol="12">
        <primitive_type field="type" srow="1" scol="2" erow="1" ecol="5">int</primitive_type>
        <init_declarator srow="1" scol="6" erow="1" ecol="11">
          <identifier field="declarator" srow="1" scol="6" erow="1" ecol="7">s</identifier>
          =
          <number_literal field="value" srow="1" scol="10" erow="1" ecol="11">0</number_literal>
        </init_declarator>
        ;
      </declaration>
      <for_statement srow="2" scol="2" erow="3" ecol="3">
        for
        (
        <declaration field="initializer" srow="2" scol="7" erow="2" ecol="16">
          <primitive_type field="type" srow="2" scol="7" erow="2" ecol="10">int</primitive_type>
          <init_declarator srow="2" scol="11" erow="2" ecol="15">
            <identifier field="declarator" srow="2" scol="11" erow="2" ecol="12">i</identifier>
            =
            <number_literal field="value" srow="2" scol="15" erow="2" ecol="16">0</number_literal>
          </init_declarator>
          ;
        </declaration>
        <binary_expression field="condition" srow="2" scol="18" erow="2" ecol="23">
          <identifier field="left" srow="2" scol="18" erow="2" ecol="19">i</identifier>
          &lt;
          <identifier field="right" srow="2" scol="22" erow="2" ecol="23">n</identifier>
        </binary_expression>
        ;
        <update_expression field="update" srow="2" scol="25" erow="2" ecol="28">
          <identifier field="argument" srow="2" scol="25" erow="2" ecol="26">i</identifier>
          ++
        </update_expression>
        )
        <compound_statement field="body" srow="2" scol="30" erow="3" ecol="3">
          {
          <expression_statement srow="3" scol="4" erow="3" ecol="10">
            <assignment_expression srow="3" scol="4" erow="3" ecol="9">
              <identifier field="left" srow="3" scol="4" erow="3" ecol="5">s</identifier>
              +=
              <identifier field="right" srow="3" scol="9" erow="3" ecol="10">i</identifier>
            </assignment_expression>
            ;
          </expression_statement>
          }
        </compound_statement>
      </for_statement>
      <return_statement srow="4" scol="2" erow="4" ecol="11">
        return
        <identifier srow="4" scol="9" erow="4" ecol="10">s</identifier>
        ;
      </return_statement>
      }
    </compound_statement>
  </function_definition>
</translation_unit>
</source></sources>|} ;
  [%expect
    {|
    .source_language = "C"

    define sum(n: int) : int {
      local s: int, i: int
      #bb0:
          store &s <- 0:int
          jmp bb2

      #bb2:
          store &i <- 0:int
          jmp bb4

      #bb3:
          prune __sil_lnot(__sil_lt(n0, n1))
          n7:int = load &s
          ret n7

      #bb4:
          n0:int = load &i
          n1:int = load &n
          jmp bb5, bb3

      #bb5:
          prune __sil_lt(n0, n1)
          n4:int = load &i
          n5:int = load &s
          store &s <- __sil_plusa_int(n5, n4):int
          jmp bb6

      #bb6:
          n2:int = load &i
          store &i <- __sil_plusa_int(n2, 1):int
          jmp bb4

    }
    |}]


let%expect_test "struct and field access" =
  test
    {|<?xml version="1.0"?>
<sources><source name="test.c">
<translation_unit srow="0" scol="0" erow="3" ecol="0">
  <struct_specifier srow="0" scol="0" erow="0" ecol="25">
    struct
    <type_identifier field="name" srow="0" scol="7" erow="0" ecol="12">Point</type_identifier>
    <field_declaration_list field="body" srow="0" scol="13" erow="0" ecol="25">
      {
      <field_declaration srow="0" scol="15" erow="0" ecol="20">
        <primitive_type field="type" srow="0" scol="15" erow="0" ecol="18">int</primitive_type>
        <field_identifier field="declarator" srow="0" scol="19" erow="0" ecol="20">x</field_identifier>
        ;
      </field_declaration>
      }
    </field_declaration_list>
  </struct_specifier>
  ;
  <function_definition srow="1" scol="0" erow="2" ecol="1">
    <primitive_type field="type" srow="1" scol="0" erow="1" ecol="3">int</primitive_type>
    <function_declarator field="declarator" srow="1" scol="4" erow="1" ecol="26">
      <identifier field="declarator" srow="1" scol="4" erow="1" ecol="9">get_x</identifier>
      <parameter_list field="parameters" srow="1" scol="9" erow="1" ecol="26">
        (
        <parameter_declaration srow="1" scol="10" erow="1" ecol="25">
          <struct_specifier field="type" srow="1" scol="10" erow="1" ecol="22">
            struct
            <type_identifier field="name" srow="1" scol="17" erow="1" ecol="22">Point</type_identifier>
          </struct_specifier>
          <pointer_declarator field="declarator" srow="1" scol="22" erow="1" ecol="25">
            *
            <identifier field="declarator" srow="1" scol="24" erow="1" ecol="25">p</identifier>
          </pointer_declarator>
        </parameter_declaration>
        )
      </parameter_list>
    </function_declarator>
    <compound_statement field="body" srow="1" scol="27" erow="2" ecol="1">
      {
      <return_statement srow="2" scol="2" erow="2" ecol="17">
        return
        <field_expression srow="2" scol="9" erow="2" ecol="16">
          <identifier field="argument" srow="2" scol="9" erow="2" ecol="10">p</identifier>
          -&gt;
          <field_identifier field="field" srow="2" scol="12" erow="2" ecol="16">x</field_identifier>
        </field_expression>
        ;
      </return_statement>
      }
    </compound_statement>
  </function_definition>
</translation_unit>
</source></sources>|} ;
  [%expect
    {|
    .source_language = "C"

    type Point = {x: int}

    define get_x(p: *Point) : int {
      #bb0:
          n0:*Point = load &p
          n1:int = load n0.Point.x
          ret n1

    } |}]


let%expect_test "ternary expression" =
  test
    {|<?xml version="1.0"?>
<sources><source name="test.c">
<translation_unit srow="0" scol="0" erow="1" ecol="0">
  <function_definition srow="0" scol="0" erow="0" ecol="39">
    <primitive_type field="type" srow="0" scol="0" erow="0" ecol="3">int</primitive_type>
    <function_declarator field="declarator" srow="0" scol="4" erow="0" ecol="14">
      <identifier field="declarator" srow="0" scol="4" erow="0" ecol="7">abs</identifier>
      <parameter_list field="parameters" srow="0" scol="7" erow="0" ecol="14">
        (
        <parameter_declaration srow="0" scol="8" erow="0" ecol="13">
          <primitive_type field="type" srow="0" scol="8" erow="0" ecol="11">int</primitive_type>
          <identifier field="declarator" srow="0" scol="12" erow="0" ecol="13">x</identifier>
        </parameter_declaration>
        )
      </parameter_list>
    </function_declarator>
    <compound_statement field="body" srow="0" scol="15" erow="0" ecol="39">
      {
      <return_statement srow="0" scol="17" erow="0" ecol="37">
        return
        <conditional_expression srow="0" scol="24" erow="0" ecol="36">
          <binary_expression field="condition" srow="0" scol="24" erow="0" ecol="29">
            <identifier field="left" srow="0" scol="24" erow="0" ecol="25">x</identifier>
            &gt;
            <number_literal field="right" srow="0" scol="28" erow="0" ecol="29">0</number_literal>
          </binary_expression>
          ?
          <identifier field="consequence" srow="0" scol="32" erow="0" ecol="33">x</identifier>
          :
          <unary_expression field="alternative" srow="0" scol="36" erow="0" ecol="38">
            -
            <identifier field="argument" srow="0" scol="37" erow="0" ecol="38">x</identifier>
          </unary_expression>
        </conditional_expression>
        ;
      </return_statement>
      }
    </compound_statement>
  </function_definition>
</translation_unit>
</source></sources>|} ;
  [%expect
    {|
    .source_language = "C"

    define abs(x: int) : int {
      #bb0:
          n0:int = load &x
          n1:int = load &x
          n2:int = load &x
          jmp if_exp1, if_exp2

      #if_exp2:
          prune __sil_lnot(__sil_ne(__sil_gt(n0, 0), 0))
          store &__SSA3 <- __sil_neg(n2):*void
          jmp if_exp0

      #if_exp1:
          prune __sil_ne(__sil_gt(n0, 0), 0)
          store &__SSA3 <- n1:*void
          jmp if_exp0

      #if_exp0:
          n3:*void = load &__SSA3
          ret n3

    }
    |}]


let%expect_test "short-circuit and" =
  test
    {|<?xml version="1.0"?>
<sources><source name="test.c">
<translation_unit srow="0" scol="0" erow="1" ecol="0">
  <function_definition srow="0" scol="0" erow="0" ecol="52">
    <primitive_type field="type" srow="0" scol="0" erow="0" ecol="4">void</primitive_type>
    <function_declarator field="declarator" srow="0" scol="5" erow="0" ecol="14">
      <identifier field="declarator" srow="0" scol="5" erow="0" ecol="6">f</identifier>
      <parameter_list field="parameters" srow="0" scol="6" erow="0" ecol="14">
        (
        <parameter_declaration srow="0" scol="7" erow="0" ecol="13">
          <primitive_type field="type" srow="0" scol="7" erow="0" ecol="10">int</primitive_type>
          <pointer_declarator field="declarator" srow="0" scol="10" erow="0" ecol="13">
            *
            <identifier field="declarator" srow="0" scol="12" erow="0" ecol="13">p</identifier>
          </pointer_declarator>
        </parameter_declaration>
        )
      </parameter_list>
    </function_declarator>
    <compound_statement field="body" srow="0" scol="15" erow="0" ecol="52">
      {
      <if_statement srow="0" scol="17" erow="0" ecol="50">
        if
        <parenthesized_expression field="condition" srow="0" scol="20" erow="0" ecol="38">
          (
          <binary_expression srow="0" scol="21" erow="0" ecol="37">
            <binary_expression field="left" srow="0" scol="21" erow="0" ecol="27">
              <identifier field="left" srow="0" scol="21" erow="0" ecol="22">p</identifier>
              !=
              <number_literal field="right" srow="0" scol="26" erow="0" ecol="27">0</number_literal>
            </binary_expression>
            &amp;&amp;
            <binary_expression field="right" srow="0" scol="31" erow="0" ecol="37">
              <pointer_expression field="left" srow="0" scol="31" erow="0" ecol="33">
                *
                <identifier field="argument" srow="0" scol="32" erow="0" ecol="33">p</identifier>
              </pointer_expression>
              &gt;
              <number_literal field="right" srow="0" scol="36" erow="0" ecol="37">0</number_literal>
            </binary_expression>
          </binary_expression>
          )
        </parenthesized_expression>
        <compound_statement field="consequence" srow="0" scol="39" erow="0" ecol="50">
          {
          <expression_statement srow="0" scol="41" erow="0" ecol="48">
            <assignment_expression srow="0" scol="41" erow="0" ecol="47">
              <pointer_expression field="left" srow="0" scol="41" erow="0" ecol="43">
                *
                <identifier field="argument" srow="0" scol="42" erow="0" ecol="43">p</identifier>
              </pointer_expression>
              =
              <number_literal field="right" srow="0" scol="46" erow="0" ecol="47">0</number_literal>
            </assignment_expression>
            ;
          </expression_statement>
          }
        </compound_statement>
      </if_statement>
      }
    </compound_statement>
  </function_definition>
</translation_unit>
</source></sources>|} ;
  [%expect
    {|
    .source_language = "C"

    define f(p: *int) : void {
      #bb0:
          n0:*int = load &p
          n1:*int = load &p
          n2:int = load n1
          jmp bb2, if0, if1

      #if0:
          prune __sil_lnot(__sil_ne(n0, 0))
          jmp bb3

      #if1:
          prune __sil_lnot(__sil_gt(n2, 0))
          jmp bb3

      #bb1:
          jmp

      #bb2:
          prune __sil_ne(n0, 0)
          prune __sil_gt(n2, 0)
          n3:*int = load &p
          store n3 <- 0:int
          jmp bb1

      #bb3:
          jmp bb1

    }
    |}]
