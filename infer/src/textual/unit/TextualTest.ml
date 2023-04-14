(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open Textual

let sourcefile = SourceFile.create "dummy.sil"

let parse_module text =
  match TextualParser.parse_string sourcefile text with
  | Ok m ->
      m
  | Error es ->
      List.iter es ~f:(fun e -> F.printf "%a" (TextualParser.pp_error sourcefile) e) ;
      raise (Failure "Couldn't parse a module")


let%test_module "parsing" =
  ( module struct
    let text =
      {|
       .source_language = "hack"
       .source_file = "original.hack"

       .source_language = "java" // Won't have an effect

       define nothing(): void {
         #node0:
           ret null
       }
       |}


    let%expect_test _ =
      let module_ = parse_module text in
      let attrs = module_.attrs in
      F.printf "%a" (Pp.seq ~sep:"\n" Attr.pp_with_loc) attrs ;
      [%expect
        {|
        line 2, column 7: .source_language = "hack"
        line 3, column 7: .source_file = "original.hack"
        line 5, column 7: .source_language = "java" |}] ;
      let lang = Option.value_exn (Module.lang module_) in
      F.printf "%s" (Lang.to_string lang) ;
      [%expect {| hack |}]


    let%expect_test _ =
      let module_ = parse_module text in
      F.printf "%a" Module.pp module_ ;
      [%expect
        {|
        .source_language = "hack"

        .source_file = "original.hack"

        .source_language = "java"

        define nothing() : void {
          #node0:
              ret null

        } |}]


    let text =
      {|
       .source_language = "hack"

       declare HackMixed.foo(*HackMixed, int): int

       define foo(x: *HackMixed): int {
       #b0:
         n0:*HackMixed = load &x
         ret n0.HackMixed.foo(42)
       }
       |}


    let%expect_test _ =
      let m = parse_module text in
      F.printf "%a" Module.pp m ;
      [%expect
        {|
        .source_language = "hack"

        declare HackMixed.foo(*HackMixed, int) : int

        define foo(x: *HackMixed) : int {
          #b0:
              n0:*HackMixed = load &x
              ret n0.HackMixed.foo(42)

        } |}]


    let text =
      {|
       type A = {f1: int; f2: int}
       type B {f3: bool}
       type C extends A, B {f4: bool}
      |}


    let%expect_test _ =
      let m = parse_module text in
      F.printf "%a" Module.pp m ;
      [%expect
        {|
        type A = {f1: int; f2: int}

        type B = {f3: bool}

        type C extends A, B = {f4: bool} |}]


    let%expect_test "ellipsis" =
      let m =
        parse_module
          {|
           .source_language = "hack"
           declare todo(...): *Mixed
           declare todo2(int, float, ...): *Mixed
           declare foo(): *Mixed
           declare bar(int, float): *Mixed
           |}
      in
      F.printf "%a" Module.pp m ;
      [%expect
        {|
        .source_language = "hack"

        declare todo(...) : *Mixed

        declare todo2(int, float, ...) : *Mixed

        declare foo() : *Mixed

        declare bar(int, float) : *Mixed |}]


    let%expect_test "numbers lexing" =
      let text =
        {|
         .source_language = "hack"
         define foo() : int {
         #entry:
           n0 = 12
           n1 = -42
           n2 = 1e1
           n3 = 2.
           n4 = 3.14
           n5 = 6.022137e+23
           n2 = -1e1
           n3 = -2.
           n4 = -3.14
           n5 = -6.022137e+23
           ret n1
         }
         |}
      in
      let m = parse_module text in
      F.printf "%a" Module.pp m ;
      [%expect
        {|
        .source_language = "hack"

        define foo() : int {
          #entry:
              n0 = 12
              n1 = -42
              n2 = 10.
              n3 = 2.
              n4 = 3.14
              n5 = 6.022137e+23
              n2 = -10.
              n3 = -2.
              n4 = -3.14
              n5 = -6.022137e+23
              ret n1

        } |}]
  end )


let%test_module "procnames" =
  ( module struct
    let%expect_test _ =
      let toplevel_proc =
        ProcDecl.
          { qualified_name=
              { enclosing_class= TopLevel
              ; name= {value= "toplevel"; loc= Location.known ~line:0 ~col:0} }
          ; formals_types= []
          ; are_formal_types_fully_declared= true
          ; result_type= Typ.mk_without_attributes Typ.Void
          ; attributes= [] }
      in
      let as_java = TextualSil.proc_decl_to_sil Lang.Java toplevel_proc in
      let as_hack = TextualSil.proc_decl_to_sil Lang.Hack toplevel_proc in
      F.printf "%a@\n" Procname.pp as_java ;
      F.printf "%a@\n" Procname.pp as_hack ;
      [%expect {|
        void $TOPLEVEL$CLASS$.toplevel()
        toplevel |}]
  end )


let%test_module "to_sil" =
  ( module struct
    let%expect_test _ =
      let no_lang = {|define nothing() : void { #node: ret null }|} in
      let m = parse_module no_lang in
      try TextualSil.module_to_sil m |> ignore
      with TextualTransformError errs ->
        List.iter errs ~f:(Textual.pp_transform_error sourcefile F.std_formatter) ;
        [%expect
          {| dummy.sil, <unknown location>: transformation error: Missing or unsupported source_language attribute |}]


    let%expect_test "undefined types are included in tenv" =
      let source =
        {|
          .source_language = "hack"
          type Foo {}
          define Foo.f(arg1: Foo, arg2: Bar) : void { #n: ret null }
          declare Foo.undef() : void
          define Bar.f() : void {
            #entry:
              ret null
          }
          define g() : void {
            local l1: *Quux
            #n:
              n0 = __sil_allocate(<Baz>)
              ret null
          }
          |}
      in
      let m = parse_module source in
      let _, tenv = TextualSil.module_to_sil m in
      F.printf "%a@\n" Tenv.pp tenv ;
      [%expect
        {|
         hack Foo
         fields: {}
         statics: {}
         supers: {}
         objc_protocols: {}
         methods: {
                     Foo.f
                     Foo.undef
                   }
         exported_obj_methods: {}
         annots: {<>}
         java_class_info: {[None]}
         dummy: false
         hack Quux
         fields: {}
         statics: {}
         supers: {}
         objc_protocols: {}
         methods: {}
         exported_obj_methods: {}
         annots: {<>}
         java_class_info: {[None]}
         dummy: true
         hack Baz
         fields: {}
         statics: {}
         supers: {}
         objc_protocols: {}
         methods: {}
         exported_obj_methods: {}
         annots: {<>}
         java_class_info: {[None]}
         dummy: true
         hack Bar
         fields: {}
         statics: {}
         supers: {}
         objc_protocols: {}
         methods: {
                     Bar.f
                   }
         exported_obj_methods: {}
         annots: {<>}
         java_class_info: {[None]}
         dummy: false |}]


    let%expect_test "unknown formal calls" =
      let source =
        {|
         .source_language = "hack"
         declare unknown(...) : *HackMixed
         declare known(*HackInt) : void

         define foo(x: *Foo, y: *HackInt) : void {
         #b0:
           n0: *HackMixed = load &x
           n1 = unknown(n0)
           n2: *HackMixed = load &y
           n3 = known(n2)
           ret null
         }
         |}
      in
      let m = parse_module source in
      let cfg, _ = TextualSil.module_to_sil m in
      Cfg.iter_sorted cfg ~f:(fun pdesc ->
          F.printf "%a" (Procdesc.pp_with_instrs ~print_types:true) pdesc ) ;
      [%expect
        {|
        { proc_name= foo
        ; translation_unit= dummy.sil
        ; formals= [(x,Foo*);  (y,HackInt*)]
        ; is_defined= true
        ; loc= dummy.sil:6:16
        ; locals= []
        ; ret_type= void
        ; proc_id= foo }
            #n1:

            #n3:
              n$0=*&x:HackMixed* [line 8, column 11];
              n$1=_fun_unknown(n$0:HackMixed*) [line 9, column 11];
              n$2=*&y:HackMixed* [line 10, column 11];
              n$3=_fun_known(n$2:HackInt*) [line 11, column 11];
              *&return:void=0 [line 12, column 11];

            #n2: |}]
  end )


let%test_module "remove_internal_calls transformation" =
  ( module struct
    let input_text =
      {|
        declare g1(int) : int

        declare g2(int) : int

        declare g3(int) : int

        declare g4(int) : *int

        declare m(int, int) : int

        define f(x: int, y: int) : int {
          #entry:
              n0:int = load &x
              n1:int = load &y
              n3 = __sil_mult_int(g3(n0), m(g1(n0), g2(n1)))
              n4 = m(n0, g3(n1))
              jmp lab1(g1(n3), g3(n0)), lab2(g2(n3), g3(n0))
          #lab1(n6: int, n7: int):
              n8 = __sil_mult_int(n6, n7)
              jmp lab
          #lab2(n10: int, n11: int):
              ret g3(m(n10, n11))
          #lab:
              throw g4(n8)
        }

        define empty() : void {
          #entry:
              ret null
        }|}


    let%expect_test _ =
      let module_ = parse_module input_text |> TextualTransform.remove_internal_calls in
      F.printf "%a" Module.pp module_ ;
      [%expect
        {|
        declare g1(int) : int

        declare g2(int) : int

        declare g3(int) : int

        declare g4(int) : *int

        declare m(int, int) : int

        define f(x: int, y: int) : int {
          #entry:
              n0:int = load &x
              n1:int = load &y
              n12 = g3(n0)
              n13 = g1(n0)
              n14 = g2(n1)
              n15 = m(n13, n14)
              n3 = __sil_mult_int(n12, n15)
              n16 = g3(n1)
              n4 = m(n0, n16)
              n17 = g1(n3)
              n18 = g3(n0)
              n19 = g2(n3)
              n20 = g3(n0)
              jmp lab1(n17, n18), lab2(n19, n20)

          #lab1(n6: int, n7: int):
              n8 = __sil_mult_int(n6, n7)
              jmp lab

          #lab2(n10: int, n11: int):
              n21 = m(n10, n11)
              n22 = g3(n21)
              ret n22

          #lab:
              n23 = g4(n8)
              throw n23

        }

        define empty() : void {
          #entry:
              ret null

        } |}]
  end )


let%test_module "let_propagation transformation" =
  ( module struct
    let input_text =
      {|

        define f(x: int, y: int) : int {
          #entry:
              n0:int = load &x
              n1:int = load &y
              n3 = __sil_mult_int(n0, n1)
              n4 = __sil_minusa(n3, n0)
              jmp lab(n4)
          #lab(n5: int):
              n6 = __sil_neg(n1)
              n7 = __sil_plusa(n6, n3)
              n8 = 42 // dead
              ret n7
        } |}


    let%expect_test _ =
      let module_ = parse_module input_text |> TextualTransform.let_propagation in
      F.printf "%a" Module.pp module_ ;
      [%expect
        {|
        define f(x: int, y: int) : int {
          #entry:
              n0:int = load &x
              n1:int = load &y
              jmp lab(__sil_minusa(__sil_mult_int(n0, n1), n0))

          #lab(n5: int):
              ret __sil_plusa(__sil_neg(n1), __sil_mult_int(n0, n1))

        } |}]
  end )


let%test_module "out-of-ssa transformation" =
  ( module struct
    let input_text =
      {|
          define f(x: int, y: int) : int {
            #entry:
                n0:int = load &x
                n1:int = load &y
                jmp lab1(n0, n1), lab3(n1, __sil_mult_int(n1, n0))

            #lab1(n2: int, n3: int):
                jmp lab2(n3, n2)

            #lab2(n4: int, n5: int):
                ret __sil_plusa(n4, n5)

            #lab3(n6: int, n7: int):
                jmp lab2(n6, n7)

        } |}


    let%expect_test _ =
      let module_ = parse_module input_text |> TextualTransform.out_of_ssa in
      F.printf "%a" Module.pp module_ ;
      [%expect
        {|
        define f(x: int, y: int) : int {
          #entry:
              n0:int = load &x
              n1:int = load &y
              store &__SSA2 <- n0:int
              store &__SSA3 <- n1:int
              store &__SSA6 <- n1:int
              store &__SSA7 <- __sil_mult_int(n1, n0):int
              jmp lab1, lab3

          #lab1:
              n2:int = load &__SSA2
              n3:int = load &__SSA3
              store &__SSA4 <- n3:int
              store &__SSA5 <- n2:int
              jmp lab2

          #lab2:
              n4:int = load &__SSA4
              n5:int = load &__SSA5
              ret __sil_plusa(n4, n5)

          #lab3:
              n6:int = load &__SSA6
              n7:int = load &__SSA7
              store &__SSA4 <- n6:int
              store &__SSA5 <- n7:int
              jmp lab2

        } |}]
  end )


let%test_module "keywords as ident" =
  ( module struct
    let input_text =
      {|
          define f(declare: int) : int {
            #type:
                jmp type

        } |}


    let%expect_test _ =
      let module_ = parse_module input_text |> TextualTransform.out_of_ssa in
      F.printf "%a" Module.pp module_ ;
      [%expect
        {|
          define f(declare: int) : int {
            #type:
                jmp type

          } |}]
  end )


let%test_module "line map" =
  ( module struct
    let text =
      {|
          // TEXTUAL UNIT START level1.hack
          .source_language = "hack"

          // .file "level1.hack"
          // .line 6
          define $root.taintSource(this: *void) : *HackInt {
          #b0:
          // .line 7
            n0 = $builtins.hhbc_is_type_int($builtins.hack_int(42))
            n1 = $builtins.hhbc_not(n0)
            jmp b1, b2
          #b1:
            prune $builtins.hack_is_true(n1)
            n2 = $builtins.hhbc_verify_failed()
            unreachable
          #b2:
            prune ! $builtins.hack_is_true(n1)
            ret $builtins.hack_int(42)
          }

          // .file "level1.hack"
          // .line 10
          define $root.taintSink(this: *void, $i: *HackInt) : *void {
          #b0:
          // .line 11
            ret $builtins.hack_null()
          }

          // .file "level1.hack"
          // .line 13
          define $root.FN_basicFlowBad(this: *void) : *void {
          local $tainted: *void
          #b0:
          // .line 14
            n0 = $root.taintSource(null)
            store &$tainted <- n0: *Mixed
          // .line 15
            n1: *Mixed = load &$tainted
            n2 = $root.taintSink(null, n1)
          // .line 16
            ret $builtins.hack_null()
          }

          // .file "level1.hack"
          // .line 18
          define $root.basicFlowOk(this: *void, $untainted: *HackInt) : *void {
          #b0:
          // .line 19
            n0: *Mixed = load &$untainted
            n1 = $root.taintSink(null, n0)
          // .line 20
            ret $builtins.hack_null()
          }
          // TEXTUAL UNIT END level1.hack
      |}


    let%expect_test _ =
      let line_map = LineMap.create text in
      let lines = String.split_lines text in
      List.iteri lines ~f:(fun i text ->
          let line = LineMap.find line_map i in
          F.printf "%i: %s\n" (Option.value_exn line) text ) ;
      [%expect
        {|
        1:
        1:           // TEXTUAL UNIT START level1.hack
        1:           .source_language = "hack"
        1:
        1:           // .file "level1.hack"
        1:           // .line 6
        6:           define $root.taintSource(this: *void) : *HackInt {
        6:           #b0:
        6:           // .line 7
        7:             n0 = $builtins.hhbc_is_type_int($builtins.hack_int(42))
        7:             n1 = $builtins.hhbc_not(n0)
        7:             jmp b1, b2
        7:           #b1:
        7:             prune $builtins.hack_is_true(n1)
        7:             n2 = $builtins.hhbc_verify_failed()
        7:             unreachable
        7:           #b2:
        7:             prune ! $builtins.hack_is_true(n1)
        7:             ret $builtins.hack_int(42)
        7:           }
        7:
        7:           // .file "level1.hack"
        7:           // .line 10
        10:           define $root.taintSink(this: *void, $i: *HackInt) : *void {
        10:           #b0:
        10:           // .line 11
        11:             ret $builtins.hack_null()
        11:           }
        11:
        11:           // .file "level1.hack"
        11:           // .line 13
        13:           define $root.FN_basicFlowBad(this: *void) : *void {
        13:           local $tainted: *void
        13:           #b0:
        13:           // .line 14
        14:             n0 = $root.taintSource(null)
        14:             store &$tainted <- n0: *Mixed
        14:           // .line 15
        15:             n1: *Mixed = load &$tainted
        15:             n2 = $root.taintSink(null, n1)
        15:           // .line 16
        16:             ret $builtins.hack_null()
        16:           }
        16:
        16:           // .file "level1.hack"
        16:           // .line 18
        18:           define $root.basicFlowOk(this: *void, $untainted: *HackInt) : *void {
        18:           #b0:
        18:           // .line 19
        19:             n0: *Mixed = load &$untainted
        19:             n1 = $root.taintSink(null, n0)
        19:           // .line 20
        20:             ret $builtins.hack_null()
        20:           }
        20:           // TEXTUAL UNIT END level1.hack
        20: |}]
  end )
