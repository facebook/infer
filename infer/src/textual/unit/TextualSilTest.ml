(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module T = Textual
open TextualTestHelpers

let%expect_test _ =
  let no_lang = {|define nothing() : void { #node: ret null }|} in
  let m = parse_module no_lang in
  try TextualSil.module_to_sil m |> ignore
  with T.TextualTransformError errs ->
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
                     Foo.f#2
                   }
         exported_obj_methods: {}
         annots: {<>}
         class_info: {HackClassInfo (Class)}
         dummy: false
         source_file: dummy.sil
         hack Quux
         fields: {}
         statics: {}
         supers: {}
         objc_protocols: {}
         methods: {}
         exported_obj_methods: {}
         annots: {<>}
         class_info: {NoInfo}
         dummy: true
         hack Baz
         fields: {}
         statics: {}
         supers: {}
         objc_protocols: {}
         methods: {}
         exported_obj_methods: {}
         annots: {<>}
         class_info: {NoInfo}
         dummy: true
         hack Bar
         fields: {}
         statics: {}
         supers: {}
         objc_protocols: {}
         methods: {}
         exported_obj_methods: {}
         annots: {<>}
         class_info: {NoInfo}
         dummy: true |}]


let%expect_test "final annotation" =
  let source =
    {|
          .source_language = "hack"
          type Foo .final {}
          type Bar {}
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
      methods: {}
      exported_obj_methods: {}
      annots: {<_final>}
      class_info: {HackClassInfo (Class)}
      dummy: false
      source_file: dummy.sil
      hack Bar
      fields: {}
      statics: {}
      supers: {}
      objc_protocols: {}
      methods: {}
      exported_obj_methods: {}
      annots: {<>}
      class_info: {HackClassInfo (Class)}
      dummy: false
      source_file: dummy.sil |}]


let%expect_test "abstract class" =
  let source =
    {|
          .source_language = "hack"
          type Foo .abstract {}
          type Bar {}
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
      methods: {}
      exported_obj_methods: {}
      annots: {<>}
      class_info: {HackClassInfo (AbstractClass)}
      dummy: false
      source_file: dummy.sil
      hack Bar
      fields: {}
      statics: {}
      supers: {}
      objc_protocols: {}
      methods: {}
      exported_obj_methods: {}
      annots: {<>}
      class_info: {HackClassInfo (Class)}
      dummy: false
      source_file: dummy.sil |}]


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
        ; proc_id= foo#2 }
            #n1:

            #n4:
              n$0=*&x:HackMixed* [line 8, column 11];
              n$1=_fun_unknown(n$0:HackMixed*) [line 9, column 11];
              n$2=*&y:HackMixed* [line 10, column 11];
              n$3=_fun_known(n$2:HackInt*) [line 11, column 11];
              *&return:void=0 [line 12, column 11];

            #n2: |}]


let%expect_test "hack extends is ordered" =
  let source =
    {|
      .source_language = "hack"
      type A extends P0, P1, P2, P3, T0, T1, T2, T3 = .kind="class" { }

      type T3 = .kind="trait" {}
      type T0 = .kind="trait" {}
      type T2 = .kind="trait" {}
      type T1 = .kind="trait" {}

      type P3 = .kind="class" {}
      type P1 = .kind="class" {}
      type P0 = .kind="class" {}
      type P2 = .kind="class" {}

      |}
  in
  let m = parse_module source in
  let _, tenv = TextualSil.module_to_sil m in
  let name = IR.Typ.HackClass (IR.HackClassName.make "A") in
  let supers = Tenv.fold_supers tenv name ~init:[] ~f:(fun name _ acc -> name :: acc) in
  F.printf "%a@\n" (Fmt.list ~sep:(Fmt.any " ") IR.Typ.Name.pp) (List.rev supers) ;
  [%expect {|
    hack A hack T0 hack T1 hack T2 hack T3 hack P0 hack P1 hack P2 hack P3 |}]


let%expect_test "overloads in tenv" =
  let source =
    {|
     .source_language = "hack"
     define C.f(x: int) : void { #n0: ret null }
     define C.f(x: int, y: bool) : void { #n0: ret null }
     |}
  in
  let m = parse_module source in
  let _, tenv = TextualSil.module_to_sil m in
  F.printf "%a" Tenv.pp tenv ;
  [%expect
    {|
    hack C
    fields: {}
    statics: {}
    supers: {}
    objc_protocols: {}
    methods: {}
    exported_obj_methods: {}
    annots: {<>}
    class_info: {NoInfo}
    dummy: true
    hack bool
    fields: {}
    statics: {}
    supers: {}
    objc_protocols: {}
    methods: {}
    exported_obj_methods: {}
    annots: {<>}
    class_info: {NoInfo}
    dummy: true |}]


let%expect_test "undefined + overloads in merged tenv" =
  let main_source =
    {|
     .source_language = "hack"

     declare Dep.f(...) : *HackMixed

     define Main.main(x: int, y: bool) : void {
       #b0:
         n0:int = load &x
         n1:bool = load &y
         n2 = Dep.f(n0)
         n3 = Dep.f(n0, n1)
         ret null
     }
     |}
  in
  let dep_source =
    {|
     .source_language = "hack"

     define Dep.f(x: int) : *int {
       #n0:
         ret null
     }
     define Dep.f(x: int, y: bool) : *float {
       #n0:
         ret null
     }
     |}
  in
  let tenvs =
    List.map [main_source; dep_source] ~f:(fun x ->
        parse_module x |> TextualSil.module_to_sil |> snd )
  in
  let tenv_merged = Tenv.create () in
  List.iter tenvs ~f:(fun tenv -> Tenv.merge ~src:tenv ~dst:tenv_merged) ;
  let dep_name = Typ.HackClass (HackClassName.make "Dep") in
  let dep_struct = Tenv.lookup tenv_merged dep_name |> Option.value_exn in
  F.printf "%a" (Struct.pp Pp.text dep_name) dep_struct ;
  [%expect
    {|
    hack Dep
    fields: {}
    statics: {}
    supers: {}
    objc_protocols: {}
    methods: {}
    exported_obj_methods: {}
    annots: {<>}
    class_info: {NoInfo}
    dummy: true |}]


let%expect_test "instanceof translation" =
  let source =
    {|
     .source_language = "hack"
     declare foo() : *HackMixed

     define bar() : int {
       #b0:
         n0 = foo()
         jmp b1, b2
       #b1:
         n1 = __sil_instanceof(n0, <*HackBool>)
         ret n1
       #b2:
         ret __sil_instanceof(n0, <*HackBool>)
     }
     |}
  in
  let m = parse_module source in
  let cfg, _ = TextualSil.module_to_sil m in
  Cfg.iter_sorted cfg ~f:(fun pdesc ->
      F.printf "%a" (Procdesc.pp_with_instrs ~print_types:true) pdesc ) ;
  [%expect
    {|
    { proc_name= bar
    ; translation_unit= dummy.sil
    ; formals= []
    ; is_defined= true
    ; loc= dummy.sil:5:12
    ; locals= []
    ; ret_type= int
    ; proc_id= bar#0 }
        #n1:

        #n4:
          n$0=_fun_foo() [line 7, column 9];

        #n5:
          n$1=_fun___instanceof(n$0:void*,sizeof(t=HackBool*;nullable=false):void) [line 10, column 9];
          *&return:int=n$1 [line 11, column 9];

        #n6:
          n$2=_fun___instanceof(n$0:void*,sizeof(t=HackBool*;nullable=false):void) [line 13, column 9];
          *&return:int=n$2 [line 13, column 9];

        #n2: |}]


let%expect_test "trait vs class kind" =
  let source =
    {|
          .source_language = "hack"

          type T = .kind="trait" {}
          type C = .kind="class" {}
    |}
  in
  let m = parse_module source in
  let _, tenv = TextualSil.module_to_sil m in
  F.printf "%a@\n" Tenv.pp tenv ;
  [%expect
    {|
      hack C
      fields: {}
      statics: {}
      supers: {}
      objc_protocols: {}
      methods: {}
      exported_obj_methods: {}
      annots: {<>}
      class_info: {HackClassInfo (Class)}
      dummy: false
      source_file: dummy.sil
      hack T
      fields: {}
      statics: {}
      supers: {}
      objc_protocols: {}
      methods: {}
      exported_obj_methods: {}
      annots: {<>}
      class_info: {HackClassInfo (Trait)}
      dummy: false
      source_file: dummy.sil |}]


let%expect_test "const" =
  let source =
    {|
     .source_language = "hack"

     type Uninit::A$static = .kind="class" .static {
       FIELD: .public .constant *HackMixed
     }
     |}
  in
  let m = parse_module source in
  let _, tenv = TextualSil.module_to_sil m in
  F.printf "%a@\n" Tenv.pp tenv ;
  [%expect
    {|
    hack HackMixed
    fields: {}
    statics: {}
    supers: {}
    objc_protocols: {}
    methods: {}
    exported_obj_methods: {}
    annots: {<>}
    class_info: {NoInfo}
    dummy: true
    hack Uninit::A$static
    fields: {
               HackMixed* const  FIELD <>
             }
    statics: {}
    supers: {}
    objc_protocols: {}
    methods: {}
    exported_obj_methods: {}
    annots: {<>}
    class_info: {HackClassInfo (Class)}
    dummy: false
    source_file: dummy.sil |}]
