(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open TextualTestHelpers

(* Swift's [Double] (mangled name [TSd]) is the standard 64-bit floating-point
   type and on 64-bit platforms is what [CGFloat] is typedef'd to. The verifier
   recognizes it as compatible with [Float] in arithmetic and assignment
   contexts, the same way it does for [CGFloat] and Swift's [Float] ([TSf]). *)
let text_double_as_float =
  {|
       .source_language = "swift"
       .source_file = "fake.sil"

       declare make_double() : *__infer_swift_type<TSd>

       define returns_double_as_float() : float {
         #start:
           n0 = make_double()
           ret n0
       }
       |}


let%expect_test "Swift Double satisfies Float-typed return" =
  parse_string_and_verify_keep_going text_double_as_float ;
  [%expect
    {|
    verification succeeded - no warnings
    ------
    .source_language = "swift"

    .source_file = "fake.sil"

    declare make_double() : *__infer_swift_type<TSd>

    define returns_double_as_float() : float {
      #start:
          n0 = make_double()
          ret n0

    }


    Veryfing the transformed module...
    verification succeeded
    |}]


(* In Swift mode, when a [Load]/[Store] with [typ= None] cannot have its type
   recovered during type inference (e.g., the loaded expression is not a
   pointer), the verifier falls back to [Some Void] so SIL emission does not
   later raise the [TextualTransformError "to_sil should come after type
   inference"] and abandon the whole procedure. Other frontends are left
   unchanged. *)
let text_load_typ_fallback =
  {|
       .source_language = "swift"
       .source_file = "fake.sil"

       define f() : void {
         #start:
           n0 = load 42
           ret null
       }
       |}


let%expect_test "Swift Load with un-inferrable typ falls back to Void" =
  parse_string_and_verify_keep_going_lenient text_load_typ_fallback ;
  [%expect
    {|
    lenient verification succeeded - no warnings
    ------
    .source_language = "swift"

    .source_file = "fake.sil"

    define f() : void {
      #start:
          n0:void = load 42
          ret null

    }


    Verifying the transformed module...
    verification succeeded
    |}]


(* The CG geometry value types ([CGPoint], [CGSize], [CGRect]) are structs
   whose fields are all [CGFloat]s. The Textual frontend sometimes drops the
   field-selection step, so the verifier sees a raw struct pointer flowing
   into a [*float] slot (or vice versa). The two patterns below mirror what
   we observe in production logs:

   1. [**__infer_swift_type<struct::CGPoint>] given for a [*float] slot —
      [compat] unwraps both pointers once, then matches [Float] against
      [Ptr (Struct CGPoint)] via [is_float_swift].
   2. [**float] given for a [**__infer_swift_type<TSo6CGSizeV,CGSize>] slot —
      [compat] unwraps both pointers twice, then matches [Float] against
      bare [Struct CGSize], which [is_float_swift] also accepts. *)
let text_cgpoint_double_ptr_as_float_ptr =
  {|
       .source_language = "swift"
       .source_file = "fake.sil"

       declare make_cgpoint_pp() : **__infer_swift_type<struct::CGPoint>

       define returns_cgpoint_pp_as_float_p() : *float {
         #start:
           n0 = make_cgpoint_pp()
           ret n0
       }
       |}


let%expect_test "Swift **CGPoint satisfies *float return" =
  parse_string_and_verify_keep_going text_cgpoint_double_ptr_as_float_ptr ;
  [%expect
    {|
    verification succeeded - no warnings
    ------
    .source_language = "swift"

    .source_file = "fake.sil"

    declare make_cgpoint_pp() : **__infer_swift_type<struct::CGPoint>

    define returns_cgpoint_pp_as_float_p() : *float {
      #start:
          n0 = make_cgpoint_pp()
          ret n0

    }


    Veryfing the transformed module...
    verification succeeded
    |}]


let text_cgsize_pp_satisfied_by_float_pp =
  {|
       .source_language = "swift"
       .source_file = "fake.sil"

       declare make_float_pp() : **float

       define returns_float_pp_as_cgsize_pp() : **__infer_swift_type<TSo6CGSizeV,CGSize> {
         #start:
           n0 = make_float_pp()
           ret n0
       }
       |}


let%expect_test "Swift **CGSize return slot accepts **float" =
  parse_string_and_verify_keep_going text_cgsize_pp_satisfied_by_float_pp ;
  [%expect
    {|
    verification succeeded - no warnings
    ------
    .source_language = "swift"

    .source_file = "fake.sil"

    declare make_float_pp() : **float

    define returns_float_pp_as_cgsize_pp() : **__infer_swift_type<TSo6CGSizeV,CGSize> {
      #start:
          n0 = make_float_pp()
          ret n0

    }


    Veryfing the transformed module...
    verification succeeded
    |}]


let text_cgrect_double_ptr_as_float_ptr =
  {|
       .source_language = "swift"
       .source_file = "fake.sil"

       declare make_cgrect_pp() : **__infer_swift_type<TSo6CGRectV,CGRect>

       define returns_cgrect_pp_as_float_p() : *float {
         #start:
           n0 = make_cgrect_pp()
           ret n0
       }
       |}


let%expect_test "Swift **CGRect satisfies *float return" =
  parse_string_and_verify_keep_going text_cgrect_double_ptr_as_float_ptr ;
  [%expect
    {|
    verification succeeded - no warnings
    ------
    .source_language = "swift"

    .source_file = "fake.sil"

    declare make_cgrect_pp() : **__infer_swift_type<TSo6CGRectV,CGRect>

    define returns_cgrect_pp_as_float_p() : *float {
      #start:
          n0 = make_cgrect_pp()
          ret n0

    }


    Veryfing the transformed module...
    verification succeeded
    |}]


(* [__infer_swift_type<ptr_elt>] is the canonical Swift "any type" placeholder
   the frontend emits when LLVM-level pointer-element types cannot be resolved
   (see [BaseTypeName.swift_any_type_name] and [Typ.any_type_swift] in
   [Textual.ml]). It already participates in [is_any_type_llvm] at the bare-
   struct level. The pattern below — a [**__infer_swift_type<ptr_elt>] flowing
   into a [*float] slot — peels both pointers once and lands on
   [Float] vs [Ptr (Struct ptr_elt)]. The guarded [is_float_swift] case then
   falls through to the [Ptr _, _ | _, Ptr _] arm, which consults
   [is_any_type_llvm] on the inner [Struct ptr_elt] and accepts it. *)
let text_ptr_elt_double_ptr_as_float_ptr =
  {|
       .source_language = "swift"
       .source_file = "fake.sil"

       declare make_ptr_elt_pp() : **__infer_swift_type<ptr_elt>

       define returns_ptr_elt_pp_as_float_p() : *float {
         #start:
           n0 = make_ptr_elt_pp()
           ret n0
       }
       |}


let%expect_test "Swift **ptr_elt satisfies *float return" =
  parse_string_and_verify_keep_going text_ptr_elt_double_ptr_as_float_ptr ;
  [%expect
    {|
    verification succeeded - no warnings
    ------
    .source_language = "swift"

    .source_file = "fake.sil"

    declare make_ptr_elt_pp() : **__infer_swift_type<ptr_elt>

    define returns_ptr_elt_pp_as_float_p() : *float {
      #start:
          n0 = make_ptr_elt_pp()
          ret n0

    }


    Veryfing the transformed module...
    verification succeeded
    |}]


(* [CGVector] (struct of 2 [CGFloat]s) and [UIEdgeInsets] (struct of 4
   [CGFloat]s) follow the same logic as [CGPoint]/[CGSize]/[CGRect]: simple
   value structs whose memory layout is layout-compatible with a [*CGFloat]
   at the address of their first field. The two patterns below mirror what
   we observe in production logs (METAGeometrySwift extensions for CGVector,
   IGSticker UI helpers for UIEdgeInsets). *)
let text_cgvector_double_ptr_as_float_ptr =
  {|
       .source_language = "swift"
       .source_file = "fake.sil"

       declare make_cgvector_pp() : **__infer_swift_type<struct::CGVector>

       define returns_cgvector_pp_as_float_p() : *float {
         #start:
           n0 = make_cgvector_pp()
           ret n0
       }
       |}


let%expect_test "Swift **CGVector satisfies *float return" =
  parse_string_and_verify_keep_going text_cgvector_double_ptr_as_float_ptr ;
  [%expect
    {|
    verification succeeded - no warnings
    ------
    .source_language = "swift"

    .source_file = "fake.sil"

    declare make_cgvector_pp() : **__infer_swift_type<struct::CGVector>

    define returns_cgvector_pp_as_float_p() : *float {
      #start:
          n0 = make_cgvector_pp()
          ret n0

    }


    Veryfing the transformed module...
    verification succeeded
    |}]


let text_uiedgeinsets_pp_satisfied_by_float_pp =
  {|
       .source_language = "swift"
       .source_file = "fake.sil"

       declare make_float_pp() : **float

       define returns_float_pp_as_uiedgeinsets_pp() : **__infer_swift_type<TSo12UIEdgeInsetsV,UIEdgeInsets> {
         #start:
           n0 = make_float_pp()
           ret n0
       }
       |}


let%expect_test "Swift **UIEdgeInsets return slot accepts **float" =
  parse_string_and_verify_keep_going text_uiedgeinsets_pp_satisfied_by_float_pp ;
  [%expect
    {|
    verification succeeded - no warnings
    ------
    .source_language = "swift"

    .source_file = "fake.sil"

    declare make_float_pp() : **float

    define returns_float_pp_as_uiedgeinsets_pp() : **__infer_swift_type<TSo12UIEdgeInsetsV,UIEdgeInsets> {
      #start:
          n0 = make_float_pp()
          ret n0

    }


    Veryfing the transformed module...
    verification succeeded
    |}]
