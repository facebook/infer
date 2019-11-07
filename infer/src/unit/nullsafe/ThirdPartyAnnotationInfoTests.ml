(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2
module F = Format

let assert_has_nullability_info storage unique_repr ~expected_nullability =
  match ThirdPartyAnnotationInfo.find_nullability_info storage unique_repr with
  | None ->
      assert_failure
        (F.asprintf "Expected to find info for %a, but it was not found"
           ThirdPartyMethod.pp_unique_repr unique_repr)
  | Some nullability ->
      assert_equal expected_nullability nullability
        ~msg:
          (F.asprintf "Nullability info for %a does not match" ThirdPartyMethod.pp_unique_repr
             unique_repr)
        ~printer:(Pp.string_of_pp ThirdPartyMethod.pp_nullability)


let assert_no_info storage unique_repr =
  match ThirdPartyAnnotationInfo.find_nullability_info storage unique_repr with
  | None ->
      ()
  | Some nullability ->
      assert_failure
        (F.asprintf "Did not expect to find nullability info for method %a, but found %a"
           ThirdPartyMethod.pp_unique_repr unique_repr ThirdPartyMethod.pp_nullability nullability)


let add_from_annot_file_and_check_success storage ~lines =
  ThirdPartyAnnotationInfo.add_from_signature_file storage ~lines
  |> Result.iter_error ~f:(fun parsing_error ->
         assert_failure
           (F.asprintf "Expected to parse the file, but it was unparsable: %a"
              ThirdPartyAnnotationInfo.pp_parsing_error parsing_error) )


let add_from_annot_file_and_check_failure storage ~lines ~expected_error_line_number =
  match ThirdPartyAnnotationInfo.add_from_signature_file storage ~lines with
  | Ok () ->
      assert_failure
        "Expected to not be able to parse the file, but it was successfully parsed instead"
  | Error {line_number} ->
      assert_equal expected_error_line_number line_number ~msg:"Error line number does not match"
        ~printer:string_of_int


let basic_find =
  let open ThirdPartyMethod in
  "basic_find"
  >:: fun _ ->
  let storage = ThirdPartyAnnotationInfo.create_storage () in
  let lines = ["a.A#foo(b.B)"; "b.B#bar(c.C, @Nullable d.D) @Nullable"] in
  (* Load some functions from the file *)
  add_from_annot_file_and_check_success storage ~lines ;
  (* Make sure we can find what we just stored *)
  assert_has_nullability_info storage
    {class_name= "a.A"; method_name= Method "foo"; param_types= ["b.B"]}
    ~expected_nullability:{ret_nullability= Nonnull; param_nullability= [Nonnull]} ;
  assert_has_nullability_info storage
    {class_name= "b.B"; method_name= Method "bar"; param_types= ["c.C"; "d.D"]}
    ~expected_nullability:{ret_nullability= Nullable; param_nullability= [Nonnull; Nullable]} ;
  (* Make sure we can not find stuff we did not store *)
  (* Wrong class name *)
  assert_no_info storage {class_name= "a.AB"; method_name= Method "foo"; param_types= ["b.B"]} ;
  (* Wrong method name *)
  assert_no_info storage {class_name= "a.A"; method_name= Method "foo1"; param_types= ["b.B"]} ;
  (* Wrong param type *)
  assert_no_info storage {class_name= "a.A"; method_name= Method "foo"; param_types= ["c.C"]} ;
  (* Not enough params *)
  assert_no_info storage {class_name= "a.A"; method_name= Method "foo"; param_types= []} ;
  (* Too many params *)
  assert_no_info storage {class_name= "a.A"; method_name= Method "foo"; param_types= ["b.B"; "c.C"]}


let overload_resolution =
  let open ThirdPartyMethod in
  "overload_resolution"
  >:: fun _ ->
  let storage = ThirdPartyAnnotationInfo.create_storage () in
  let lines =
    [ "a.b.SomeClass#foo(@Nullable a.b.C1) @Nullable"
    ; "a.b.SomeClass#<init>(a.b.C1)"
    ; "a.b.SomeClass#foo(@Nullable a.b.C1, @Nullable a.b.C3, c.d.C4) @Nullable"
    ; "c.d.OtherClass#foo(@Nullable a.b.C2)"
    ; "a.b.SomeClass#<init>()"
    ; "a.b.SomeClass#<init>(@Nullable a.b.C2)"
    ; "a.b.SomeClass#foo(@Nullable a.b.C2)" ]
  in
  (* Load some functions from the file *)
  add_from_annot_file_and_check_success storage ~lines ;
  (* Make sure we can find what we just stored *)
  (* a.b.SomeClass.foo with 1 param *)
  assert_has_nullability_info storage
    {class_name= "a.b.SomeClass"; method_name= Method "foo"; param_types= ["a.b.C1"]}
    ~expected_nullability:{ret_nullability= Nullable; param_nullability= [Nullable]} ;
  assert_has_nullability_info storage
    {class_name= "a.b.SomeClass"; method_name= Method "foo"; param_types= ["a.b.C2"]}
    ~expected_nullability:{ret_nullability= Nonnull; param_nullability= [Nullable]} ;
  (* wrong type *)
  assert_no_info storage
    {class_name= "a.b.SomeClass"; method_name= Method "foo"; param_types= ["a.b.C3"]} ;
  (* wrong class *)
  assert_no_info storage
    {class_name= "a.b.c.SomeClass"; method_name= Method "foo"; param_types= ["a.b.C1"]} ;
  (* wrong method name *)
  assert_no_info storage
    {class_name= "a.b.SomeClass"; method_name= Method "bar"; param_types= ["a.b.C1"]} ;
  (* a.b.SomeClass.foo with many params *)
  assert_has_nullability_info storage
    { class_name= "a.b.SomeClass"
    ; method_name= Method "foo"
    ; param_types= ["a.b.C1"; "a.b.C3"; "c.d.C4"] }
    ~expected_nullability:
      {ret_nullability= Nullable; param_nullability= [Nullable; Nullable; Nonnull]} ;
  (* wrong param order *)
  assert_no_info storage
    { class_name= "a.b.SomeClass"
    ; method_name= Method "foo"
    ; param_types= ["a.b.C3"; "a.b.C1"; "c.d.C4"] } ;
  (* third param is missing *)
  assert_no_info storage
    {class_name= "a.b.SomeClass"; method_name= Method "foo"; param_types= ["a.b.C1"; "a.b.C3"]} ;
  (* possibility of constructor overload should be respected *)
  assert_has_nullability_info storage
    {class_name= "a.b.SomeClass"; method_name= Constructor; param_types= []}
    ~expected_nullability:{ret_nullability= Nonnull; param_nullability= []} ;
  assert_has_nullability_info storage
    {class_name= "a.b.SomeClass"; method_name= Constructor; param_types= ["a.b.C1"]}
    ~expected_nullability:{ret_nullability= Nonnull; param_nullability= [Nonnull]} ;
  assert_has_nullability_info storage
    {class_name= "a.b.SomeClass"; method_name= Constructor; param_types= ["a.b.C2"]}
    ~expected_nullability:{ret_nullability= Nonnull; param_nullability= [Nullable]} ;
  (* wrong param type *)
  assert_no_info storage
    {class_name= "a.b.SomeClass"; method_name= Constructor; param_types= ["a.b.C3"]}


let can_add_several_files =
  "can_add_several_files"
  >:: fun _ ->
  let open ThirdPartyMethod in
  let storage = ThirdPartyAnnotationInfo.create_storage () in
  (* 1. Add file and check if we added info *)
  let file1 = ["a.A#foo(b.B)"; "b.B#bar(c.C, @Nullable d.D) @Nullable"] in
  add_from_annot_file_and_check_success storage ~lines:file1 ;
  assert_has_nullability_info storage
    {class_name= "a.A"; method_name= Method "foo"; param_types= ["b.B"]}
    ~expected_nullability:{ret_nullability= Nonnull; param_nullability= [Nonnull]} ;
  (* 2. Add another file and check if we added info *)
  let file2 = ["e.E#baz(f.F)"; "g.G#<init>(h.H, @Nullable i.I) @Nullable"] in
  add_from_annot_file_and_check_success storage ~lines:file2 ;
  assert_has_nullability_info storage
    {class_name= "e.E"; method_name= Method "baz"; param_types= ["f.F"]}
    ~expected_nullability:{ret_nullability= Nonnull; param_nullability= [Nonnull]} ;
  (* 3. Ensure we did not forget the content from the first file *)
  assert_has_nullability_info storage
    {class_name= "a.A"; method_name= Method "foo"; param_types= ["b.B"]}
    ~expected_nullability:{ret_nullability= Nonnull; param_nullability= [Nonnull]}


let should_not_forgive_unparsable_strings =
  "should_not_forgive_unparsable_strings"
  >:: fun _ ->
  let line1 = "a.b.SomeClass#foo(@Nullable a.b.C1) @Nullable" in
  let line2_ok = "a.b.SomeClass#<init>(a.b.C1)" in
  (* like line2_ok, but one extra open parenthesis *)
  let line2_bad = "a.b.SomeClass#<init>((a.b.C1)" in
  let line3 = "a.b.SomeClass#foo(@Nullable a.b.C1, @Nullable a.b.C3, c.d.C4) @Nullable" in
  let file_ok = [line1; line2_ok; line3] in
  let file_bad = [line1; line2_bad; line3] in
  (* Ensure we can add the good file, but can not add the bad one *)
  add_from_annot_file_and_check_success (ThirdPartyAnnotationInfo.create_storage ()) ~lines:file_ok ;
  add_from_annot_file_and_check_failure
    (ThirdPartyAnnotationInfo.create_storage ())
    ~lines:file_bad ~expected_error_line_number:2


let test =
  "ThirdPartyAnnotationInfoTests"
  >::: [ basic_find
       ; overload_resolution
       ; can_add_several_files
       ; should_not_forgive_unparsable_strings ]
