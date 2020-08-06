(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2
module F = Format

let assert_has_nullability_info ?expected_file ?expected_line storage unique_repr
    ~expected_nullability =
  match ThirdPartyAnnotationInfo.find_nullability_info storage unique_repr with
  | None ->
      assert_failure
        (F.asprintf "Expected to find info for %a, but it was not found"
           ThirdPartyAnnotationInfo.pp_unique_repr unique_repr)
  | Some {filename; line_number; signature} ->
      let expected_ret, expected_param_nullability = expected_nullability in
      let expected_params =
        List.zip_exn unique_repr.ThirdPartyAnnotationInfo.param_types expected_param_nullability
      in
      let expected_signature =
        { ThirdPartyMethod.class_name= unique_repr.ThirdPartyAnnotationInfo.class_name
        ; method_name= unique_repr.ThirdPartyAnnotationInfo.method_name
        ; ret_nullability= expected_ret
        ; params= expected_params }
      in
      assert_equal expected_signature signature
        ~msg:
          (F.asprintf "Signature for %a does not match" ThirdPartyAnnotationInfo.pp_unique_repr
             unique_repr)
        ~printer:(Pp.string_of_pp ThirdPartyMethod.pp) ;
      Option.iter expected_file ~f:(fun expected_file ->
          assert_equal expected_file filename ~msg:"Filename does not match" ) ;
      Option.iter expected_line ~f:(fun expected_line ->
          assert_equal expected_line line_number ~msg:"Line number does not match" )


let assert_no_info storage unique_repr =
  match ThirdPartyAnnotationInfo.find_nullability_info storage unique_repr with
  | None ->
      ()
  | Some {signature} ->
      assert_failure
        (F.asprintf "Did not expect to find nullability info for method %a, but found %a"
           ThirdPartyAnnotationInfo.pp_unique_repr unique_repr ThirdPartyMethod.pp signature)


let add_from_annot_file_and_check_success storage ~filename ~lines =
  match ThirdPartyAnnotationInfo.add_from_signature_file storage ~filename ~lines with
  | Ok storage ->
      storage
  | Error parsing_error ->
      assert_failure
        (F.asprintf "Expected to parse the file, but it was unparsable: %a"
           ThirdPartyAnnotationInfo.pp_parsing_error parsing_error)


let add_from_annot_file_and_check_failure storage ~filename ~lines ~expected_error_line_number =
  match ThirdPartyAnnotationInfo.add_from_signature_file storage ~filename ~lines with
  | Ok _ ->
      assert_failure
        "Expected to not be able to parse the file, but it was successfully parsed instead"
  | Error {line_number} ->
      assert_equal expected_error_line_number line_number ~msg:"Error line number does not match"
        ~printer:string_of_int


let basic_find =
  let open ThirdPartyMethod in
  "basic_find"
  >:: fun _ ->
  let lines = ["a.A#foo(b.B)"; "b.B#bar(c.C, @Nullable d.D) @Nullable"] in
  (* Load some functions from the file *)
  let storage =
    add_from_annot_file_and_check_success ~filename:"test.sig"
      (ThirdPartyAnnotationInfo.create_storage ())
      ~lines
  in
  (* Make sure we can find what we just stored *)
  assert_has_nullability_info storage
    {class_name= "a.A"; method_name= Method "foo"; param_types= ["b.B"]}
    ~expected_nullability:(Nonnull, [Nonnull])
    ~expected_file:"test.sig" ~expected_line:1 ;
  assert_has_nullability_info storage
    {class_name= "b.B"; method_name= Method "bar"; param_types= ["c.C"; "d.D"]}
    ~expected_nullability:(Nullable, [Nonnull; Nullable])
    ~expected_file:"test.sig" ~expected_line:2 ;
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


let disregards_whitespace_lines_and_comments =
  let open ThirdPartyMethod in
  "disregards_comments"
  >:: fun _ ->
  let lines = [" "; "a.A#foo(b.B)"; ""; "// a.A#bar(b.B)"; "// Hello world"] in
  (* Load some functions from the file *)
  let storage =
    add_from_annot_file_and_check_success ~filename:"test.sig"
      (ThirdPartyAnnotationInfo.create_storage ())
      ~lines
  in
  assert_has_nullability_info storage
    {class_name= "a.A"; method_name= Method "foo"; param_types= ["b.B"]}
    ~expected_nullability:(Nonnull, [Nonnull])
    ~expected_file:"test.sig" ~expected_line:2 ;
  (* Commented out signatures should be ignored *)
  assert_no_info storage {class_name= "a.A"; method_name= Method "bar"; param_types= ["b.B"]}


let overload_resolution =
  let open ThirdPartyMethod in
  "overload_resolution"
  >:: fun _ ->
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
  let storage =
    add_from_annot_file_and_check_success ~filename:"test.sig"
      (ThirdPartyAnnotationInfo.create_storage ())
      ~lines
  in
  (* Make sure we can find what we just stored *)
  (* a.b.SomeClass.foo with 1 param *)
  assert_has_nullability_info storage
    {class_name= "a.b.SomeClass"; method_name= Method "foo"; param_types= ["a.b.C1"]}
    ~expected_nullability:(Nullable, [Nullable]) ;
  assert_has_nullability_info storage
    {class_name= "a.b.SomeClass"; method_name= Method "foo"; param_types= ["a.b.C2"]}
    ~expected_nullability:(Nonnull, [Nullable]) ;
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
    ~expected_nullability:(Nullable, [Nullable; Nullable; Nonnull]) ;
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
    ~expected_nullability:(Nonnull, []) ;
  assert_has_nullability_info storage
    {class_name= "a.b.SomeClass"; method_name= Constructor; param_types= ["a.b.C1"]}
    ~expected_nullability:(Nonnull, [Nonnull]) ;
  assert_has_nullability_info storage
    {class_name= "a.b.SomeClass"; method_name= Constructor; param_types= ["a.b.C2"]}
    ~expected_nullability:(Nonnull, [Nullable]) ;
  (* wrong param type *)
  assert_no_info storage
    {class_name= "a.b.SomeClass"; method_name= Constructor; param_types= ["a.b.C3"]}


let can_add_several_files =
  "can_add_several_files"
  >:: fun _ ->
  let open ThirdPartyMethod in
  (* 1. Add file and check if we added info *)
  let file1 = ["a.A#foo(b.B)"; "b.B#bar(c.C, @Nullable d.D) @Nullable"] in
  let storage =
    add_from_annot_file_and_check_success
      (ThirdPartyAnnotationInfo.create_storage ())
      ~filename:"file1.sig" ~lines:file1
  in
  assert_has_nullability_info storage
    {class_name= "a.A"; method_name= Method "foo"; param_types= ["b.B"]}
    ~expected_nullability:(Nonnull, [Nonnull])
    ~expected_file:"file1.sig" ~expected_line:1 ;
  (* 2. Add another file and check if we added info *)
  let file2 = ["e.E#baz(f.F)"; "g.G#<init>(h.H, @Nullable i.I) @Nullable"] in
  let storage = add_from_annot_file_and_check_success storage ~filename:"file2.sig" ~lines:file2 in
  assert_has_nullability_info storage
    {class_name= "e.E"; method_name= Method "baz"; param_types= ["f.F"]}
    ~expected_nullability:(Nonnull, [Nonnull])
    ~expected_file:"file2.sig" ~expected_line:1 ;
  (* 3. Ensure we did not forget the content from the first file *)
  assert_has_nullability_info storage
    {class_name= "a.A"; method_name= Method "foo"; param_types= ["b.B"]}
    ~expected_nullability:(Nonnull, [Nonnull])
    ~expected_file:"file1.sig" ~expected_line:1


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
  add_from_annot_file_and_check_success (ThirdPartyAnnotationInfo.create_storage ()) ~lines:file_ok
  |> ignore ;
  add_from_annot_file_and_check_failure ~filename:"test.sig"
    (ThirdPartyAnnotationInfo.create_storage ())
    ~lines:file_bad ~expected_error_line_number:2


let assert_third_party storage ~package ~expected_filename =
  match ThirdPartyAnnotationInfo.lookup_related_sig_file storage ~package with
  | None ->
      assert_failure (F.sprintf "Did not find %s" package)
  | Some sig_file_name ->
      assert_equal expected_filename sig_file_name ~msg:"Unexpected sig file" ~printer:(fun s -> s)


let assert_not_third_party storage ~package =
  match ThirdPartyAnnotationInfo.lookup_related_sig_file storage ~package with
  | None ->
      ()
  | Some sig_file_name ->
      assert_failure
        (F.sprintf "Did not expect %s to be third-party, but it was wrongly attributed to %s"
           package sig_file_name)


let test_is_third_party =
  "test_is_third_party"
  >:: fun _ ->
  let storage =
    ThirdPartyAnnotationInfo.create_storage ()
    |> add_from_annot_file_and_check_success ~filename:"aaa.bbb.sig" ~lines:[]
    |> add_from_annot_file_and_check_success ~filename:"aaa.bbb.ccc.sig" ~lines:[]
    |> add_from_annot_file_and_check_success ~filename:"bbb.ccc.sig" ~lines:[]
  in
  assert_not_third_party storage ~package:"aaa" ;
  assert_third_party storage ~package:"aaa.bbb" ~expected_filename:"aaa.bbb.sig" ;
  assert_third_party storage ~package:"aaa.bbb.xxx.yyy" ~expected_filename:"aaa.bbb.sig" ;
  assert_third_party storage ~package:"aaa.bbb.ccc" ~expected_filename:"aaa.bbb.ccc.sig" ;
  assert_third_party storage ~package:"aaa.bbb.ccc.ddd.eee" ~expected_filename:"aaa.bbb.ccc.sig" ;
  assert_not_third_party storage ~package:"aaa.ccc"


let test =
  "ThirdPartyAnnotationInfoTests"
  >::: [ basic_find
       ; disregards_whitespace_lines_and_comments
       ; overload_resolution
       ; can_add_several_files
       ; should_not_forgive_unparsable_strings
       ; test_is_third_party ]
