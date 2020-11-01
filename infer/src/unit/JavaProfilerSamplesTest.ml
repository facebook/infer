(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2
module T = JProcname.JNI.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY

let test_jni_pp =
  let create_test input expected _ =
    let found = Format.asprintf "%a" T.pp input in
    let pp_diff fmt (expected, actual) =
      Format.fprintf fmt "Expected: '%s', found: '%s'" expected actual
    in
    assert_equal ~cmp:String.equal ~pp_diff expected found
  in
  [ ( "test_jni_pp_1"
    , T.(Method ([Int; Boolean; FullyQualifiedClass ("java.lang", "String")], Array Char))
    , "(IZLjava/lang/String;)[C" )
  ; ( "test_jni_pp_2"
    , (let open T in
      Method
        ( [ Array
              (Method
                 ( [Int; Method ([Long; Array (Array Long); Boolean], Long)]
                 , Array
                     (Array
                        (Array
                           (Method
                              ([Int; FullyQualifiedClass ("aaa.bbb", "Ccc"); Boolean], Array Char))))
                 )) ]
        , Void ))
    , "([(I(J[[JZ)J)[[[(ILaaa/bbb/Ccc;Z)[C)V" ) ]
  |> List.map ~f:(fun (name, test_input, expected_output) ->
         name >:: create_test test_input expected_output )


let test_jni_parse_method_str_with_invalid_input =
  let create_test input expected_exception _ =
    let run () = T.parse_method_str input in
    assert_raises expected_exception run
  in
  [ ( "test_jni_parse_method_str_with_empty_input"
    , ""
    , Logging.InferUserError "'' did not parse as one JNI method signature" )
  ; ( "test_jni_parse_method_str_with_valid_non_method_input"
    , "I"
    , Logging.InferUserError "'I' did not parse as one JNI method signature" ) ]
  |> List.map ~f:(fun (name, test_input, expected_exception) ->
         name >:: create_test test_input expected_exception )


let test_jni_parse_str_with_valid_input =
  let create_test input expected _ =
    let found = T.parse_str input in
    let pp_diff fmt (expected, actual) =
      Format.fprintf fmt "Expected: '%a', found: '%a'" (Format.pp_print_list T.pp) expected
        (Format.pp_print_list T.pp) actual
    in
    assert_equal ~cmp:(List.equal T.equal) ~pp_diff expected found
  in
  [ ( "test_jni_parse_str_with_method_signature"
    , "(IZLjava/lang/String;)[C"
    , T.[Method ([Int; Boolean; FullyQualifiedClass ("java.lang", "String")], Array Char)] )
  ; ( "test_jni_parse_str_with_multiple_separate_types"
    , "I[[[CIJ(I)[C"
    , T.[Int; Array (Array (Array Char)); Int; Long; Method ([Int], Array Char)] )
  ; ( "test_jni_parse_str_with_multiple_fully_qualified_classes"
    , "(Laaa/bbb/Ccc;Laaa/bbb/Ccc;)V"
    , let open T in
      [ Method
          ([FullyQualifiedClass ("aaa.bbb", "Ccc"); FullyQualifiedClass ("aaa.bbb", "Ccc")], Void)
      ] )
  ; ( "test_jni_parse_str_with_complex_method_signature_1"
    , "([[J(I)V)V"
    , T.[Method ([Array (Array Long); Method ([Int], Void)], Void)] )
  ; ( "test_jni_parse_str_with_complex_method_signature_2"
    , "([C()V)V"
    , T.[Method ([Array Char; Method ([], Void)], Void)] )
  ; ( "test_jni_parse_str_with_complex_method_signature_3"
    , "(J[[J)V"
    , T.[Method ([Long; Array (Array Long)], Void)] )
  ; ( "test_jni_parse_str_with_complex_method_signature_4"
    , "([(I(J[[JZ)J)[[[(ILaaa/bbb/Ccc;Z)[C)V"
    , let open T in
      [ Method
          ( [ Array
                (Method
                   ( [Int; Method ([Long; Array (Array Long); Boolean], Long)]
                   , Array
                       (Array
                          (Array
                             (Method
                                ([Int; FullyQualifiedClass ("aaa.bbb", "Ccc"); Boolean], Array Char))))
                   )) ]
          , Void ) ] )
  ; ( "test_jni_parse_str_with_empty_method_signature"
    , JProcname.JNI.void_method_with_no_arguments
    , T.[Method ([], Void)] )
  ; ("test_jni_parse_str_with_empty_input", "", []) ]
  |> List.map ~f:(fun (name, test_input, expected_output) ->
         name >:: create_test test_input expected_output )


let test_jni_parse_str_with_invalid_input =
  let create_test input expected_exception _ =
    let run () = T.parse_str input in
    assert_raises expected_exception run
  in
  [ ( "test_jni_parse_str_with_missing_semicolon"
    , "Ljava/lang/String"
    , Logging.InferUserError
        "Cannot find a semicolon symbol to delimit the L token. Failed parsing input" )
  ; ( "test_jni_parse_str_with_unrecognized_char"
    , "M"
    , Logging.InferUserError "Unrecognized char 'M' while reading the input sequence" )
  ; ( "test_jni_parse_str_with_no_reductions_in_a_scan"
    , "(((("
    , Logging.InferUserError "No symbols were reduced during a scan, failed parsing input" ) ]
  |> List.map ~f:(fun (name, test_input, expected_exception) ->
         name >:: create_test test_input expected_exception )


let test_from_json_string_with_valid_input =
  let create_test input expected ~use_signature _ =
    let found = JavaProfilerSamples.from_json_string input ~use_signature in
    assert_equal ~cmp:(List.equal JavaProfilerSamples.equal_labeled_profiler_sample) expected found
  in
  let input1 = "[{\"test\": \"label1\",\"methods\": []}]" in
  let expected1 = [("label1", Procname.Set.of_list [])] in
  let input2 =
    Printf.sprintf
      "[{\"foo\":{},\"test\": \"label1\",\"methods\": [{\"class\": \"ggg.hhh.Iii\", \"boo\": \"\", \
       \"method\": \"<clinit>\", \"signature\": \"(Ljava/lang/String;[IJ)V\",\"wat\": \
       \"\"},{\"class\": \"lll.mmm.Nnn\",\"boo\": \"\",\"method\": \"<init>\",\"signature\": \
       \"(Ljava/lang/String;[IJ)V\",\"wat\": \"\"}]},{\"boo\":\"aaa\",\"test\": \
       \"label2\",\"methods\": [{\"class\": \"aaa.bbb.Ccc\",\"boo\": \"\",\"method\": \
       \"methodOne\",\"signature\": \"%s\",\"wat\": \"\"},{\"class\": \"ddd.eee.Fff\",\"boo\": \
       \"\",\"method\": \"methodTwo\",\"signature\": \"(Ljava/lang/String;[IJ)[[C\",\"wat\": \
       \"\"}]}]"
      JProcname.JNI.void_method_with_no_arguments
  in
  let expected2 =
    [ ( "label1"
      , Procname.Set.of_list
          [ Procname.(
              make_java
                ~class_name:(Typ.Name.Java.from_string "lll.mmm.Nnn")
                ~return_type:None ~method_name:Java.constructor_method_name
                ~parameters:
                  [ StdTyp.Java.pointer_to_java_lang_string
                  ; Typ.(mk_ptr (mk_array StdTyp.int))
                  ; StdTyp.long ]
                ~kind:Java.Non_Static ())
          ; Procname.(
              make_java
                ~class_name:(Typ.Name.Java.from_string "ggg.hhh.Iii")
                ~return_type:None ~method_name:Java.class_initializer_method_name
                ~parameters:
                  [ StdTyp.Java.pointer_to_java_lang_string
                  ; Typ.(mk_ptr (mk_array StdTyp.int))
                  ; StdTyp.long ]
                ~kind:Java.Non_Static ()) ] )
    ; ( "label2"
      , Procname.Set.of_list
          [ Procname.(
              make_java
                ~class_name:(Typ.Name.Java.from_string "ddd.eee.Fff")
                ~return_type:(Some Typ.(mk_ptr (mk_array (mk_ptr (mk_array StdTyp.Java.char)))))
                ~method_name:"methodTwo"
                ~parameters:
                  [ StdTyp.Java.pointer_to_java_lang_string
                  ; Typ.(mk_ptr (mk_array StdTyp.int))
                  ; StdTyp.long ]
                ~kind:Java.Non_Static ())
          ; Procname.(
              make_java
                ~class_name:(Typ.Name.Java.from_string "aaa.bbb.Ccc")
                ~return_type:(Some StdTyp.void) ~method_name:"methodOne" ~parameters:[]
                ~kind:Java.Non_Static ()) ] ) ]
  in
  let expected3 =
    [ ( "label1"
      , Procname.Set.of_list
          [ Procname.(
              make_java
                ~class_name:(Typ.Name.Java.from_string "lll.mmm.Nnn")
                ~return_type:None ~method_name:Java.constructor_method_name ~parameters:[]
                ~kind:Java.Non_Static ())
          ; Procname.(
              make_java
                ~class_name:(Typ.Name.Java.from_string "ggg.hhh.Iii")
                ~return_type:None ~method_name:Java.class_initializer_method_name ~parameters:[]
                ~kind:Java.Non_Static ()) ] )
    ; ( "label2"
      , Procname.Set.of_list
          [ Procname.(
              make_java
                ~class_name:(Typ.Name.Java.from_string "ddd.eee.Fff")
                ~return_type:(Some StdTyp.void) ~method_name:"methodTwo" ~parameters:[]
                ~kind:Java.Non_Static ())
          ; Procname.(
              make_java
                ~class_name:(Typ.Name.Java.from_string "aaa.bbb.Ccc")
                ~return_type:(Some StdTyp.void) ~method_name:"methodOne" ~parameters:[]
                ~kind:Java.Non_Static ()) ] ) ]
  in
  [ ("test_from_json_string_1", input1, expected1, true)
  ; ("test_from_json_string_2", input2, expected2, true)
  ; ("test_from_json_string_3", input2, expected3, false) ]
  |> List.map ~f:(fun (name, test_input, expected_output, use_signature) ->
         name >:: create_test test_input expected_output ~use_signature )


let test_from_json_string_with_invalid_input =
  let create_test input expected_exception _ =
    let run () = JavaProfilerSamples.from_json_string input ~use_signature:true in
    assert_raises expected_exception run
  in
  [ ( "test_from_json_string_1"
    , "{\"whatever\": {}, \"methods\": []}"
    , Yojson.Json_error
        "Line 1, bytes 0-31:\nExpected '[' but found '{\"whatever\": {}, \"methods\": []}'" )
  ; ( "test_from_json_string_2"
    , Printf.sprintf
        "{\"whatever\": {}, \"methods\": [{\"class\": \"aaa.bbb.Ccc\", \"boo\": \"\", \"method\": \
         \"methodOne\", \"signature\": \"%s\"}], \"foo\": {}}"
        JProcname.JNI.void_method_with_no_arguments
    , Yojson.Json_error
        "Line 1, bytes 0-33:\nExpected '[' but found '{\"whatever\": {}, \"methods\": [{\"cl'" )
  ; ( "test_from_json_string_3"
    , "("
    , Yojson.Json_error "Line 1, bytes 0-1:\nExpected '[' but found '('" ) ]
  |> List.map ~f:(fun (name, test_input, expected_exception) ->
         name >:: create_test test_input expected_exception )


let tests =
  "java_profiler_samples"
  >::: test_jni_parse_str_with_valid_input @ test_jni_parse_str_with_invalid_input
       @ test_jni_parse_method_str_with_invalid_input @ test_jni_pp
       @ test_from_json_string_with_valid_input @ test_from_json_string_with_invalid_input
