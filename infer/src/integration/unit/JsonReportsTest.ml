(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let%test_module "sanitize_qualifier" =
  ( module struct
    let%expect_test "replaces volatile information with a placeholder" =
      let sanitized =
        JsonReports.sanitize_qualifier
          "Error on line 5 column 2. Bad value passed as argument `#42`. Look at function \
           parameter 3. Parameter 7 is also worth looking at at line 5:17"
      in
      F.printf "%s" sanitized ;
      [%expect
        {| Error on $_ $_. Bad value passed as $_. Look at function $_. $_ is also worth looking at at $_$_ |}]
  end )


let%test_module "compute_hash" =
  ( module struct
    let%expect_test "not affected by the argument number change" =
      let proc_name =
        Procname.make_hack ~class_name:None ~function_name:"example_func" ~arity:(Some 2)
      in
      let file = "file.php" in
      let qualifier_after =
        "`example_func()` is tainted by value returned from `Source.get` with kind `source` and \
         flows to value passed as argument `#1` to `Sink.sink` with kind `sink`"
      in
      let qualifier_before =
        "`example_func()` is tainted by value returned from `Source.get` with kind `source` and \
         flows to value passed as argument `#2` to `Sink.sink` with kind `sink`"
      in
      let compute_hash qualifier =
        JsonReports.compute_hash ~severity:"ERROR" ~bug_type:"TAINT_ERROR" ~proc_name ~file
          ~qualifier
      in
      let hash_before = compute_hash qualifier_before in
      F.printf "%s" hash_before ;
      [%expect {| 1c30912ff00238016893f5efdfd6d7f1 |}] ;
      let hash_after = compute_hash qualifier_after in
      F.printf "%b" (String.equal hash_before hash_after) ;
      [%expect {| true |}]
  end )
