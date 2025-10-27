(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open TextualTestHelpers

let%expect_test "" =
  let source =
    {|
        .source_language = "c"
        define main(var1: int, var2: int) : int {
          local temp: int
          #b0:
              n4 = load &var2
              if __sil_cast(<int>, n4) then jmp b1 else jmp b3
          #b1:
              n7 = 0
              jmp b2
           #b2:
               ret n7
           #b3:
               n6 = load &var1
               n5 = n6
               n4 = 10
               jmp b4
           #b4:
               store &temp <- n5
               n5 = 2
               jmp b5
           #b5:
               n7 = n5
               jmp b2

        } |}
  in
  test_restore_ssa source ;
  [%expect
    {|
    .source_language = "c"

    define main(var1: int, var2: int) : int {
      local __TEMP7: int, __TEMP5: int, __TEMP4: int, temp: int
      #b0:
          store &__TEMP4 <- [&var2:int]:int
          if __sil_cast(<int>, [&__TEMP4:int]) then jmp b1 else jmp b3

      #b1:
          store &__TEMP7 <- 0:int
          jmp b2

      #b2:
          ret [&__TEMP7:int]

      #b3:
          n6:int = load &var1
          store &__TEMP5 <- n6:int
          store &__TEMP4 <- 10:int
          jmp b4

      #b4:
          store &temp <- [&__TEMP5:int]:int
          store &__TEMP5 <- 2:int
          jmp b5

      #b5:
          store &__TEMP7 <- [&__TEMP5:int]:int
          jmp b2

    }


    AFTER COMPLETE TRANSFORMATION:
    .source_language = "c"

    define main(var1: int, var2: int) : int {
      local __TEMP7: int, __TEMP5: int, __TEMP4: int, temp: int
      #b0:
          n7:int = load &var2
          store &__TEMP4 <- n7:int
          n8:int = load &__TEMP4
          jmp b1, b3

      #b1:
          prune __sil_cast(<int>, n8)
          store &__TEMP7 <- 0:int
          jmp b2

      #b2:
          n9:int = load &__TEMP7
          ret n9

      #b3:
          prune __sil_lnot(__sil_cast(<int>, n8))
          n6:int = load &var1
          store &__TEMP5 <- n6:int
          store &__TEMP4 <- 10:int
          jmp b4

      #b4:
          n10:int = load &__TEMP5
          store &temp <- n10:int
          store &__TEMP5 <- 2:int
          jmp b5

      #b5:
          n11:int = load &__TEMP5
          store &__TEMP7 <- n11:int
          jmp b2

    }
    |}]
