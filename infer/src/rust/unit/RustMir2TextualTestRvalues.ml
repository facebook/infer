(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open IStd
open RustMir2TextualTest

(* Tests for binop *)
let%expect_test "arithmetic" =
  test "./rvalues/binop/arithmetic.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define arithmetic::arithmetic() : void {
      local var_0: void, a_1: int, b_2: int, var_3: int, var_4: int, var_5: int, var_6: int, var_7: int, var_8: int, var_9: int, var_10: int, var_11: int, var_12: int, var_13: int, var_14: int, x_15: int, y_16: int, var_17: int, var_18: int, var_19: int, var_20: int, var_21: int, var_22: int, var_23: int, var_24: int, var_25: int, var_26: int, var_27: int, var_28: int, c_29: int, d_30: int, var_31: int, var_32: int, var_33: int, var_34: int, var_35: int, var_36: int, var_37: int, var_38: int, var_39: int, var_40: int, var_41: int, var_42: int, z_43: int, w_44: int, var_45: int, var_46: int, var_47: int, var_48: int, var_49: int, var_50: int, var_51: int, var_52: int, var_53: int, var_54: int, var_55: int, var_56: int      
      #node_0:
          store &a_1 <- -7:int
          store &b_2 <- 3:int
          n0:int = load &a_1
          store &var_4 <- n0:int
          n1:int = load &b_2
          store &var_5 <- n1:int
          n2:int = load &var_4
          n3:int = load &var_5
          store &var_6 <- __sil_plusa_int(n2, n3):int
          n4:int = load &var_6
          store &var_3 <- n4:int
          n5:int = load &a_1
          store &var_8 <- n5:int
          n6:int = load &b_2
          store &var_9 <- n6:int
          n7:int = load &var_8
          n8:int = load &var_9
          store &var_10 <- __sil_minusa_int(n7, n8):int
          n9:int = load &var_10
          store &var_7 <- n9:int
          n10:int = load &a_1
          store &var_12 <- n10:int
          n11:int = load &b_2
          store &var_13 <- n11:int
          n12:int = load &var_12
          n13:int = load &var_13
          store &var_14 <- __sil_mult_int(n12, n13):int
          n14:int = load &var_14
          store &var_11 <- n14:int
          store &x_15 <- 2:int
          store &y_16 <- 5:int
          n15:int = load &x_15
          store &var_18 <- n15:int
          n16:int = load &y_16
          store &var_19 <- n16:int
          n17:int = load &var_18
          n18:int = load &var_19
          store &var_20 <- __sil_plusa_int(n17, n18):int
          n19:int = load &var_20
          store &var_17 <- n19:int
          n20:int = load &y_16
          store &var_22 <- n20:int
          n21:int = load &x_15
          store &var_23 <- n21:int
          n22:int = load &var_22
          n23:int = load &var_23
          store &var_24 <- __sil_minusa_int(n22, n23):int
          n24:int = load &var_24
          store &var_21 <- n24:int
          n25:int = load &x_15
          store &var_26 <- n25:int
          n26:int = load &y_16
          store &var_27 <- n26:int
          n27:int = load &var_26 
          n28:int = load &var_27
          store &var_28 <- __sil_mult_int(n27, n28):int
          n29:int = load &var_28
          store &var_25 <- n29:int
          store &c_29 <- 1:int
          store &d_30 <- 2:int
          n30:int = load &c_29
          store &var_32 <- n30:int
          n31:int = load &d_30
          store &var_33 <- n31:int
          n32:int = load &var_32
          n33:int = load &var_33
          store &var_34 <- __sil_plusa_int(n32, n33):int
          n34:int = load &var_34
          store &var_31 <- n34:int
          n35:int = load &c_29
          store &var_36 <- n35:int
          n36:int = load &d_30
          store &var_37 <- n36:int
          n37:int = load &var_36
          n38:int = load &var_37
          store &var_38 <- __sil_minusa_int(n37, n38):int
          n39:int = load &var_38
          store &var_35 <- n39:int
          n40:int = load &c_29
          store &var_40 <- n40:int
          n41:int = load &d_30
          store &var_41 <- n41:int
          n42:int = load &var_40
          n43:int = load &var_41
          store &var_42 <- __sil_mult_int(n42, n43):int
          n44:int = load &var_42
          store &var_39 <- n44:int
          store &z_43 <- 1:int
          store &w_44 <- 2:int
          n45:int = load &z_43
          store &var_46 <- n45:int
          n46:int = load &w_44
          store &var_47 <- n46:int
          n47:int = load &var_46
          n48:int = load &var_47
          store &var_48 <- __sil_plusa_int(n47, n48):int
          n49:int = load &var_48
          store &var_45 <- n49:int
          n50:int = load &z_43
          store &var_50 <- n50:int
          n51:int = load &w_44
          store &var_51 <- n51:int
          n52:int = load &var_50
          n53:int = load &var_51
          store &var_52 <- __sil_minusa_int(n52, n53):int
          n54:int = load &var_52
          store &var_49 <- n54:int
          n55:int = load &z_43
          store &var_54 <- n55:int
          n56:int = load &w_44
          store &var_55 <- n56:int
          n57:int = load &var_54
          n58:int = load &var_55
          store &var_56 <- __sil_mult_int(n57, n58):int
          n59:int = load &var_56
          store &var_53 <- n59:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n60:void = load &var_0
          ret n60

    }

    define arithmetic::main() : void {
      local var_0: void, var_1: void
      #node_0:
          n0 = arithmetic::arithmetic()
          store &var_1 <- n0:void
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

      #node_2:
          unreachable

    }
  |}]


let%expect_test "bitwise" =
  test "./rvalues/binop/bitwise.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define bitwise::bitwise_ops() : void {
      local var_0: void, var_1: int, var_2: int, var_3: int, a_4: int, var_5: int, var_6: int
      #node_0:
          store &var_1 <- __sil_band(10, 12):int
          store &var_2 <- __sil_bor(10, 5):int
          store &var_3 <- __sil_bxor(15, 5):int
          store &a_4 <- -1:int
          n0:int = load &a_4
          store &var_6 <- n0:int
          n1:int = load &var_6
          store &var_5 <- __sil_band(n1, 15):int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n2:void = load &var_0
          ret n2

    }

    define bitwise::main() : void {
      local var_0: void, var_1: void
      #node_0:
          n0 = bitwise::bitwise_ops()
          store &var_1 <- n0:void
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

      #node_2:
          unreachable

    }

  |}]


let%expect_test "comparisons" =
  test "./rvalues/binop/comparisons.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define comparisons::comparisons() : void {
      local var_0: void, a_1: int, b_2: int, var_3: int, var_4: int, var_5: int, var_6: int, var_7: int, var_8: int, var_9: int, var_10: int, var_11: int, var_12: int, var_13: int, var_14: int, var_15: int, var_16: int, var_17: int, var_18: int, var_19: int, var_20: int
      #node_0:
          store &a_1 <- 5:int
          store &b_2 <- 3:int
          n0:int = load &a_1
          store &var_4 <- n0:int
          n1:int = load &b_2
          store &var_5 <- n1:int
          n2:int = load &var_4
          n3:int = load &var_5
          store &var_3 <- __sil_lt(n2, n3):int
          n4:int = load &a_1
          store &var_7 <- n4:int
          n5:int = load &b_2
          store &var_8 <- n5:int
          n6:int = load &var_7
          n7:int = load &var_8
          store &var_6 <- __sil_ge(n6, n7):int
          n8:int = load &a_1
          store &var_10 <- n8:int
          n9:int = load &b_2
          store &var_11 <- n9:int
          n10:int = load &var_10
          n11:int = load &var_11
          store &var_9 <- __sil_gt(n10, n11):int
          n12:int = load &a_1
          store &var_13 <- n12:int
          n13:int = load &b_2
          store &var_14 <- n13:int
          n14:int = load &var_13
          n15:int = load &var_14
          store &var_12 <- __sil_le(n14, n15):int
          n16:int = load &a_1
          store &var_16 <- n16:int
          n17:int = load &b_2
          store &var_17 <- n17:int
          n18:int = load &var_16
          n19:int = load &var_17
          store &var_15 <- __sil_ne(n18, n19):int
          n20:int = load &a_1
          store &var_19 <- n20:int
          n21:int = load &b_2
          store &var_20 <- n21:int
          n22:int = load &var_19
          n23:int = load &var_20
          store &var_18 <- __sil_eq(n22, n23):int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n24:void = load &var_0
          ret n24

    }

    define comparisons::main() : void {
      local var_0: void, var_1: void
      #node_0:
          n0 = comparisons::comparisons()
          store &var_1 <- n0:void
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

      #node_2:
          unreachable

    }

  |}]


let%expect_test "div_modulo" =
  test "./rvalues/binop/div_modulo.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define div_modulo::div_and_modulo() : void {
      local var_0: void, a_1: int, b_2: int, var_3: int, var_4: int, var_5: int, var_6: int, var_7: int, var_8: int, c_9: int, d_10: int, var_11: int, var_12: int, var_13: int, var_14: int, var_15: int, var_16: int, e_17: int, f_18: int, var_19: int, var_20: int, var_21: int, var_22: int, var_23: int, var_24: int
      #node_0:
          store &a_1 <- 7:int
          store &b_2 <- 3:int
          n0:int = load &a_1
          store &var_4 <- n0:int
          n1:int = load &b_2
          store &var_5 <- n1:int
          n2:int = load &var_4
          n3:int = load &var_5
          store &var_3 <- __sil_divi(n2, n3):int
          n4:int = load &a_1
          store &var_7 <- n4:int
          n5:int = load &b_2
          store &var_8 <- n5:int
          n6:int = load &var_7
          n7:int = load &var_8
          store &var_6 <- __sil_mod(n6, n7):int
          store &c_9 <- 7:int
          store &d_10 <- -3:int
          n8:int = load &c_9
          store &var_12 <- n8:int
          n9:int = load &d_10
          store &var_13 <- n9:int
          n10:int = load &var_12
          n11:int = load &var_13
          store &var_11 <- __sil_divi(n10, n11):int
          n12:int = load &c_9
          store &var_15 <- n12:int
          n13:int = load &d_10
          store &var_16 <- n13:int
          n14:int = load &var_15
          n15:int = load &var_16
          store &var_14 <- __sil_mod(n14, n15):int
          store &e_17 <- -7:int
          store &f_18 <- 3:int
          n16:int = load &e_17
          store &var_20 <- n16:int
          n17:int = load &f_18
          store &var_21 <- n17:int
          n18:int = load &var_20
          n19:int = load &var_21
          store &var_19 <- __sil_divi(n18, n19):int
          n20:int = load &e_17
          store &var_23 <- n20:int
          n21:int = load &f_18
          store &var_24 <- n21:int
          n22:int = load &var_23
          n23:int = load &var_24
          store &var_22 <- __sil_mod(n22, n23):int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n24:void = load &var_0
          ret n24

    }

    define div_modulo::main() : void {
      local var_0: void, var_1: void
      #node_0:
          n0 = div_modulo::div_and_modulo()
          store &var_1 <- n0:void
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

      #node_2:
          unreachable

    }

  |}]


let%expect_test "shifts" =
  test "./rvalues/binop/shifts.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"
    
    define shifts::shifts() : void {
      local var_0: void, var_1: int, var_2: int, x_3: int, var_4: int, var_5: int
      #node_0:
          store &var_1 <- __sil_shiftlt(1, 3):int
          store &var_2 <- __sil_shiftrt(128, 7):int
          store &x_3 <- -2:int
          n0:int = load &x_3
          store &var_5 <- n0:int
          n1:int = load &var_5
          store &var_4 <- __sil_shiftrt(n1, 1):int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n2:void = load &var_0
          ret n2

    }

    define shifts::main() : void {
      local var_0: void, var_1: void
      #node_0:
          n0 = shifts::shifts()
          store &var_1 <- n0:void
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

      #node_2:
          unreachable

    }

  |}]


(* Tests for dereferencing *)
let%expect_test "deref0" =
  test "./rvalues/deref/deref0.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define deref0::deref() : void {
      local var_0: void, x_1: int, ref_x_2: *int, y_3: int
      #node_0:
          store &x_1 <- 42:int
          store &ref_x_2 <- &x_1:*int
          n1:*int = load &ref_x_2
          n0:int = load n1
          store &y_3 <- n0:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n2:void = load &var_0
          ret n2

    }

    define deref0::main() : void {
      local var_0: void, var_1: void
      #node_0:
          n0 = deref0::deref()
          store &var_1 <- n0:void
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

      #node_2:
          unreachable

    }
  |}]


(* Tests for mutable raw pointers *)
let%expect_test "mut_raw_ptr0" =
  test "./rvalues/mut_raw_ptr/mut_raw_ptr0.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define mut_raw_ptr0::main() : void {
      local var_0: void, y_1: int, x_2: *int, var_3: *int
      #node_0:
          store &y_1 <- 10:int
          store &var_3 <- &y_1:*int
          store &x_2 <- var_3:*int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }
    
  |}]


(* Tests for mutable references *)
let%expect_test "mut_ref0" =
  test "./rvalues/mut_ref/mut_ref0.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define mut_ref0::main() : void {
      local var_0: void, y_1: int, x_2: *int
      #node_0:
          store &y_1 <- 10:int
          store &x_2 <- &y_1:*int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }

  |}]


(* Tests for raw pointers  *)
let%expect_test "raw_ptr0" =
  test "./rvalues/raw_ptr/raw_ptr0.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define raw_ptr0::main() : void {
      local var_0: void, y_1: int, x_2: *int, var_3: *int
      #node_0:
          store &y_1 <- 10:int
          store &var_3 <- &y_1:*int
          store &x_2 <- var_3:*int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }

  |}]


(* Tests for references *)
let%expect_test "ref0" =
  test "./rvalues/ref/ref0.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define ref0::main() : void {
      local var_0: void, y_1: int, x_2: *int
      #node_0:
          store &y_1 <- 10:int
          store &x_2 <- &y_1:*int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }

  |}]


(* Tests for unary operators *)
let%expect_test "logical_not" =
  test "./rvalues/unop/logical_not.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define logical_not::main() : void {
      local var_0: void, x_1: int, y_2: int, var_3: int
      #node_0:
          store &x_1 <- 1:int
          n0:int = load &x_1
          store &var_3 <- n0:int
          n1:int = load &var_3
          store &y_2 <- __sil_lnot(n1):int
          store &var_0 <- null:void 
          store &var_0 <- null:void
          n2:void = load &var_0
          ret n2

    }
  |}]


let%expect_test "neg_int" =
  test "./rvalues/unop/neg_int.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define neg_int::main() : void {
      local var_0: void, y_1: int, x_2: int, var_3: int
      #node_0:
          store &y_1 <- 1:int
          n0:int = load &y_1
          store &var_3 <- n0:int
          n1:int = load &var_3
          store &x_2 <- __sil_neg(n1):int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n2:void = load &var_0
          ret n2

    }
  |}]
