(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open RustMir2TextualTest

(* Tests for dereferencing *)
let%expect_test "basic_deref" =
  let source = {|
fn main() {
    let x = 42;
    let ref_x = &x;
    let y = *ref_x; 
}

    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int, ref_x_2: *int, y_3: int
      #node_0:
          store &var_0 <- null:void
          store &x_1 <- 42:int
          store &ref_x_2 <- &x_1:*int
          n0:*int = load &ref_x_2
          n1:int = load n0
          store &y_3 <- n1:int
          store &var_0 <- null:void
          n2:void = load &var_0
          ret n2

    }
    |}]


let%expect_test "deref_multiple" =
  let source =
    {|
fn main() {
    let x = 42;
    let ref_1 = &x;           
    let ref_2 = &ref_1;       
    let ref_3 = &ref_2;       
    let y = ***ref_3;
}
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int, ref_1_2: *int, ref_2_3: **int, ref_3_4: ***int, y_5: int, var_6: **int, var_7: *int
      #node_0:
          store &var_0 <- null:void
          store &x_1 <- 42:int
          store &ref_1_2 <- &x_1:*int
          store &ref_2_3 <- &ref_1_2:**int
          store &ref_3_4 <- &ref_2_3:***int
          n0:***int = load &ref_3_4
          n1:**int = load n0
          store &var_6 <- n1:**int
          n2:**int = load &var_6
          n3:*int = load n2
          store &var_7 <- n3:*int
          n4:*int = load &var_7
          n5:int = load n4
          store &y_5 <- n5:int
          store &var_0 <- null:void
          n6:void = load &var_0
          ret n6

    }
    |}]


(* Tests for mutable raw pointers *)
let%expect_test "basic_mut_raw_ptr" =
  let source = {|
fn main() {
    let mut y = 10;
    let x = &mut y as *mut i32;
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, y_1: int, x_2: *int, var_3: *int
      #node_0:
          store &var_0 <- null:void
          store &y_1 <- 10:int
          store &var_3 <- &y_1:*int
          n0:*int = load &var_3
          store &x_2 <- n0:*int
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }
    |}]


(* Tests for mutable references *)
let%expect_test "basic_mut_ref" =
  let source = {|
fn main() {
    let mut y = 10;
    let x = &mut y;
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, y_1: int, x_2: *int
      #node_0:
          store &var_0 <- null:void
          store &y_1 <- 10:int
          store &x_2 <- &y_1:*int
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }
    |}]


(* Tests for raw pointers  *)
let%expect_test "basic_raw_ptr" =
  let source = {|
fn main() {
    let y = 10;
    let x = &y as *const i32;
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, y_1: int, x_2: *int, var_3: *int
      #node_0:
          store &var_0 <- null:void
          store &y_1 <- 10:int
          store &var_3 <- &y_1:*int
          n0:*int = load &var_3
          store &x_2 <- n0:*int
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }
    |}]


(* Tests for references *)
let%expect_test "basic_ref" =
  let source = {|
fn main() {
    let y = 10;
    let x = &y;
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, y_1: int, x_2: *int
      #node_0:
          store &var_0 <- null:void
          store &y_1 <- 10:int
          store &x_2 <- &y_1:*int
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }
    |}]


(* Tests for dereference after reference *)
let%expect_test "basic_ref_deref" =
  let source = {|
fn main() {
    let x = 42;
    
    let y = &*&*&x; 
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int, y_2: *int, var_3: *int, var_4: *int
      #node_0:
          store &var_0 <- null:void
          store &x_1 <- 42:int
          store &var_4 <- &x_1:*int
          n0:*int = load &var_4
          store &var_3 <- n0:*int
          n1:*int = load &var_3
          store &y_2 <- n1:*int
          store &var_0 <- null:void
          n2:void = load &var_0
          ret n2

    }
    |}]


let%expect_test "basic_deref_ref" =
  let source = {|
fn main() {
    let x = 42;
    let ptr = &x;
    let y = &*ptr; 
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int, ptr_2: *int, y_3: *int
      #node_0:
          store &var_0 <- null:void
          store &x_1 <- 42:int
          store &ptr_2 <- &x_1:*int
          n0:*int = load &ptr_2
          store &y_3 <- n0:*int
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }
    |}]


let%expect_test "mutate_through_reference" =
  let source =
    {|
  fn main() {
      let mut x = 10;
      let ptr = &mut x;
      *ptr = 20;
  }
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int, ptr_2: *int
      #node_0:
          store &var_0 <- null:void
          store &x_1 <- 10:int
          store &ptr_2 <- &x_1:*int
          n0:*int = load &ptr_2
          store n0 <- 20:int
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }
    |}]


let%expect_test "null_pointer" =
  let source =
    {|
    fn main() {
        let ptr: *const i32 = std::ptr::null(); 
        let x = unsafe {*ptr};
    }
    |}
  in
  test ~args:"--exclude=core --include=core::ptr::null" source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, ptr_1: *int, x_2: int
      #node_0:
          store &var_0 <- null:void
          n0 = core::ptr::null()
          store &ptr_1 <- n0:*int
          jmp node_2
          .handlers node_1

      #node_1:
          throw "UnwindResume"

      #node_2:
          n1:*int = load &ptr_1
          n2:int = load n1
          store &x_2 <- n2:int
          store &var_0 <- null:void
          n3:void = load &var_0
          ret n3

    }

    define core::ptr::null() : *void {
      local var_0: *void, var_1: *void
      #node_0:
          store &var_1 <- __sil_cast(<*void>, 0):*void
          n0:*void = load &var_1
          store &var_0 <- n0:*void
          n1:*void = load &var_0
          ret n1

    }
    |}]


let%expect_test "box" =
  let source = {|
    fn main() {
        let b = Box::new(42);
    }
    |} in
  test ~args:"--hide-marker-traits --hide-allocator" source ;
  [%expect
    {|
    .source_language = "Rust"

    type alloc::alloc::Global = {}

    define dummy::main() : void {
      local var_0: void, b_1: *int
      #node_0:
          store &var_0 <- null:void
          n0 = __sil_boxnew(42)
          store &b_1 <- n0:*int
          jmp node_2
          .handlers node_1

      #node_1:
          throw "UnwindResume"

      #node_2:
          store &var_0 <- null:void
          n1:*int = load &b_1
          n2 = __sil_free(n1)
          jmp node_3
          .handlers node_4

      #node_3:
          n3:void = load &var_0
          ret n3

      #node_4:
          throw "UnwindResume"

    }
    |}]


let%expect_test "box_memoryleak" =
  let source =
    {|
    fn main() {
        let b = Box::new(42);

        std::mem::forget(b)
    }
    |}
  in
  test ~args:"--hide-marker-traits --hide-allocator --include=core::mem::forget" source ;
  [%expect
    {|
    .source_language = "Rust"

    type alloc::alloc::Global = {}

    define dummy::main() : void {
      local var_0: void, b_1: *int, var_2: *int
      #node_0:
          store &var_0 <- null:void
          n0 = __sil_boxnew(42)
          store &b_1 <- n0:*int
          jmp node_2
          .handlers node_1

      #node_1:
          throw "UnwindResume"

      #node_2:
          n1:*int = load &b_1
          store &var_2 <- n1:*int
          n2:*int = load &var_2
          n3 = core::mem::forget(n2)
          store &var_0 <- n3:void
          jmp node_4
          .handlers node_3

      #node_3:
          throw "UnwindResume"

      #node_4:
          n4:void = load &var_0
          ret n4

    }

    define core::mem::forget(t_1: void) : void {
      local var_0: void
      #node_0:
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }
    |}]


let%expect_test "box_use_after_free" =
  let source =
    {|
    fn main() {
        let ptr: *const i32;
        {
            let x = Box::new(50);
            ptr = &*x; // Convert box to raw pointer
            // Box is freed here since borrowchecker does not keep track of raw pointer
        }
        #[allow(unused)]
        let ub = unsafe {*ptr}; // Error Occurs Here
    }
    |}
  in
  test
    ~args:
      "--hide-marker-traits --hide-allocator --monomorphize --include=core::ptr \
       --start-from=crate::main"
    source ;
  [%expect
    {|
    .source_language = "Rust"

    type alloc::alloc::Global = {}

    type core::marker::PhantomData::<i32> = {}

    type core::ptr::non_null::NonNull::<i32> = {pointer: *int}

    type core::ptr::unique::Unique::<i32> = {pointer: *void; _marker: core::marker::PhantomData::<i32>}

    define dummy::main() : void {
      local var_0: void, ptr_1: *int, x_2: *int, var_3: *int, var_4: *int, ub_5: int, var_6: *int
      #node_0:
          store &var_0 <- null:void
          n0 = __sil_boxnew(50)
          store &x_2 <- n0:*int
          jmp node_2
          .handlers node_1

      #node_1:
          throw "UnwindResume"

      #node_2:
          n1:*void = load &x_2
          store &var_6 <- __sil_cast(<*int>, n1):*int
          n2:*int = load &var_6
          store &var_4 <- n2:*int
          n3:*int = load &var_4
          store &var_3 <- n3:*int
          n4:*int = load &var_3
          store &ptr_1 <- n4:*int
          n5:*int = load &x_2
          n6 = __sil_free(n5)
          jmp node_3
          .handlers node_4

      #node_3:
          n7:*int = load &ptr_1
          n8:int = load n7
          store &ub_5 <- n8:int
          store &var_0 <- null:void
          n9:void = load &var_0
          ret n9

      #node_4:
          throw "UnwindResume"

    }

    declare alloc::alloc::Global::{TraitImpl@1}::drop_in_place(*alloc::alloc::Global) : void

    declare alloc::boxed::Box::{TraitImpl@2}::drop_in_place::<i32, alloc::alloc::Global>(**int) : void
    |}]
