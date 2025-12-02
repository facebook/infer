//@ charon-args=--remove-associated-types=*
//! Exercise the translation of arrays, with features supported by Eurydice

pub enum AB {
    A,
    B,
}

pub fn incr(x: &mut u32) {
    *x += 1;
}

// Nano-tests
// ----------

// The suffix `_` prevents name collisions in some backends
pub fn array_to_shared_slice_<T>(s: &[T; 32]) -> &[T] {
    s
}

// The suffix `_` prevents name collisions in some backends
pub fn array_to_mut_slice_<T>(s: &mut [T; 32]) -> &mut [T] {
    s
}

pub fn array_len<T>(s: [T; 32]) -> usize {
    s.len()
}

pub fn shared_array_len<T>(s: &[T; 32]) -> usize {
    s.len()
}

pub fn shared_slice_len<T>(s: &[T]) -> usize {
    s.len()
}

pub fn index_array_shared<T>(s: &[T; 32], i: usize) -> &T {
    &s[i]
}

// Remark: can't move out of an array
// Also: can't move out of a slice.

pub fn index_array_u32(s: [u32; 32], i: usize) -> u32 {
    s[i]
}

pub fn index_array_copy(x: &[u32; 32]) -> u32 {
    x[0]
}

pub fn index_mut_array<T>(s: &mut [T; 32], i: usize) -> &mut T {
    &mut s[i]
}

pub fn index_slice<T>(s: &[T], i: usize) -> &T {
    &s[i]
}

pub fn index_mut_slice<T>(s: &mut [T], i: usize) -> &mut T {
    &mut s[i]
}

pub fn slice_subslice_shared_(x: &[u32], y: usize, z: usize) -> &[u32] {
    &x[y..z]
}

pub fn slice_subslice_mut_(x: &mut [u32], y: usize, z: usize) -> &mut [u32] {
    &mut x[y..z]
}

pub fn array_to_slice_shared_(x: &[u32; 32]) -> &[u32] {
    x
}

pub fn array_to_slice_mut_(x: &mut [u32; 32]) -> &mut [u32] {
    x
}

pub fn array_subslice_shared_(x: &[u32; 32], y: usize, z: usize) -> &[u32] {
    &x[y..z]
}

pub fn array_subslice_mut_(x: &mut [u32; 32], y: usize, z: usize) -> &mut [u32] {
    &mut x[y..z]
}

pub fn index_slice_0<T>(s: &[T]) -> &T {
    &s[0]
}

pub fn index_array_0<T>(s: &[T; 32]) -> &T {
    &s[0]
}

/*
// Unsupported by Aeneas for now
pub fn index_index_slice<'a, T>(s: &'a [&[T]], i: usize, j: usize) -> &'a T {
    &s[i][j]
}
*/

pub fn index_index_array(s: [[u32; 32]; 32], i: usize, j: usize) -> u32 {
    s[i][j]
}

/*
// Unsupported by Aeneas for now
pub fn update_update_slice(s: &mut [&mut [u32]], i: usize, j: usize) {
    s[i][j] = 0;
}
*/

pub fn update_update_array(mut s: [[u32; 32]; 32], i: usize, j: usize) {
    s[i][j] = 0;
}

pub fn incr_array_self(s: &mut [u32; 2]) {
    s[0] += 1;
}

pub fn incr_slice_self(s: &mut [u32]) {
    s[0] += 1;
}

pub fn array_local_deep_copy(x: &[u32; 32]) {
    let _y = *x;
}

pub fn take_array(_: [u32; 2]) {}
pub fn take_array_borrow(_: &[u32; 2]) {}
pub fn take_slice(_: &[u32]) {}
pub fn take_mut_slice(_: &mut [u32]) {}

pub fn const_array() -> [u32; 2] {
    [0, 0]
}

pub fn const_slice() {
    let _: &[u32] = &[0, 0];
}

/*
// This triggers a special case in the constant expressions
pub fn const_string() {
    let _ = "hello";
}*/

pub fn take_all() {
    let mut x: [u32; 2] = [0, 0];
    // x is deep copied (copy node appears in Charon, followed by a move)
    take_array(x);
    take_array(x);
    // x passed by address, there is a reborrow here
    take_array_borrow(&x);
    // automatic cast from array to slice (spanning entire array)
    take_slice(&x);
    // note that both appear as SliceNew expressions, meaning the SliceNew UnOp is overloaded for
    // mut and non-mut cases
    take_mut_slice(&mut x);
}

pub fn index_array(x: [u32; 2]) -> u32 {
    x[0]
}
pub fn index_array_borrow(x: &[u32; 2]) -> u32 {
    x[0]
}

pub fn index_slice_u32_0(x: &[u32]) -> u32 {
    x[0]
}

pub fn index_mut_slice_u32_0(x: &mut [u32]) -> u32 {
    x[0]
}

pub fn index_all() -> u32 {
    let mut x: [u32; 2] = [0, 0];
    if true {
        let _y: [u32; 2] = [0, 0];
    } else {
        let _z: [u32; 1] = [0];
    }
    index_array(x)
        + index_array(x)
        + index_array_borrow(&x)
        + index_slice_u32_0(&x)
        + index_mut_slice_u32_0(&mut x)
}

pub fn update_array(mut x: [u32; 2]) {
    x[0] = 1
}
pub fn update_array_mut_borrow(x: &mut [u32; 2]) {
    x[0] = 1
}
pub fn update_mut_slice(x: &mut [u32]) {
    x[0] = 1
}

pub fn update_all() {
    let mut x: [u32; 2] = [0, 0];
    update_array(x);
    update_array(x);
    update_array_mut_borrow(&mut x);
    update_mut_slice(&mut x);
}

// Nano-tests, with ranges
// -----------------------

pub fn range_all() {
    let mut x: [u32; 4] = [0, 0, 0, 0];
    // CONFIRM: there is no way to shrink [T;N] into [T;M] with M<N?
    update_mut_slice(&mut x[1..3]);
}

// Nano-tests, with dereferences
// -----------------------------

pub fn deref_array_borrow(x: &[u32; 2]) -> u32 {
    let x: [u32; 2] = *x;
    x[0]
}

pub fn deref_array_mut_borrow(x: &mut [u32; 2]) -> u32 {
    let x: [u32; 2] = *x;
    x[0]
}

// Non-copiable arrays
// -------------------

pub fn take_array_t(_: [AB; 2]) {}

pub fn non_copyable_array() {
    let x: [AB; 2] = [AB::A, AB::B];
    // x is moved (not deep copied!)
    // TODO: determine whether the translation needs to be aware of that and pass by ref instead of by copy
    take_array_t(x);

    // this fails, naturally:
    // take_array_t(x);
}

// Larger, random tests
// --------------------

pub fn sum(s: &[u32]) -> u32 {
    let mut sum = 0;
    let mut i = 0;
    while i < s.len() {
        sum += s[i];
        i += 1;
    }
    sum
}

pub fn sum2(s: &[u32], s2: &[u32]) -> u32 {
    let mut sum = 0;
    assert!(s.len() == s2.len());
    let mut i = 0;
    while i < s.len() {
        sum += s[i] + s2[i];
        i += 1;
    }
    sum
}

pub fn f0() {
    let s: &mut [u32] = &mut [1, 2];
    s[0] = 1;
}

pub fn f1() {
    let mut s: [u32; 2] = [1, 2];
    s[0] = 1;
}

pub fn f2(_: u32) {}

pub fn f3() -> u32 {
    let a: [u32; 2] = [1, 2];
    f2(a[0]);
    let b = [0; 32];
    sum2(&a, f4(&b, 16, 18))
}

pub fn f4(x: &[u32; 32], y: usize, z: usize) -> &[u32] {
    &x[y..z]
}

pub const SZ: usize = 32;

// There is something slightly annoying here: the SZ constant gets inlined
pub fn f5(x: &[u32; SZ]) -> u32 {
    x[0]
}

// To avoid lifetime shortening
pub fn ite() {
    let mut x: [u32; 2] = [0, 0];
    if true {
        let mut y: [u32; 2] = [0, 0];
        index_mut_slice_u32_0(&mut x);
        index_mut_slice_u32_0(&mut y);
    }
}

pub fn zero_slice(a: &mut [u8]) {
    let mut i: usize = 0;
    let len = a.len();
    while i < len {
        a[i] = 0;
        i += 1;
    }
}

pub fn iter_mut_slice(a: &mut [u8]) {
    let len = a.len();
    let mut i = 0;
    while i < len {
        i += 1;
    }
}

pub fn sum_mut_slice(a: &mut [u32]) -> u32 {
    let mut i = 0;
    let mut s = 0;
    while i < a.len() {
        s += a[i];
        i += 1;
    }
    s
}

// From issue #203
fn slice_pattern_1(x: [(); 1]) {
    let [_named] = x;
}

fn slice_pattern_2<T>(x: [&mut T; 3]) {
    let [_a, _b, _c] = x;
}

fn slice_pattern_3(x: &[(); 1]) {
    let [_named] = x;
}

fn slice_pattern_4(x: &[()]) {
    match x {
        [_named] => (),
        _ => (),
    }
}
