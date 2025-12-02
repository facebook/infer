/// Testing unop simplification
/// In debug mode, rust introduces an assertion before the negation.
pub fn neg_test(x: i32) -> i32 {
    -x
}

/// Testing binop simplification
/// In debug mode, rust inserts an assertion after the addition
pub fn add_u32(x: u32, y: u32) -> u32 {
    x + y
}

pub fn incr(x: &mut u32) {
    *x += 1;
}

/// Testing binop simplification
/// In debug mode, rust inserts an assertion after the substraction
pub fn subs_u32(x: u32, y: u32) -> u32 {
    x - y
}

/// Testing binop simplification
/// In debug mode, rust inserts an assertion before the division
pub fn div_u32(x: u32, y: u32) -> u32 {
    x / y
}

/// Testing binop simplification
/// When using constants, rustc removes the unnecessary assertions (but
/// only at a specific pass)
pub fn div_u32_const(x: u32) -> u32 {
    x / 2
}

/// Testing binop simplification
pub fn rem_u32(x: u32, y: u32) -> u32 {
    x % y
}

pub fn mul_u32(x: u32, y: u32) -> u32 {
    x * y
}

/// The assertions introduced by rust are not the same for the signed integers
/// and the unsigned integers. For instance, `i32::min / (-1)` can overflow.
pub fn add_i32(x: i32, y: i32) -> i32 {
    x + y
}

pub fn subs_i32(x: i32, y: i32) -> i32 {
    x - y
}

pub fn div_i32(x: i32, y: i32) -> i32 {
    x / y
}

pub fn div_i32_const(x: i32) -> i32 {
    x / 2
}

pub fn rem_i32(x: i32, y: i32) -> i32 {
    x % y
}

pub fn mul_i32(x: i32, y: i32) -> i32 {
    x * y
}

pub fn mix_arith_u32(x: u32, y: u32, z: u32) -> u32 {
    ((x + y) * (x / y) + (x - (z % y))) % (x + y + z)
}

pub fn mix_arith_i32(x: i32, y: i32, z: i32) -> i32 {
    ((x + y) * (x / y) + (x - (z % y))) % (x + y + z)
}

fn shl_u32(x: u32, y: u32) -> u32 {
    x << y
}

fn shr_u32(x: u32, y: u32) -> u32 {
    x >> y
}

fn shr_add_u32(x: u32, y: u32) -> u32 {
    x >> (y + 1)
}

fn shl_i32(x: i32, y: i32) -> i32 {
    x << y
}

fn shr_i32(x: i32, y: i32) -> i32 {
    x >> y
}

/// The test above removes an automatically-added cast of `y as u32`. This makes sure we don't
/// remove a manually-added one.
fn shr_i32_manual_cast(x: i32, y: i32) -> i32 {
    x >> (y as u32)
}

// Checking the simplification of binop operations *inside* global constants.
// Even in release mode, rustc inserts additional checks inside constant bodies.
pub const _: isize = 1 + 1;
pub const _: isize = 1 - 1;
pub const _: isize = -1;
pub const _: isize = 2 * 2;
pub const _: isize = 2 >> 2;
pub const _: isize = 2 << 2;
pub const _: isize = 2 % 2;
pub const _: isize = 2 / 2;

// When the operand is a bare const it sometimes gets inlined, which trips up our simplification
// pass. I used `u32` here because other types use a `cast` and thus aren't inlined.
pub const FOO: u32 = 10;
pub const _: u32 = 1 + FOO;
pub const _: u32 = 10 - FOO;
pub const _: u32 = 2 * FOO;
pub const _: u32 = 2 >> FOO;
pub const _: u32 = 2 << FOO;
pub const _: u32 = 2 % FOO;
pub const _: u32 = 2 / FOO;

fn div_signed_with_constant() -> i32 {
    const FOO: i32 = 42;
    FOO / 2
}

// See issue #87
fn div_unsigned_to_slice(result: &mut [u32], x: u32) {
    result[0] = x / 3329;
}

fn div_signed_to_slice(result: &mut [i32], x: i32) {
    result[0] = x / 3329;
}

fn add_to_slice(result: &mut [u32], x: u32) {
    result[0] = x + 1;
}

fn add_to_slice2(result: &mut [u8], i: usize, x: u8) {
    result[i + 1] = x * 7;
}
