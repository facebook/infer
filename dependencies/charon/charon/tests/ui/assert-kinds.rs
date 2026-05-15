//@ no-default-options
//@ charon-args=--ullbc --print-ullbc
// see "assert-kinds-reconstruct-fallible" for the version with --reconstruct-fallible-operations

fn main() {
    let _ = [1, 2, 3][0];
    let _ = 5 * 10;
    let x = (127i8);
    let _ = -x;
    let _ = 1 / 0;
    let _ = 1 % 0;
}
