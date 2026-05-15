//@ charon-args=--ullbc --print-ullbc
//@ charon-args=--include core::slice::_::binary_search_by

fn main() {
    let a: [u32; 5] = [1, 2, 3, 4, 5];
    let b: &[u32] = &a;

    let res = a.binary_search_by(|x| x.cmp(&3));
}
