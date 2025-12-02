//@ charon-args=--include core::slice::index::_

fn main() {
    let array = [1, 2, 3, 4, 5, 6];
    let slice = &array[2..5];
    assert!(slice[0] == 3);
}
