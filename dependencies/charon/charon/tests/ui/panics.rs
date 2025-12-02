#![feature(libstd_sys_internals)]
#![feature(rt)]
#![allow(internal_features)]
#![allow(unreachable_code)]
fn panic1() {
    panic!();
}
fn panic2() {
    panic!("O no!");
}
fn panic3() {
    panic!("O {}!", "no");
}
fn panic4() {
    assert!(false);
}
fn panic5() {
    assert!(false, "assert failed");
}
fn panic6() {
    unreachable!();
}
fn panic7() {
    unreachable!("can't reach {}", "this");
}
fn panic8() {
    todo!();
}
fn panic9() {
    ::std::rt::begin_panic("explicit panic");
}
