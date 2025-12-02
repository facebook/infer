//@ charon-args=--start-from=crate::module1
//@ charon-args=--start-from=test_crate::module2
//@ charon-args=--start-from=std::iter::once
//@ charon-args=--start-from=std::clone::Clone::clone_from
//@ charon-args=--start-from=std::slice::Iter::as_slice
//@ charon-args=--start-from=u32::wrapping_add

fn dont_translate() {}

mod module1 {
    fn do_translate() {}
}
mod module2 {
    fn do_translate() {}
}
mod module3 {
    fn dont_translate() {}
}
