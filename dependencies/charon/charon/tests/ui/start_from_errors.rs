//@ known-failure
//@ charon-args=--start-from=start_from_errors::module1
//@ charon-args=--start-from=module2
//@ charon-args=--start-from=std::iter:once
//@ charon-args=--start-from=std::ops::Deref::Target

mod module1 {}
mod module2 {}
