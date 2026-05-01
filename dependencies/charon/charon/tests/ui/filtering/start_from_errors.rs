//@ known-failure
//@ charon-arg=--start-from=start_from_errors::module1
//@ charon-arg=--start-from=module2
//@ charon-arg=--start-from=crate::module3
//@ charon-arg=--start-from=std::iter:once
//@ charon-arg=--start-from=std::ops::Deref::Target
//@ charon-arg=--start-from={impl crate::Type}
//@ charon-arg=--start-from={impl crate::Trait for &crate::Type}
//@ charon-arg=--start-from={impl crate::Trait for crate::MissingType}
//@ charon-arg=--start-from={impl crate::Trait<_> for _}
//@ charon-arg=--start-from={impl crate::Trait for crate::Type}::missing_method
//@ charon-arg=--start-from=crate::{impl crate::Trait for crate::Type}

mod module1 {}
mod module2 {}
mod module3 {}

struct Type;
impl Type {
    fn inherent_method() {}
}

trait Trait<T> {
    fn method();
}
impl Trait<()> for Type {
    fn method() {}
}
