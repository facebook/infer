// This tests that we correctly support type signatures with predicates involving late bound
// regions. Here, `Option<T>` requires `T: Sized` which is the predicate in question.

fn wrap<'a>(x: &'a u32) -> Option<&'a u32> {
    Some(x)
}

// The `Clone` bound makes `'a` early-bound.
fn wrap2<'a>(x: &'a u32) -> Option<&'a u32>
where
    &'a (): Clone,
{
    Some(x)
}

// Test with a signature from a foreign crate.
fn foo() {
    use std::cell::RefCell;
    let ref_b = RefCell::new(false);
    // `try_borrow` has a type that includes predicates on late bound regions.
    let _ = ref_b.try_borrow();
}

trait Foo {
    type S;
}
// Test normalization order just in case.
fn f<T: Foo<S = U::S>, U: Foo>() -> Option<T::S> {
    todo!()
}
