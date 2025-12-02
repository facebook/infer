pub trait Trait {
    fn method(&self);
}

impl<T> Trait for T {
    fn method(&self) {}
}
