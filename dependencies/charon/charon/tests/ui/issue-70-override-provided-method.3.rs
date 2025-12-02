//! Ensure we pass generics correctly when dealing with default methods.
//! We implement the trait twice, one with override and the other without. If we did things right,
//! the two cases should end up identical.

trait GenericTrait<T: Clone> {
    fn other_method();
    fn provided<U: PartialEq<T>>(x: T, y: U) {
        if y == x {
            Self::other_method()
        }
    }
}

struct Override<T>(T);
impl<T: Copy> GenericTrait<Option<T>> for Override<T> {
    fn other_method() {}
    fn provided<U: PartialEq<Option<T>>>(x: Option<T>, y: U) {
        if y == x {
            Self::other_method()
        }
    }
}

struct NoOverride<T>(T);
impl<T: Copy> GenericTrait<Option<T>> for NoOverride<T> {
    fn other_method() {}
}
