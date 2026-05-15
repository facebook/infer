/// This tests `dyn` with a diamond hierarchy between supertraits.

trait Super<T> {
    fn super_method(&self, arg: T) -> i32;
}
trait Internal {
    type Internal;
    fn internal_method(&self) -> Self::Internal;
}
trait Left: Internal {
    type Left;
    fn left_method(&self) -> Self::Left;
}
trait Right<T>: Internal + Super<T> {
    type Right;
    fn right_method(&self) -> Self::Right;
}
trait Join<T>: Left + Right<T> {
    fn join_method(&self) -> (Self::Left, Self::Right);
}

impl Super<i32> for i32 {
    fn super_method(&self, arg: i32) -> i32 {
        self + arg
    }
}
impl Internal for i32 {
    type Internal = i32;
    fn internal_method(&self) -> Self::Internal {
        *self + 1
    }
}
impl Left for i32 {
    type Left = i32;
    fn left_method(&self) -> Self::Left {
        *self + 2
    }
}
impl Right<i32> for i32 {
    type Right = i32;
    fn right_method(&self) -> Self::Right {
        *self + 3 + self.internal_method() + self.super_method(10)
    }
}
impl Join<i32> for i32 {
    fn join_method(&self) -> (Self::Left, Self::Right) {
        (self.left_method(), self.right_method())
    }
}

fn main() {
    let v: &dyn Join<i32, Internal = i32, Left = i32, Right = i32> = &97;
    let (_, _) = v.join_method();
}
