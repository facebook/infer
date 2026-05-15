//@ known-failure
enum List<T> {
    Nil,
    Cons(T, Box<List<T>>),
}

// This is the code emitted by "perfect derive" macros. The second where bound forces any proof of
// `List<T>: Clone` to be coinductive.
impl<T: Clone> Clone for List<T>
where
    T: Clone,
    Box<List<T>>: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Nil => Self::Nil,
            Self::Cons(x, q) => Self::Cons(x.clone(), q.clone()),
        }
    }
}

fn main() {
    let list: List<u32> = List::Nil;
    let _ = list.clone();
}
