//@ charon-args=--monomorphize
//@ charon-args=--start-from=crate::main
use std::marker::PhantomData;
use std::ptr::NonNull;

pub struct LinkedList<T> {
    head: Option<NonNull<Node<T>>>,
}

struct Node<T> {
    next: Option<NonNull<Node<T>>>,
    element: T,
}

impl<T> LinkedList<T> {
    fn new() -> Self {
        Self { head: None }
    }
}

fn main() {
    let list: LinkedList<u8> = LinkedList::new();
}
