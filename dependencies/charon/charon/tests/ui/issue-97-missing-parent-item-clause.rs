pub trait Ord {}

pub struct AVLTree<T> {
    pub x: T,
}

impl<T: Ord> AVLTree<T> {
    pub fn insert(&mut self) {
        unimplemented!();
    }
}

impl Ord for u32 {}

pub fn test(mut tree: AVLTree<u32>) {
    tree.insert();
}
