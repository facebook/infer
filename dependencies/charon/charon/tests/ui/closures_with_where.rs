trait Ops<const K: usize> {
    fn of_usize(x: usize) -> Self;
}

fn test<const K: usize, T: Ops<K>>() -> [T; 1] {
    core::array::from_fn(|i| T::of_usize(i))
}
