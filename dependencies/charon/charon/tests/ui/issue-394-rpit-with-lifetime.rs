fn sparse_transitions<'a>() -> impl Iterator<Item = u8> + 'a {
    core::iter::from_fn(|| None)
}
