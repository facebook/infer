//@ charon-args=--remove-associated-types=*
// This derive generates code that uses `mem::discriminant`, which involves the builtin associated
// type `<T as DiscriminantKind>::Discriminant`.
#[derive(PartialEq)]
enum Enum {
    Some(u8),
    None,
}
