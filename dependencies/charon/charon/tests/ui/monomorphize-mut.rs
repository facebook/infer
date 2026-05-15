//@ charon-arg=--monomorphize-mut

// Various levels of nesting.
fn option_mut<X, A>(mut x: A) {
    let _ = Some(&mut 0u32);
    let _ = Some(&mut x);
    let _ = Some(Some(&mut x));
    let _ = Some(&mut &mut 0u32);
}

fn identity<T>(x: T) -> T {
    x
}
fn mutable_identity<T>(x: &mut T) -> &mut T {
    identity(x)
}
fn use_id_mut<X, A>(mut x: A) {
    let _ = identity(&0u32);
    let _ = identity(&mut 0u32);
    let _ = identity(Some(&mut 0u32));
    // Make sure we do generics right.
    let _ = identity(Some(Some(&mut x)));
    // Use a function pointer.
    let _ = Some(&0u32).map(identity);
    // These cause errors because of missing normalization :'(
    // let _ = Some(&mut 0u32).map(identity);
    // let _ = Some(&mut 0u32).map(mutable_identity);
}

// Each instantiation of one requires instantiating the next one.
struct Foo1<T>(T);
struct Foo2<T>(Foo1<T>);
struct Foo3<T>(Foo2<T>);
fn use_foo(_: Foo3<&mut u32>) {}

struct Triple<A, B, C>(A, B, C);
type Example<'a, 'b, T> = Triple<u32, &'a mut &'b mut T, Option<&'a mut bool>>;
fn use_example<'a, 'b, T>(_: Example<'a, 'b, T>) {}

// Mutually recursive types.
enum List<T> {
    Nil,
    Cons(ListNode<T>),
}
struct ListNode<T> {
    val: T,
    next: Box<List<T>>,
}
fn use_list_mut(_: &List<&mut u32>) {}

// Type that already contains mutable references.
struct Iter<'a, T> {
    head: &'a mut T,
    tail: Option<&'a mut T>,
}
fn use_iter<'a, 'b, T>(_: Option<Iter<'a, bool>>) {}
fn use_iter_mut<'a, 'b, T>(_: Iter<'a, &'b mut T>) {}

// Type that already contains mutable references but indirectly.
struct IterWrapper<'a, T>(Iter<'a, T>);
fn use_iter_wrapper<'a, 'b, T>(_: Option<IterWrapper<'a, bool>>) {}

struct ArrayWrapper<const N: usize, T>([T; N]);
fn use_const_generic(_: ArrayWrapper<1, &mut u32>, _: ArrayWrapper<2, &mut u32>) {}
// fn use_const_generic2<const N: usize, const M: usize>(
//     _: ArrayWrapper<N, &mut u32>,
//     _: ArrayWrapper<M, &mut u32>,
// ) {
// }

// // Instantiate method generics
// trait Trait {
//     fn method<T>(&self) {}
// }
// impl Trait for () {}
// fn call_method<X: Trait>(x: &X) {
//     x.method::<&mut bool>();
// }

// Use opaque iter type
fn use_opaque_iter<'a, 'b, T>(x: std::slice::IterMut<'a, bool>) {
    let _ = identity(x);
}
