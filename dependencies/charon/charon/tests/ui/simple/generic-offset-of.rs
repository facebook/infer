struct A<T> {
    x: B<T>,
}

struct B<T> {
    y: u32,
    t: T,
}

fn foo<T>() {
    let _ = std::mem::offset_of!(A<T>, x.y);
}
