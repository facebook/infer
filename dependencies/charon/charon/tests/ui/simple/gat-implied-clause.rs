trait Trait {
    type LifetimeGat<'a>: Clone;
    type NonLifetimeGat<T>: Clone;
}

fn lifetime_gat_bound<X: Trait>(x: X::LifetimeGat<'_>) {
    let _ = x.clone();
}

fn non_lifetime_gat_bound<X: Trait>(x: X::NonLifetimeGat<u8>) {
    let _ = x.clone();
}
