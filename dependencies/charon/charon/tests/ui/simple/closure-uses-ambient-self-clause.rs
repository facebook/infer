trait Thing {
    type Item;
    fn foo(i: Self::Item) {
        (|_: Self::Item| ())(i)
    }
}
