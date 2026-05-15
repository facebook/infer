//@ charon-args=--mir promoted
// If we're not careful, the promoted const ends up treated like a trait associated item.
trait Thing {
    fn foo() {
        let _ = &42;
    }
}
