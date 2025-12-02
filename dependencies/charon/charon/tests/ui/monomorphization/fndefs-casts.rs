//@ charon-args=--monomorphize
//@ charon-args=--start-from=crate::main
// Ensures casts of FnDefs monomorphise properly

fn foo<'a, T>(x: &'a T) {}

fn takes_closure(c: impl for<'a> Fn(&'a u32)) {
    c(&13)
}

fn main() {
    let fooint1: fn(&u8) = foo;
    let fooint2: fn(&u8) = foo;
    let foochar: fn(&char) = foo;

    let a = 11;
    fooint1(&a);
    let b = 12;
    fooint1(&a);
    fooint1(&b);
    fooint2(&b);

    foochar(&'x');

    takes_closure(foo);
}
