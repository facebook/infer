//@ charon-args=--monomorphize
//@ charon-args=--start-from=crate::main
//@ charon-args=--start-from=crate::FooInt
//@ charon-args=--start-from=crate::FooBool
// Ensures monomorphization handles globals with generics

struct Foo<T> {
    value: T,
}

static FooInt: Foo<i32> = Foo { value: 0i32 };
static FooBool: Foo<bool> = Foo { value: false };

fn main() {
    let _b = FooBool.value;
}
