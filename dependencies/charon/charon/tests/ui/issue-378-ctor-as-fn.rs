static F: fn(u8) -> Option<u8> = Some;

struct Foo(u32, String);

enum Bar<'a, T> {
    Variant(&'a T),
}

fn main() {
    let f = Some;
    let _ = f(42);
    let f: fn(u8) -> _ = f;
    let _ = f(42);
    let f = Foo;
    let _ = f(42, String::new());
    let f = Bar::Variant;
    let _ = f(&42);
}
