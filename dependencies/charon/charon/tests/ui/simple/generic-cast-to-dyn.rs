use std::any::Any;

fn foo<T: Any>(x: &T) -> &dyn Any {
    x
}
