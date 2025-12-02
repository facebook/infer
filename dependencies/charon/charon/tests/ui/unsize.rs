use std::fmt::Display;
use std::rc::Rc;

fn foo() {
    let array: [_; 2] = [0, 0];
    let _: &[_] = &array;
    let _: Box<[_]> = Box::new(array);
    let _: Rc<[_]> = Rc::new(array);

    let string = String::new();
    let _: &dyn Display = &string;
    let _: Box<dyn Display> = Box::new(string.clone());
    let _: Rc<dyn Display> = Rc::new(string.clone());
}
