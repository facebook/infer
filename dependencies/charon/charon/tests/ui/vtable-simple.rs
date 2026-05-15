trait Modifiable<T> {
    fn modify(&mut self, arg: &T) -> T;
}
impl<T: Clone> Modifiable<T> for i32 {
    fn modify(&mut self, arg: &T) -> T {
        *self += 1;
        arg.clone()
    }
}
fn modify_trait_object<T: Clone>(arg: &T) -> T {
    let x: &mut dyn Modifiable<T> = &mut 199;
    x.modify(arg)
}

fn main() {
    modify_trait_object(&42);
}