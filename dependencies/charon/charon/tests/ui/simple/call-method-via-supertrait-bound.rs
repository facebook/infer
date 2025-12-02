trait OtherTrait {}
trait ImpliesOtherTrait: OtherTrait {}

trait HasMethod {
    fn method(&self);
}
impl<T: OtherTrait> HasMethod for T {
    fn method(&self) {}
}

fn call_method<T: ImpliesOtherTrait>(x: T) {
    let _ = x.method();
}
