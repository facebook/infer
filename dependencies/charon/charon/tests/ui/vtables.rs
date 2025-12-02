#![feature(trait_alias)]
trait Super<T> {
    fn super_method(&self, arg: T) -> i32;
}
trait Checkable<T>: Super<T> {
    fn check(&self) -> bool;
}
impl Super<i32> for i32 {
    fn super_method(&self, arg: i32) -> i32 {
        *self + arg
    }
}
impl Checkable<i32> for i32 {
    fn check(&self) -> bool {
        self.super_method(10) > 0
    }
}

trait NoParam {
    fn dummy(&self);
}
impl NoParam for i32 {
    fn dummy(&self) {
        assert!(*self > 0);
    }
}
fn to_dyn_obj<T: NoParam>(arg: &T) -> &dyn NoParam {
    arg
}

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

trait BaseOn<T> {
    fn operate_on(&self, t: &T);
}
trait Both32And64: BaseOn<i32> + BaseOn<i64> {
    fn both_operate(&self, t32: &i32, t64: &i64) {
        self.operate_on(t32);
        self.operate_on(t64);
    }
}
impl BaseOn<i32> for i32 {
    fn operate_on(&self, t: &i32) {
        assert!(*self > *t);
    }
}
impl BaseOn<i64> for i32 {
    fn operate_on(&self, t: &i64) {
        assert!(*self as i64 > *t);
    }
}
impl Both32And64 for i32 {}
trait Alias = Both32And64;

fn use_alias(x: &dyn Alias) {
    x.both_operate(&100, &200);
}

fn main() {
    let x: &dyn Checkable<i32> = &42;
    assert!(x.check());
    let y: &mut dyn Modifiable<i32> = &mut 99;
    assert!(!modify_trait_object(&"Hello".to_string()).is_empty());
    assert_eq!(y.modify(&mut 100), 100);
    let z: &dyn NoParam = to_dyn_obj(&42);
    z.dummy();
    let a: &dyn Both32And64 = &42;
    a.both_operate(&100, &200);
}
